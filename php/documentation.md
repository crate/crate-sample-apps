# Getting Started with Crate PDO

## Installation

The PHP PDO client library for Crate.IO is available on [Packagist](https://packagist.org/packages/crate/crate-pdo) and can be installed by using [Composer](https://getcomposer.org/) or manually with adding a dependency to `composer.json`.

**Directly**

```console
php composer.phar require crate/crate-pdo:~0.3.0
```

**Manually**  
Adding following dependency to `composer.json`.

```json
{
  "require": {
    "crate/crate-pdo":"~0.3.0"
  }
}
```

Install the driver by typing:

```console
php composer.phar install
```

Composer installs the library and its dependencies into `./vendor/`.

## Usage

After installing the driver it can be loaded automatically by adding this line on top of the php document.

```php
require 'vendor/autoload.php'
```

### Connecting to Crate Server

To connect to your cluster, Crate follows standard PDO syntax to form a data source name string ([dsn](https://en.wikipedia.org/wiki/Data_source_name)) and then connect to it.

```php
require_once 'vendor/autoload.php';

$dsn = 'crate:SERVER_IP:4200';
$conn = new Crate\PDO\PDO($dsn, null, null, null);
```

As Crate doesn't support authentication, the other parameters can be left null.

In our PHP example application we read the DSN connection information from the attached config file `app.ini`. The PDO connection is set in the constructor of the  `CrateResource` instance which serves the [Slim](http://www.slimframework.com/) web application.

```php
class CrateResource extends \Slim\Slim
{
  public $conn;
  public $config;

  function __construct($config)
  {
    parent::__construct();
    $this->conn = new Crate\PDO\PDO("
                {$config['db_host']}:{$config['db_port']}",
                null, null, null);
  }
```

### Executing Statements

#### Executing single statements

SQL statements can be executed using the `query()` method on the `PDOStatement` object that gets returned when the query gets prepared. Each `query()` call results in a new HTTP request to the Crate server. The response of the request (such as `rowCount()`, `columnCount()`, etc.) is writing directly to the `PDOStatement` object.

```php
$query = $conn->query("SELECT * FROM guestbook.posts");
echo "table guestbook.posts counts " . $query->rowCount() . " row(s)";
```

To perform parameter substitution the method `bindParam()` ensures that the parameters are bind correctly to its assigned values. Afterwards the query can be executed using the `execute()` method on the variable that holds the prepared statement object.

```php
$qry = $app->conn->prepare("
          INSERT INTO guestbook.posts (
            id, user, text, created, image_ref, like_count)
          VALUES(?, ?, ?, ?, ?, ?)");
$qry->bindParam(1, $id);
$qry->bindParam(2, $user);
$qry->bindParam(3, $text);
$qry->bindParam(4, time());
$qry->bindParam(5, $image_ref);
$qry->bindParam(6, $likeCount);
$state = $qry->execute();
```

For a prepared statement using named placeholders, the first argument is the parameter name of the form `:name`. For a prepared statement using question mark placeholders, this will be the **1-indexed** position of the parameter

#### Fetching query results

To fetch data from row returning statements (DQL) there are multiple methods:
  * `fetch()`: fetches the next row from a result set (see [PDOStatement::fetch](http://php.net/manual/de/pdostatement.fetch.php))
  * `fetchall()`: returns an array containing all of the result set rows (see [PDOStatement::fetchall](http://php.net/manual/de/pdostatement.fetchall.php))

```php
$qry = $conn->query("SELECT COUNT(*) FROM sys.nodes");
$row = $qry->fetch();
echo "Crate cluster has " . $row[0] . " nodes";
```

Our example application uses the data obtained from `fetchall()` and forwards the output directly to the response body. The fetch style argument `PDO::FETCH_ASSOC` ensures that the array is indexed by column name.

```php
$result = $qry->fetchAll(PDO::FETCH_ASSOC);
$app->success(200, $result);
```

#### Handling BLOBs

Crate PDO does not contain an implementation or API for handling BLOBs in Crate.IO. Therefore it is necessary to use the library [libcurl](http://php.net/manual/de/intro.curl.php) that is already supported by PHP since version 4.0.2. It is used to upload binaries via HTTP PUT onto a server or host.

The sequence of uploading a binary file to a BLOB table on the Crate server is:
  1. Read BLOB content
  2. Compute SHA-1 digest out of BLOB content
  3. Initialize a cURL session using `curl_init()`
  4. Set all necessary options via `curl_setopt()` to use a PUT-Request for transfer method
  5. Execute the session with `cur_exec()`
  6. Finish off the session using `curl_close()`

Following code listing shows how an upload via libcurl could look like. The POST endpoint demonstrates how to generate a `sha1` digest from a binary file content and how to use it to create the BLOB on the Crate server.

```php
$app->post('/images', function() use ($app)
{
    $data = json_decode($app->request->getBody());
    if (!isset($data->blob)) {
        $app->error(400, "blob is required");
        return;
    }
    $content = base64_decode($data->blob);
    $digest  = sha1($content);
    $ch      = curl_init("{$app->config['blob_url']}guestbook_images/{$digest}");
    curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "PUT");
    curl_setopt($ch, CURLOPT_POSTFIELDS, $content);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
    $result = curl_exec($ch);
    $info   = curl_getinfo($ch);
    if ($info['http_code'] != "201") {
        $app->error($info['http_code'], curl_error($ch));
    } else {
        $app->success($info['http_code'], array(
            'url' => "{$app->config['blob_url']}guestbook_images/{$digest}",
            'digest' => $digest
        ));
    }
})->name('image-post');
```

With the BLOB API on Crate it is possible to handle BLOBs within different HTTP request methods on the endpoint `{HOST}/_blobs/{digest}`:
  * `PUT` to create/upload blobs
  * `DELETE` to remove a blob from the database

For a detailed documentation of handling BLOB tables see [BLOB Support](https://crate.io/docs/reference/blob.html).

#### Closing the connection

The connection to Crate.IO remains active for the lifetime of the Crate PDO object. To close the connection the variable that holds the PDO object needs to be assigned NULL. It destroys the object by ensuring that all remaining references to it are deleted.