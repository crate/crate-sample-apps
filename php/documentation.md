# Crate.IO PDO Sample App Explanation
## Installation
The PHP PDO client library for Crate.IO is available on [Packagist](https://packagist.org/packages/crate/crate-pdo) and can be installed by using [Composer](https://getcomposer.org/) or manually by adding a dependency to _composer.json_.

### Directly
Install with Composer:

```bash
composer require crate/crate-pdo:~0.3.0
```

### Manually
Add the following dependency to _composer.json_:

```json
{
  "require": {
    "crate/crate-pdo":"~0.3.0"
  }
}
```

And install the driver with:

```bash
composer install
```

Composer will install the library and its dependencies into _./vendor/_.

## Usage
After installing the driver, load it automatically by adding this line to the top of the php file.

```php
require 'vendor/autoload.php'
```

### Connecting to a Crate Cluster
To connect to a cluster, Crate follows standard PDO syntax to form a data source name string ([dsn](https://en.wikipedia.org/wiki/Data_source_name)) and connect to it.

```php
require_once 'vendor/autoload.php';

$dsn = 'crate:SERVER_IP:4200';
$conn = new Crate\PDO\PDO($dsn, null, null, null);
```

As Crate doesn't support authentication, the other parameters can be left null.

In the PHP example application we read the DSN connection information from the _app.ini_ config file. The PDO connection is set in the constructor of the  `CrateResource` instance which serves the [Slim](http://www.slimframework.com/) web application.

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
#### Executing Single Statements
SQL statements can be executed using the `query()` method on the `PDOStatement` object that gets returned when the query is prepared. Each `query()` call results in a new HTTP request to the Crate server. The response of the request (such as `rowCount()`, `columnCount()`, etc.) is written directly to the `PDOStatement` object.

```php
$query = $conn->query("SELECT * FROM guestbook.posts");
echo "table guestbook.posts counts " . $query->rowCount() . " row(s)";
```

To perform parameter substitution the method `bindParam()` ensures that the parameters bind correctly to their assigned values. Afterwards execute the query using the `execute()` method on the variable that holds the prepared statement object.

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

For a prepared statement using named placeholders, the first argument is the parameter name in the form `:name`. For a prepared statement using question mark placeholders, this will be the **1-indexed** position of the parameter

#### Fetching Query Results
There are multiple ways to fetch data from row returning statements (DQL):

- `fetch()`: Fetches the next row from a result set (see [PDOStatement::fetch](http://php.net/manual/de/pdostatement.fetch.php))
- `fetchall()`: Returns an array containing all the result set rows (see [PDOStatement::fetchall](http://php.net/manual/de/pdostatement.fetchall.php))

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
The Crate PDO does not have an implementation or API for handling BLOBs in Crate. So we need to use the [libcurl](http://php.net/manual/de/intro.curl.php) library supported by PHP since version 4.0.2. It's used to upload binaries via HTTP PUT onto a server or host.

The sequence for uploading a binary file to a BLOB table on the Crate server is:

1. Read BLOB content
2. Compute SHA-1 digest out of BLOB content
3. Initialize a cURL session using `curl_init()`
4. Set all necessary options via `curl_setopt()` to use a PUT-Request for transfer method
5. Execute the session with `cur_exec()`
6. Finish off the session using `curl_close()`

The following code listing shows an upload via libcurl. The POST endpoint demonstrates how to generate a `sha1` digest from binary file content and how to use it to create the BLOB on a Crate cluster.

```php
$app->post('/images', function() use ($app)
{
  $data = json_decode($app->request->getBody());

  if (!isset($data->blob)) {
    $app->error(400, "blob is required");
    return;
  }

  $content = base64_decode($data->blob);
  $digest = sha1($content);
  $ch = curl_init("{$app->config['blob_url']}guestbook_images/{$digest}");
  curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "PUT");
  curl_setopt($ch, CURLOPT_POSTFIELDS, $content);
  curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
  $result = curl_exec($ch);
  $info = curl_getinfo($ch);

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

With Crate's BLOB API it's possible to handle BLOBs within different HTTP request methods on the _{HOST}/_blobs/{digest}_ endpoint:

- `PUT` to create/upload blobs
- `DELETE` to remove a blob from the database

For detailed documentation on handling BLOB tables see [BLOB Support](https://crate.io/docs/reference/blob.html).

#### Closing the Connection
The connection to Crate remains active for the lifetime of the Crate PDO object. To close the connection, the variable that holds the PDO object needs to be assigned NULL. It destroys the object by ensuring that all remaining references to it are deleted.
