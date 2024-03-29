# CrateDB PDO Sample Application

## Installation
The PHP PDO client library for CrateDB is available as [crate-pdo on
Packagist], and can be installed by using [Composer], or by manually
adding it as a dependency to your project's `composer.json`.

### Directly
Install with Composer:

```bash
composer require crate/crate-pdo:2.*
```

### Manually
Add the following dependency to `composer.json`:

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

Composer will install the library and its dependencies into the `./vendor/` directory.

## Usage
After installing the driver, load it automatically by adding this line to the top of the php file.

```php
require 'vendor/autoload.php'
```

### Connecting to a CrateDB Cluster
To connect to a cluster, CrateDB follows the standard PDO constructor interface
`PDO($dsn, $user, $password, $options)` and its syntax to form a data source name
string ([dsn]). See also the documentation about [authenticating with CrateDB PDO]
for more details.

```php
require_once 'vendor/autoload.php';

$dsn = 'crate:SERVER_IP:4200';
$conn = new Crate\PDO\PDO($dsn, null, null, null);
```

In the PHP example application, we read the DSN connection information from the
`app.ini` configuration file. The PDO connection is set in the constructor of
the  `CrateResource` instance, which serves the web application based on the
[Slim] web framework.

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
SQL statements can be executed using the `query()` method on the [PDOStatement]
object that gets returned when the query is prepared. Each `query()` call
will run a HTTP request to the CrateDB server. The response of the request
(such as `rowCount()`, `columnCount()`, etc.) is written directly to the
[PDOStatement] object instance.

```php
$query = $conn->query("SELECT * FROM guestbook.posts");
echo "table guestbook.posts counts " . $query->rowCount() . " row(s)";
```

To perform parameter substitution the method `bindParam()` ensures that the
parameters bind correctly to their assigned values. Afterwards execute the
query using the `execute()` method on the variable that holds the prepared
statement object.

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

- For a prepared statement using named placeholders, the first argument is the
  parameter name in the form `:name`.
- For a prepared statement using question mark placeholders, this will be the
  **1-indexed** position of the parameter.

#### Fetching Query Results
There are multiple ways to fetch data from row returning statements (DQL):

- `fetch()`: Fetches the next row from a result set, see [PDOStatement::fetch].
- `fetchall()`: Returns an array containing all the result set rows, see [PDOStatement::fetchall].

```php
$qry = $conn->query("SELECT COUNT(*) FROM sys.nodes");
$row = $qry->fetch();
echo "CrateDB cluster has " . $row[0] . " nodes";
```

Our example application uses the data obtained from `fetchall()` and forwards the output directly to the response body. The fetch style argument `PDO::FETCH_ASSOC` ensures that the array is indexed by column name.

```php
$result = $qry->fetchAll(PDO::FETCH_ASSOC);
$app->success(200, $result);
```

#### Handling BLOBs
The CrateDB PDO does not have an implementation or API for handling BLOBs in
CrateDB. So we need to use the [libcurl] library supported by PHP since
version 4.0.2. It is used to upload files via HTTP PUT onto a server or host.

The sequence for uploading a file to a BLOB table on the CrateDB server is:

1. Read BLOB content
2. Compute SHA-1 digest out of BLOB content
3. Initialize a cURL session using `curl_init()`
4. Set all necessary options via `curl_setopt()` to use a PUT-Request for transfer method
5. Execute the session with `cur_exec()`
6. Finish off the session using `curl_close()`

The following PHP code demonstrates the corresponding procedure. The POST
endpoint demonstrates how to generate a `sha1` digest from binary file content
and how to use it to create the BLOB on a CrateDB cluster.

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

With CrateDB's BLOB API, it is possible to handle BLOBs within different HTTP
request methods on the `{HOST}/_blobs/{digest}` endpoint:

- `PUT` to create/upload blobs
- `DELETE` to remove a blob from the database

For detailed documentation on handling BLOB tables, see [BLOB support].

#### Closing the connection
The connection to CrateDB remains active for the lifetime of the CrateDB PDO
object. To close the connection, the variable that holds the PDO object needs
to be assigned `NULL`. It destroys the object by ensuring that all remaining
references to it are deleted.


[authenticating with CrateDB PDO]: https://crate.io/docs/pdo/en/latest/connect.html#get-a-connection
[BLOB support]: https://crate.io/docs/reference/blob.html
[Composer]: https://getcomposer.org/
[crate-pdo on Packagist]: https://packagist.org/packages/crate/crate-pdo
[dsn]: https://en.wikipedia.org/wiki/Data_source_name
[libcurl]: https://www.php.net/manual/en/intro.curl.php
[PDOStatement]: https://www.php.net/manual/en/class.pdostatement.php
[PDOStatement::fetch]: https://www.php.net/manual/en/pdostatement.fetch.php
[PDOStatement::fetchall]: https://www.php.net/manual/en/pdostatement.fetchall.php
[Slim]: https://www.slimframework.com/
