<?php
error_reporting(E_STRICT);

require 'vendor/autoload.php';
$config = parse_ini_file('app.ini');

class CrateResource extends \Slim\Slim
{
    public $conn;
    public $config;

    function __construct($config)
    {
        parent::__construct();
        $this->config = $config;
        $dsn = "{$config['db_host']}:{$config['db_port']}";
        $this->conn = new Crate\PDO\PDO($dsn, null, null, null);
    }

    function refreshTable($table)
    {
        $qry = $this->conn->prepare("REFRESH TABLE {$table}");
        $result = $qry->execute();
    }

    function argument_required($message)
    {
        $this->resource_error(400, $message);
    }

    function not_found($message)
    {
        $this->resource_error(404, $message);
    }

    function resource_error($status, $message, $contenttype = 'application/json')
    {
        $this->response->headers->set('Content-Type', $contenttype);
        $this->response->setStatus($status);
        $this->response->write(json_encode(array(
            "error" => $message,
            "status" => $status
        )));
    }

    function success($status, $result, $contenttype = 'application/json')
    {
        $this->response->headers->set('Content-Type', $contenttype);
        $this->response->setStatus($status);
        $this->response->write(json_encode($result));
    }
}

$app = new CrateResource($config);
// apply CORS headers to all responses
$app->add(new \CorsSlim\CorsSlim());

/**
 * Default action.
 */
$app->get('/', function() use ($app)
{
    $app->success(200, 'Server up and running');
})->name('default');

/**
 * Get the post for a given id.
 */
$app->get('/post/:id', function($id) use ($app)
{
    $qry = $app->conn->prepare("SELECT p.*, c.name as country, c.geometry as area
            FROM guestbook.posts AS p, guestbook.countries AS c
            WHERE within(p.user['location'], c.geometry) AND p.id = ?");
    $qry->bindParam(1, $id);
    $qry->execute();
    $result = $qry->fetch(PDO::FETCH_ASSOC);
    if (!$result) {
        $app->not_found("Post with id=\"{$id}\" not found");
    } else {
        $app->success(200, $result);
    }
})->name('post-get');
/**
 * insert a posts.
 */
$app->post('/posts', function() use ($app)
{
    $data      = json_decode($app->request->getBody());
    $user      = $data->user;
    $text      = $data->text;
    $image_ref = $data->image_ref;

    if (empty($user)) {
        $app->argument_required('Argument "user" is required');
        return;
    } else if (empty($user->name)) {
        $app->argument_required('Argument "name" is required');
        return;
    } else if (empty($user->location)) {
        $app->argument_required('Argument "location" is required');
        return;
    }

    $id        = uniqid();
    $now       = time() * 1000;
    $likeCount = 0;
    $qry       = $app->conn->prepare("INSERT INTO guestbook.posts (
      id, user, text, created, image_ref, like_count
    ) VALUES (
      ?, ?, ?, ?, ?, ?
    )");
    $qry->bindParam(1, $id);
    //$user = array('name' => 'test', 'location' => array(9.74379 , 47.4124));
    $qry->bindParam(2, $user);
    $qry->bindParam(3, $text);
    $qry->bindParam(4, $now);
    $qry->bindParam(5, $image_ref);
    $qry->bindParam(6, $likeCount);
    $state = $qry->execute();

    if ($state) {
        $app->refreshTable('guestbook.posts');
        $qry = $app->conn->prepare("SELECT p.*, c.name as country, c.geometry as area
            FROM guestbook.posts AS p, guestbook.countries AS c
            WHERE within(p.user['location'], c.geometry) AND p.id = ?");
        $qry->bindParam(1, $id);
        $qry->execute();
        $result = $qry->fetchAll(PDO::FETCH_ASSOC);
        $app->success(201, $result);
    } else {
        $app->resource_error(500, $app->conn->errorInfo());
    }
})->name('post-post');

/**
 * sets the text of a post
 */
$app->put('/post/:id', function($id) use ($app)
{
    $data = json_decode($app->request->getBody());

    if(!$data || !isset($data->text)) {
        $app->argument_required('Argument "text" is required');
        return;
    }

    $qry = $app->conn->prepare("UPDATE guestbook.posts SET text=? WHERE id=?");
    $qry->bindParam(1, $data->text);
    $qry->bindParam(2, $id);
    $state = $qry->execute();

    if ($state) {
        $app->refreshTable("guestbook.posts");
        $qry = $app->conn->prepare("SELECT p.*, c.name as country, c.geometry as area
            FROM guestbook.posts AS p, guestbook.countries AS c
            WHERE within(p.user['location'], c.geometry) AND p.id = ?");
        $qry->bindParam(1, $id);
        $result = $qry->fetch(PDO::FETCH_ASSOC);
        $app->success(200, $result);
    } else {
        $app->resource_error(500, $app->conn->errorInfo());
    }
})->name('post-put');

/**
 * deletes a post with a given id.
 */
$app->delete('/post/:id', function($id) use ($app)
{
    if (empty($id)) {
        $app->not_found('Please provide a post id: /post/<id>');
        return;
    }
    $qry = $app->conn->prepare("SELECT * FROM guestbook.posts WHERE id = ?");
    $qry->bindParam(1, $id);
    $qry->execute();
    $result = $qry->fetchAll(PDO::FETCH_ASSOC);
    if (!$result) {
        $app->not_found("Post with id=\"{$id}\" not found");
        return;
    }

    $qry = $app->conn->prepare("DELETE FROM guestbook.posts WHERE id=?");
    $qry->bindParam(1, $id);
    $state = $qry->execute();

    if ($state) {
      $app->response->setStatus(204);
    } else {
      // nothing deleted?
      $app->not_found("Post with id=\"{$id}\" not deleted");
    }
})->name('post-delete');

/**
 * increments the number of likes for a given post.
 */
$app->put('/post/:id/like', function($id) use ($app)
{
    if (empty($id)) {
        $app->not_found('Please provide a post id: /post/<id>/like');
        return;
    }
    $qry = $app->conn->prepare("SELECT * FROM guestbook.posts WHERE id=?");
    $qry->bindParam(1, $id);
    $result = $qry->execute();
    $row    = $qry->fetch(PDO::FETCH_ASSOC);

    if ($row) {
        $qryU = $app->conn->prepare("UPDATE guestbook.posts SET like_count = like_count + 1 WHERE id=?");
        $qryU->bindParam(1, $id);
        $state = $qryU->execute();

        if ($state) {
            $app->refreshTable("guestbook.posts");
            $qryS = $app->conn->prepare("SELECT p.*, c.name as country, c.geometry as area
                FROM guestbook.posts AS p, guestbook.countries AS c
                WHERE within(p.user['location'], c.geometry) AND p.id = ?");
            $qryS->bindParam(1, $id);
            $result = $qryS->fetch(PDO::FETCH_ASSOC);
            $app->success(200, $result);
        } else {
            $app->resource_error(500, 'update statement went wrong');
        }
    } else {
        $app->not_found("Post with id=\"{$id}\" not found");
    }
})->name('post-like-put');

/**
 * Get a list of all posts.
 */
$app->get('/posts', function() use ($app)
{
    $qry = $app->conn->prepare("SELECT p.*, c.name as country, c.geometry as area
            FROM guestbook.posts AS p, guestbook.countries AS c
            WHERE within(p.user['location'], c.geometry)");
    $qry->execute();
    $result = $qry->fetchAll(PDO::FETCH_ASSOC);
    $app->success(200, $result);
})->name('posts-get');

/**
 * returns all images that are saved in the crate blob store.
 */
$app->get('/images', function() use ($app)
{
    $qry = $app->conn->prepare("SELECT digest, last_modified FROM blob.guestbook_images");
    $qry->execute();
    $result = $qry->fetchAll(PDO::FETCH_ASSOC);
    $app->success(200, $result);
})->name('images-get');

/**
 * inserts an image
 */
$app->post('/images', function() use ($app)
{
    $data = json_decode($app->request->getBody());
    if (!isset($data->blob)) {
        $app->argument_required('Argument "blob" is required');
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
    if ($info['http_code'] != 201) {
        $app->resource_error($info['http_code'], curl_error($ch));
    } else {
        $app->success($info['http_code'], array(
            'url' => "/image/{$digest}",
            'digest' => $digest
        ));
    }
})->name('image-post');

/**
 * returns the image for a given digest.
 */
$app->get('/image/:digest', function($digest) use ($app)
{
    if (empty($digest)) {
        $app->not_found('Please provide an image digest: /image/<digest>');
        return;
    }
    $ch     = curl_init("{$app->config['blob_url']}guestbook_images/{$digest}");
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    $result = curl_exec($ch);
    if (!$result || is_bool($result)) {
        $info = curl_getinfo($ch);
        $app->not_found("Image with digest=\"{$digest}\" not found");
    } else {
        $app->response->headers->set("Content-Type", "image/gif");
        $app->response->headers->set("Content-Length", strlen($result));
        $app->response->setStatus(200);
        $app->response->write($result);
    }
})->name('image-get');

/**
 * deltes a image that is saved in the blobstore with the given digest.
 */
$app->delete('/image/:digest', function($digest) use ($app)
{
    if (empty($digest)) {
        $app->not_found('Please provide an image digest: /image/<digest>');
        return;
    }
    $ch = curl_init("{$app->config['blob_url']}guestbook_images/{$digest}");
    curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "DELETE");
    $result = curl_exec($ch);
    $info   = curl_getinfo($ch);
    if ($info['http_code'] == 404) {
        $app->not_found("Image with digest=\"{$digest}\" not found");
    } else if ($info['http_code'] == 204) {
        $app->response->setStatus(204);
    } else {
        $err = curl_error($ch);
        $app->resource_error(500, "Could not delete image: {$err}");
    }
})->name('image-delete');

$app->post('/search', function() use ($app)
{
    $data = json_decode($app->request->getBody());
    if (!isset($data->query_string)) {
        $app->argument_required('Argument "query_string" is required');
        return;
    }
    $qry = $app->conn->prepare("SELECT p.*, p._score as _score,
              c.name as country, c.geometry as area
            FROM guestbook.posts AS p, guestbook.countries AS c
            WHERE within(p.user['location'], c.geometry)
              AND match(p.text, ?)");
    $qry->bindParam(1, $data->query_string);
    $qry->execute();
    $result = $qry->fetchAll(PDO::FETCH_ASSOC);
    $app->success(200, $result);
})->name('search');

$app->run();

?>
