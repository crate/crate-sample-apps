<?php
error_reporting(E_STRICT);

use DI\Container;
use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Psr\Http\Server\MiddlewareInterface;
use Psr\Http\Server\RequestHandlerInterface as RequestHandler;
use Slim\Exception\HttpNotFoundException;
use Slim\Factory\AppFactory;

require 'vendor/autoload.php';


class AppHelper
{
    public $conn;
    public $config;

    function __construct($config)
    {
        $this->config = $config;
        $dsn = "{$config['db_host']}:{$config['db_port']}";
        $this->conn = new Crate\PDO\PDO($dsn, null, null, null);
    }

    function refreshTable($table)
    {
        $qry = $this->conn->prepare("REFRESH TABLE {$table}");
        return $qry->execute();
    }

    function argument_required(Response $response, $message)
    {
        return $this->resource_error($response, 400, $message);
    }

    function not_found(Response $response, $message)
    {
        return $this->resource_error($response, 404, $message);
    }

    function resource_error(Response $response, $status, $message, $contenttype = 'application/json')
    {
        $payload = json_encode(array(
            "error" => $message,
            "status" => $status
        ));
        $response->getBody()->write($payload);
        return $response
            ->withHeader('Content-Type', $contenttype)
            ->withStatus($status);
    }

    function success(Response $response, $status, $result, $contenttype = 'application/json')
    {
        $payload = json_encode($result);
        $response->getBody()->write($payload);
        return $response
            ->withHeader('Content-Type', $contenttype)
            ->withStatus($status);
    }
}


class JsonBodyParserMiddleware implements MiddlewareInterface
{
    // https://www.slimframework.com/docs/v4/objects/request.html#the-request-body
    public function process(Request $request, RequestHandler $handler): Response
    {
        $contentType = $request->getHeaderLine('Content-Type');

        if (strstr($contentType, 'application/json')) {
            $contents = json_decode(file_get_contents('php://input'), true);
            if (json_last_error() === JSON_ERROR_NONE) {
                $request = $request->withParsedBody($contents);
            }
        }
        return $handler->handle($request);
    }
}


$container = new Container();
AppFactory::setContainer($container);

$container->set('helper', function () {
    $config = parse_ini_file('app.ini');
    return new AppHelper($config);
});

$app = AppFactory::create();
$app->add(JsonBodyParserMiddleware::class);

/**
 * Default action.
 */
$app->get('/', function (Request $req, Response $res, $args = [])
{
    $helper = $this->get('helper');
    return $helper->success($res, 200, 'Server up and running');
})->setName('default');

/**
 * Get the post for a given id.
 */
$app->get('/post/{id}', function (Request $req, Response $res, $args = [])
{
    $helper = $this->get('helper');
    $id = $args["id"];
    $qry = $helper->conn->prepare("SELECT p.*, c.name as country, c.geometry as area
            FROM guestbook.posts AS p, guestbook.countries AS c
            WHERE within(p.\"user\"['location'], c.geometry) AND p.id = ?");
    $qry->bindParam(1, $id);
    $qry->execute();
    $result = $qry->fetch(PDO::FETCH_ASSOC);
    if (!$result) {
        return $helper->not_found($res, "Post with id=\"{$id}\" not found");
    } else {
        return $helper->success($res, 200, $result);
    }
})->setName('post-get');

/**
 * Insert a post.
 */
$app->post('/posts', function (Request $req, Response $res, $args = [])
{
    $helper = $this->get('helper');
    $data      = $req->getParsedBody();
    $user      = $data["user"];
    $text      = $data["text"];
    $image_ref = $data["image_ref"];

    if (empty($user)) {
        return $helper->argument_required($res, 'Argument "user" is required');
    } else if (empty($user["name"])) {
        return $helper->argument_required($res, 'Argument "name" is required');
    } else if (empty($user["location"])) {
        return $helper->argument_required($res, 'Argument "location" is required');
    }

    $id        = uniqid();
    $now       = time() * 1000;
    $likeCount = 0;
    $qry       = $helper->conn->prepare("INSERT INTO guestbook.posts (
      id, \"user\", text, created, image_ref, like_count
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
        $helper->refreshTable('guestbook.posts');
        $qry = $helper->conn->prepare("SELECT p.*, c.name as country, c.geometry as area
            FROM guestbook.posts AS p, guestbook.countries AS c
            WHERE within(p.\"user\"['location'], c.geometry) AND p.id = ?");
        $qry->bindParam(1, $id);
        $qry->execute();
        $result = $qry->fetchAll(PDO::FETCH_ASSOC);
        return $helper->success($res, 201, $result);
    } else {
        return $helper->resource_error($res, 500, $helper->conn->errorInfo());
    }
})->setName('post-post');

/**
 * Set the text of a post.
 */
$app->put('/post/{id}', function (Request $req, Response $res, $args = [])
{
    $helper = $this->get('helper');
    $id = $args["id"];
    $data = $req->getParsedBody();

    if(!$data || !isset($data["text"])) {
        return $helper->argument_required($res, 'Argument "text" is required');
    }

    $qry = $helper->conn->prepare("UPDATE guestbook.posts SET text=? WHERE id=?");
    $qry->bindParam(1, $data["text"]);
    $qry->bindParam(2, $id);
    $state = $qry->execute();

    if ($state) {
        $helper->refreshTable("guestbook.posts");
        $qry = $helper->conn->prepare("SELECT p.*, c.name as country, c.geometry as area
            FROM guestbook.posts AS p, guestbook.countries AS c
            WHERE within(p.\"user\"['location'], c.geometry) AND p.id = ?");
        $qry->bindParam(1, $id);
        $result = $qry->fetch(PDO::FETCH_ASSOC);
        return $helper->success($res, 200, $result);
    } else {
        return $helper->resource_error($res, 500, $helper->conn->errorInfo());
    }
})->setName('post-put');

/**
 * Delete a post with a given id.
 */
$app->delete('/post/{id}', function (Request $req, Response $res, $args = [])
{
    $helper = $this->get('helper');
    $id = $args["id"];
    if (empty($id)) {
        return $helper->not_found($res, 'Please provide a post id: /post/<id>');
    }
    $qry = $helper->conn->prepare("SELECT * FROM guestbook.posts WHERE id = ?");
    $qry->bindParam(1, $id);
    $qry->execute();
    $result = $qry->fetchAll(PDO::FETCH_ASSOC);
    if (!$result) {
        return $helper->not_found($res, "Post with id=\"{$id}\" not found");
    }

    $qry = $helper->conn->prepare("DELETE FROM guestbook.posts WHERE id=?");
    $qry->bindParam(1, $id);
    $state = $qry->execute();

    if ($state) {
      return $res->withStatus(204);
    } else {
      // nothing deleted?
      return $helper->not_found($res, "Post with id=\"{$id}\" not deleted");
    }
})->setName('post-delete');

/**
 * Increment the number of likes for a given post.
 */
$app->put('/post/{id}/like', function (Request $req, Response $res, $args = [])
{
    $helper = $this->get('helper');
    $id = $args["id"];
    if (empty($id)) {
        return $helper->not_found($res, 'Please provide a post id: /post/<id>/like');
    }
    $qry = $helper->conn->prepare("SELECT * FROM guestbook.posts WHERE id=?");
    $qry->bindParam(1, $id);
    $result = $qry->execute();
    $row    = $qry->fetch(PDO::FETCH_ASSOC);

    if ($row) {
        $qryU = $helper->conn->prepare("UPDATE guestbook.posts SET like_count = like_count + 1 WHERE id=?");
        $qryU->bindParam(1, $id);
        $state = $qryU->execute();

        if ($state) {
            $helper->refreshTable("guestbook.posts");
            $qryS = $helper->conn->prepare("SELECT p.*, c.name as country, c.geometry as area
                FROM guestbook.posts AS p, guestbook.countries AS c
                WHERE within(p.\"user\"['location'], c.geometry) AND p.id = ?");
            $qryS->bindParam(1, $id);
            $result = $qryS->fetch(PDO::FETCH_ASSOC);
            return $helper->success($res, 200, $result);
        } else {
            return $helper->resource_error($res, 500, 'update statement went wrong');
        }
    } else {
        return $helper->not_found($res, "Post with id=\"{$id}\" not found");
    }
})->setName('post-like-put');

/**
 * Get a list of all posts.
 */
$app->get('/posts', function (Request $req, Response $res, $args = [])
{
    $helper = $this->get('helper');
    $qry = $helper->conn->prepare("SELECT p.*, c.name as country, c.geometry as area
            FROM guestbook.posts AS p, guestbook.countries AS c
            WHERE within(p.\"user\"['location'], c.geometry)
            ORDER BY p.created DESC");
    $qry->execute();
    $result = $qry->fetchAll(PDO::FETCH_ASSOC);
    return $helper->success($res, 200, $result);
})->setName('posts-get');

/**
 * Return all images that are saved in the CrateDB blob store.
 */
$app->get('/images', function (Request $req, Response $res, $args = [])
{
    $helper = $this->get('helper');
    $qry = $helper->conn->prepare("SELECT digest, last_modified FROM blob.guestbook_images");
    $qry->execute();
    $result = $qry->fetchAll(PDO::FETCH_ASSOC);
    return $helper->success($res, 200, $result);
})->setName('images-get');

/**
 * Insert an image.
 */
$app->post('/images', function (Request $req, Response $res, $args = [])
{
    $helper = $this->get('helper');
    $data = $req->getParsedBody();
    if (!isset($data["blob"])) {
        return $helper->argument_required($res, 'Argument "blob" is required');
    }
    $content = base64_decode($data["blob"]);
    $digest  = sha1($content);
    $ch      = curl_init("{$helper->config['blob_url']}guestbook_images/{$digest}");
    curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "PUT");
    curl_setopt($ch, CURLOPT_POSTFIELDS, $content);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
    $result = curl_exec($ch);
    $info   = curl_getinfo($ch);
    //print("INFO");
    //print_r($info);
    if ($info['http_code'] != 201 && $info['http_code'] != 409) {
        return $helper->resource_error($res, $info['http_code'], curl_error($ch));
    } else {
        return $helper->success($res, $info['http_code'], array(
            'url' => "/image/{$digest}",
            'digest' => $digest
        ));
    }
})->setName('image-post');

/**
 * Return the image for a given digest.
 */
$app->get('/image/{digest}', function (Request $req, Response $res, $args = [])
{
    $helper = $this->get('helper');
    $digest = $args["digest"];
    if (empty($digest)) {
        return $helper->not_found($res, 'Please provide an image digest: /image/<digest>');
    }
    $ch     = curl_init("{$helper->config['blob_url']}guestbook_images/{$digest}");
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    $result = curl_exec($ch);
    if (!$result || is_bool($result)) {
        $info = curl_getinfo($ch);
        return $helper->not_found($res, "Image with digest=\"{$digest}\" not found");
    } else {
        $res->getBody()->write($result);
        return $res
            ->withHeader('Content-Type', "image/gif")
            ->withHeader('Content-Length', strlen($result))
            ->withStatus(200);
    }
})->setName('image-get');

/**
 * Delete an image that is saved in the blobstore with the given digest.
 */
$app->delete('/image/{digest}', function (Request $req, Response $res, $args = [])
{
    $helper = $this->get('helper');
    $digest = $args["digest"];
    if (empty($digest)) {
        return $helper->not_found($res, 'Please provide an image digest: /image/<digest>');
    }
    $ch = curl_init("{$helper->config['blob_url']}guestbook_images/{$digest}");
    curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "DELETE");
    $result = curl_exec($ch);
    $info   = curl_getinfo($ch);
    if ($info['http_code'] == 404) {
        return $helper->not_found($res, "Image with digest=\"{$digest}\" not found");
    } else if ($info['http_code'] == 204) {
        return $res->withStatus(204);
    } else {
        $err = curl_error($ch);
        return $helper->resource_error($res, 500, "Could not delete image: {$err}");
    }
})->setName('image-delete');

/**
 * Search for posts.
 */
$app->post('/search', function (Request $req, Response $res, $args = [])
{
    $helper = $this->get('helper');
    $data = $req->getParsedBody();
    if (!isset($data["query_string"])) {
        return $helper->argument_required($res, 'Argument "query_string" is required');
    }
    $qry = $helper->conn->prepare("SELECT p.*, p._score as _score,
              c.name as country, c.geometry as area
            FROM guestbook.posts AS p, guestbook.countries AS c
            WHERE within(p.\"user\"['location'], c.geometry)
              AND match(p.text, ?)");
    $qry->bindParam(1, $data["query_string"]);
    $qry->execute();
    $result = $qry->fetchAll(PDO::FETCH_ASSOC);
    return $helper->success($res, 200, $result);
})->setName('search');

/**
 * Enable lazy CORS.
 * https://www.slimframework.com/docs/v4/cookbook/enable-cors.html
 */
$app->options('/{routes:.+}', function ($request, $response, $args) {
    return $response;
});

$app->add(function ($request, $handler) {
    $response = $handler->handle($request);
    return $response
        ->withHeader('Access-Control-Allow-Origin', '*')
        ->withHeader('Access-Control-Allow-Headers', 'X-Requested-With, Content-Type, Accept, Origin, Authorization')
        ->withHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, PATCH, OPTIONS');
});

/**
 * Catch-all route to serve a 404 Not Found page if none of the routes match.
 * NOTE: make sure this route is defined last.
 */
$app->map(['GET', 'POST', 'PUT', 'DELETE', 'PATCH'], '/{routes:.+}', function ($request, $response) {
    throw new HttpNotFoundException($request);
});

$app->run();

?>
