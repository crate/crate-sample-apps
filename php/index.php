<?php
require 'vendor/autoload.php';
$config = parse_ini_file('app.ini');

// META CONTENT TYPES
define('CONTENT_TYPE_JSON', 'Content-type: application/json');

class CrateResource extends \Slim\Slim
{
  public $conn;

  function __construct($config)
  {
    parent::__construct();
    $this->conn = new Crate\PDO\PDO("{$config['db_dsn']}:{$config['db_port']}" , null, null, null);
  }

  function argument_required($message) {
    $this->error(400, $message);
  }

  function not_found($message) {
    $this->error(404, $message);
  }

  function error($status, $message, $contenttype='application/json') {
    $this->response->headers->set('Content-Type', $contenttype);
    $this->response->setStatus($status);
    $this->response->write(json_encode(array("error" => $message, "status" => $status)));
  }

  function success($status, $result, $contenttype='application/json') {
    $this->response->headers->set('Content-Type', $contenttype);
    $this->response->setStatus($status);
    $this->response->write(json_encode($result));
  }
}

$app = new CrateResource($config);

$app->get('/', function() use ($app) {
  $app->success(200, 'Server up and running');
});

// posts
$app->get('/posts', function() use ($app) {
	$qry = $app->conn->prepare("SELECT * FROM guestbook.posts");
	$qry->execute();
	$result = $qry->fetchAll(PDO::FETCH_ASSOC);
  $app->success(200, $result);
});

$app->post('/posts', function() use ($app) {
  $user = json_decode($app->request->post('user'));
  $text = $app->request->post('text');
  $image_ref = $app->request->post('image_ref');

  if ( empty($user) ) {
    $app->argument_required('user parameter is empty');
    return;
  } else if ( empty($user->{'name'}) ) {
    $app->argument_required('user name parameter is empty');
    return;
  }

	$id = uniqid();
	$likeCount = 0;
	$qry = $app->conn->prepare("INSERT INTO guestbook.posts (id, user, text, created, image_ref, like_count)
			VALUES(?, ?, ?, ?, ?, ?)");
	$qry->bindParam(1, $id);
	$qry->bindParam(2, $user);
	$qry->bindParam(3, $text);
	$qry->bindParam(4, time());
	$qry->bindParam(5, $image_ref);
	$qry->bindParam(6, $likeCount);
	$state = $qry->execute();

  if ($state) {
    $app->success(201, array('success' => $state));
  } else {
    $app->error(500, $app->conn->errorInfo());
  }
});

$app->put('/posts/:id', function($id) use ($app) {

	$qry = $app->conn->prepare("UPDATE guestbook.posts SET text=? WHERE id=?");
	$qry->bindParam(1, $app->request->post('text'));
	$qry->bindParam(2, $id);
	$state = $qry->execute();

  if ($state) {
    $app->success(201, array('success' => $state));
  } else {
    $app->error(500, $app->conn->errorInfo());
  }
});

$app->delete('/posts/:id', function($id) use ($app) {
  if ( empty($id) ) {
    $app->argument_required('post-id argument is empty');
    return;
  }
	$qry = $app->conn->prepare("DELETE FROM guestbook.posts WHERE id=?");
	$qry->bindParam(1, $id);
	$state = $qry->execute();

  if ($state) {
    $app->success(204, array('success' => $state));
  } else {
    $app->not_found('post-id with {$id} not found');
  }
});

$app->put('/posts/:id/likes', function($id) use ($app) {
  if ( empty($id) ) {
    $app->argument_required('post-id parameter is empty');
    return;
  }
	$qry = $app->conn->prepare("SELECT * FROM guestbook.posts WHERE id=?");
	$qry->bindParam(1, $id);
	$result = $qry->execute();
	$row = $qry->fetch(PDO::FETCH_ASSOC);

  if ($result) {
    $qryU = $app->conn->prepare("UPDATE guestbook.posts SET like_count = like_count + 1 WHERE id=?");
  	$qryU->bindParam(1, $id);
  	$state = $qryU->execute();

    if ($state) {
      $app->success(204, array('success' => $state));
    } else {
      $app->error(500, 'update statement went wrong');
    }
  } else {
    $app->not_found('post-id with {$id} not found');
  }
});

$app->get('/images', function() use ($app) {
	echo json_encode(array());
});

$app->post('/images', function() use ($app) {
	echo json_encode(array());
});

$app->get('/image/:digest', function($digest) use ($app) {

});

$app->run();