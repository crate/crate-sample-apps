<?php
require 'vendor/autoload.php';

$app = new \Slim\Slim();

$app->get('/', function() {
	echo json_encode(array());
});

$crate = new Crate\PDO\PDO("crate:127.0.0.1:4200", null, null, null);

// posts
$app->get('/posts', function() use ($app, $crate) {
	$qry = $crate->prepare("SELECT * FROM guestbook.posts");
	$qry->execute();
	$result = $qry->fetchAll(PDO::FETCH_ASSOC);

	header("Content-type: application/json");
	echo json_encode($result);
});


$app->post('/posts', function() use ($app, $crate) {
	$id = uniqid();
	$likeCount = 0;
	$qry = $crate->prepare("INSERT INTO guestbook.posts (id, user, text, created, image_ref, like_count) 
			VALUES(?, ?, ?, ?, ?, ?)");
	$qry->bindParam(1, $id);
	$qry->bindParam(2, json_decode($app->request->post('user')));
	$qry->bindParam(3, $app->request->post('text'));
	$qry->bindParam(4, time());
	$qry->bindParam(5, $app->request->post('image_ref'));
	$qry->bindParam(6, $likeCount);
	$success = $qry->execute();

	if($success) {
		$qry = $crate->prepare("DELETE FROM guestbook.posts WHERE id=?");
		$qry->bindParam(1, $id);
		$state = $qry->execute();

		header("Content-type: application/json");
		echo json_encode(array('success' => $state));
	} else {
		// header("Content-type: application/json");
		echo json_encode(array('error' => $qry->errorCode(), 'errorInfo' => $qry->errorInfo()));		
	}
});

$app->put('/posts/:id', function($id) use ($app, $crate) {
	header("Content-type: application/json");
	echo json_encode(array());
});

$app->delete('/posts/:id', function($id) use ($app, $crate) {
	$qry = $crate->prepare("DELETE FROM guestbook.posts WHERE id=?");
	$qry->bindParam(1, $id);
	$state = $qry->execute();

	header("Content-type: application/json");
	echo json_encode(array('success' => $state));
});

$app->put('/posts/:id/likes', function($id) use ($app, $crate) {
	$qry = $crate->prepare("SELECT * FROM guestbook.posts WHERE id=?");
	$qry->bindParam(1, $id);
	$qry->execute();
	$row = $qry->fetch(PDO::FETCH_ASSOC);

	$qryU = $crate->prepare("UPDATE guestbook.posts SET like_count=? WHERE id=?");
	$likeCount = $row['like_count'];
	$likeCount++;
	$qryU->bindParam(1, $likeCount);
	$qryU->bindParam(2, $id);
	$state = $qryU->execute();

	header("Content-type: application/json");
	echo json_encode(array('success' => $state));
});


// images

$app->get('/images', function() use ($app, $crate) {
	echo json_encode(array());
});

$app->post('/images', function() use ($app, $crate) {
	echo json_encode(array());
});

$app->get('/image/:digest', function($digest) use ($app, $crate) {

});


$app->run();