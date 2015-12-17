<?php
require 'vendor/autoload.php';

$app = new \Slim\Slim();

$app->get('/', function() {
	echo json_encode(array());
});


// posts
$app->get('/posts', function() {
	$crate = new Crate\PDO\PDO("crate:127.0.0.1:4200", null, null, null);
	$qry = $crate->prepare("SELECT * FROM guestbook.posts");
	$qry->execute();
	$result = $qry->fetchAll(PDO::FETCH_ASSOC);

	header("Content-type: application/json");
	echo json_encode($result);
});


$app->post('/posts', function() {
	$crate = new Crate\PDO\PDO("crate:127.0.0.1:4200", null, null, null);
	$qry = $crate->prepare("SELECT * FROM guestbook.posts");
	$qry->execute();

	header("Content-type: application/json");
	echo json_encode(array());
});

$app->put('/posts/:id', function($id) {

});

$app->delete('/posts/:id', function($id) {
	$crate = new Crate\PDO\PDO("crate:127.0.0.1:4200", null, null, null);
	$qry = $crate->prepare("DELETE FROM guestbook.posts WHERE id=?");
	$qry->bindParam(1, $id);
	$state = $qry->execute();

	header("Content-type: application/json");
	echo json_encode(array('success' => $state));
});

$app->put('/posts/:id/likes', function($id) {
	$crate = new Crate\PDO\PDO("crate:127.0.0.1:4200", null, null, null);
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

$app->get('/images', function() {
	echo json_encode(array());
});

$app->post('/images', function() {
	echo json_encode(array());
});

$app->get('/image/:digest', function($digest) {

});


$app->run();