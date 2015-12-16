<?php
require 'vendor/autoload.php';

$crate = new \Crate\PDO\PDO("crate:localhost:4200", null, null, null);

$app = new \Slim\Slim();

$app->get('/', function() {
	echo json_encode(array());
});


// posts

$app->post('/posts', function() {
	echo json_encode(array());
});

$app->get('/posts', function() {
	echo json_encode(array());
});

$app->put('/posts/:id', function($id) {

});

$app->delete('/posts/:id', function($id) {

});

$app->put('/posts/:id/likes', function($id) {
	echo json_encode(1);
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