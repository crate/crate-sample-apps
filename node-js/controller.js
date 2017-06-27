//requirements
var uuid = require('uuid/v1');
var sha1 = require('sha1');
var crate = require('node-crate');
var app = require('express')();
var bodyParser = require('body-parser');

//configurables
var crate_host = 'localhost';
var crate_port = '4200';
var service_port = 8080;

//bootstrap
crate.connect(crate_host, crate_port);
app.all('/*', function(req, res, next) {
   res.setHeader("Access-Control-Allow-Origin", "*");
   res.setHeader("Access-Control-Allow-Credentials", "true");    
   res.setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, UPDATE, DELETE, OPTION");
   res.setHeader("Access-Control-Allow-Headers", "Access-Control-Allow-Headers, Origin,Accept, X-Requested-With, Content-Type, Access-Control-Request-Method, Access-Control-Request-Headers");
   next();
})
app.use(bodyParser.json({limit: '5mb'})); // for parsing application/json
app.use(bodyParser.raw({limit: '5mb'}));
app.use(bodyParser.urlencoded({limit: '5mb', extended: true })); // for parsing application/x-www-form-urlencoded

//rest-service-functions
//############POST-RESOURCE##########################

//POST /posts - Create a new post. 
app.post('/posts', function(req, res){
    res.setHeader('Content-Type', 'application/json');
    
    //parsed data is found in req.body

    //check for required data
    if(!req.body.text) {
        res.status(400).json({
            error: 'Argument "text" is required',
            status: 400
        });
        return;
    }
    if(!req.body.user.location) {
        res.status(400).json({
            error: 'Argument "location" is required',
            status: 400
        })
        return;
    }
    
    var tablename = 'guestbook.posts';
    var id = uuid();
    //insert
    crate.insert(
    tablename, 
    {
        "id": id,
        "user": req.body.user,
        "text": req.body.text,
        "created": new Date().getTime(),
        "image_ref": req.body.image_ref,
        "like_count": 0
    }).then(() => {
        //refresh table to make sure new record is immediately available
        refreshTable(tablename).then(() => {
            //fetch new record
            getPost(id).then((response) => {
                res.status(201).json(response.json);
            })
        })
    });
});

//GET /posts - Retrieve a list of all posts. 
app.get('/posts', function(req, res) {
    res.setHeader('Content-Type', 'application/json');

    getPosts().then((response) => {
        res.status(200).json(response.json);
    });
});

//GET /post/{id} - Retrieves the post with the corresponding id. 
app.get('/post/:id', function(req, res) {
    res.setHeader('Content-Type', 'application/json');
    
    getPost(req.params.id).then((response) => {
        res.status(200).json(response.json);
    })
});

//PUT /post/{id} - Updates text property of given id. 
app.put('/post/:id', function(req, res) {
    res.setHeader('Content-Type', 'application/json');

    var body = [];
    req.on('error', function(err) {
        console.error(err);
    }).on('data', function(chunk) {
        body.push(chunk);
    }).on('end', function() {
        body = Buffer.concat(body).toString();
        var jbody = JSON.parse(body);
        
        updatePost(req.params.id, jbody.text).then(() => {
            refreshTable().then((_) => {
                getPost(req.params.id).then((response) => {
                    res.status(200).json(response.json);
                })
            })
        }).catch(() => {
            res.status(404).end();
        })
    });
});

//### `PUT /post/<id>/like` Increments the like count for a given post by one.
app.put('/post/:id/like', function(req, res){
    res.setHeader('Content-Type', 'application/json');

    var postID = req.params.id;
    getPost(postID).then((response) => {
        var newLikes = response.json[0].like_count + 1;
        likePost(postID, newLikes).then(() => {
            response.json[0].like_count += 1;
            res.status(200).json(response.json);
            })
    }).catch(() => {
        res.status(404).end();
    });
});

//### `DELETE /post/<id>`   Delete a post with given `id`.
app.delete('/post/:id', function(req, res){

    var postID = req.params.id;
    deletePost(postID).then(() => {
            res.status(204).end();
    }).catch(() => {
            res.status(404).end();
    });
});


//############IMAGE-RESOURCE##########################

app.post('/images', function(req, res){
    res.setHeader('Content-Type', 'application/json');

    if(!req.body.blob){
        res.status(400).json({
            error: 'Argument "blob" is required',
            status: 400
        });
        return;
    }

    var b64String = req.body.blob;
    var buf = Buffer.from(b64String, 'base64');

    var encrypted = sha1(buf);

    var urlValue = 'localhost:'+service_port+'/image/'+encrypted;

    var result = {};
    result['url'] = urlValue;
    result['digest'] = encrypted;

    crate.insertBlob('guestbook_images', buf).then(() => {
        res.status(201).json(result);
    }).catch(() => {
        res.status(409).json(result);
    });
});

app.get('/images', function(req, res) {
    res.setHeader('Content-Type', 'application/json');

    getImages().then((response) => {
        res.status(200).json(response.json);
    })
})

app.get('/image/:digest', function(req, res) {
    res.setHeader('Content-Type', 'image/gif');

    getImage(req.params.digest).then((data) => {
         res.end(data);
    });
});

//### `DELETE /image/<digest>` Delete an image with given `digest`.
app.delete('/image/:digest', function(req, res){

    var digest = req.params.digest;
    deletePost(digest).then(() => {
            res.status(204).end();
    }).catch(() => {
            res.status(404).end();
    });
});

//launch
var server = app.listen(service_port, function() {
   var host = server.address().address;

   console.log("app listening at http://%s:%s", host, service_port);
})

//helper
function getPosts() {
   return crate.execute("SELECT p.*, c.name as country, c.geometry as area FROM guestbook.posts AS p, guestbook.countries AS c WHERE within(p.user['location'], c.geometry) ORDER BY p.created DESC");
}

function getPost(id) {
    return crate.execute("SELECT p.*, c.name as country, c.geometry as area FROM guestbook.posts AS p, guestbook.countries AS c WHERE within(p.user['location'], c.geometry) AND p.id='"+id+"'");
}

function updatePost(id, newText) {
    var where = "id='"+id+"'";
    return crate.update('guestbook.posts', { text: newText }, where);
}

function likePost(id, newLikes) {
    var where = "id='"+id+"'";
    return crate.update('guestbook.posts', { like_count: newLikes }, where);
}

function deletePost(id) {
    var where = "id='"+id+"'";
    return crate.delete('guestbook.posts', where);
}

function getImages(){
    return crate.execute("SELECT digest, last_modified " + "FROM Blob.guestbook_images " + "ORDER BY 2 DESC");
}

function getImage(digest) {
    return crate.getBlob('guestbook_images', digest);
}

function deleteImage(digest) {
    var where = "id='"+digest+"'";
    console.log('deleting image with id:'+digest);
    return crate.delete('guestbook_images', where);
}

function refreshTable(tablename) {
    return crate.execute("REFRESH TABLE "+tablename);
}

