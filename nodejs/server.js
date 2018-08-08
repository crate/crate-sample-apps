const express = require('express');
const cors = require('cors');
const bodyParser = require('body-parser');
const crate = require('node-crate');
const uniqid = require('uniqid');
const sha1 = require('sha1');
const request = require('request');
const app = express();

app.use(cors());
app.use(bodyParser.json({
  limit: '50mb'
}));
app.use(bodyParser.urlencoded({
  limit: '50mb',
  extended: true,
  parameterLimit: 1000000
}));

crate.connect('localhost', 4200);

app.listen(8080, () => console.log('Example app listening on port 8080!'));

app.get('/', (req, response) => response.json({
  message: 'Welcome to the node.js example for CrateDB',
  state: 'Server up and running'
}));

// Get all posts
app.get('/posts', (req, response) => {
  crate.execute("SELECT p.*, c.name as country, c.geometry as area "
                + "FROM guestbook.posts AS p, guestbook.countries AS c "
                + "WHERE within(p.\"user\"['location'], c.geometry) "
                + "ORDER BY p.created DESC").then((res) => {
    response.status(200).json(res.json);
  });
});

// Get a post by id
app.get('/post/:id', (req, response) => {
  crate.execute("SELECT p.*, c.name as country, c.geometry as area "
                + "FROM guestbook.posts AS p, guestbook.countries AS c "
                + "WHERE within(p.\"user\"['location'], c.geometry) "
                + "AND p.id = ?", [req.params.id]).then((res) => {
    response.status(200).json(res.json[0]);
  });
});

// Insert new post
app.post('/posts', (req, response) => {
  if (!req.body) {
    return response.json({
      message: 'Please pass a body to the request',
    });
  }
  if (!req.body.user.name) {
    return response.json({
      message: 'Please insert a name',
    });
  }
  if (!req.body.user.location || req.body.user.location.length === 0) {
    return response.json({
      message: 'Please insert a location',
    });
  }
  let id = uniqid();
  crate.execute("INSERT INTO guestbook.posts "
                + "(id, \"user\", text, created, image_ref, like_count) "
                + "VALUES (?, ?, ?, CURRENT_TIMESTAMP, ?, ?)", [
    id,
    req.body.user,
    req.body.text,
    req.body.image_ref,
    0
  ]).then((res) => {
    if (res.rowcount === 0) {
      return response.json({
        message: 'DB error',
      });
    }
    crate.execute("REFRESH TABLE guestbook.posts").then(() => {
      crate.execute("SELECT p.*, c.name as country, c.geometry as area "
                    + "FROM guestbook.posts AS p, guestbook.countries AS c "
                    + "WHERE within(p.\"user\"['location'], c.geometry) "
                    + "AND p.id = ?", [id]).then((res) => {
        response.status(200).json(res.json);
      });
    });
  });
});

// Change the text of a post
app.put('/post/:id', (req, response) => {
  if (!req.body) {
    return response.json({
      message: 'Please pass a body to the req',
    });
  }
  if (!req.body.text) {
    return response.json({
      message: 'Argument "text" is required',
    });
  }
  crate.execute("UPDATE guestbook.posts SET text=? WHERE id=?",
    [req.body.text, req.params.id]).then((res) => {
    if (res.rowcount === 0) {
      return response.status(404).json({
        message: 'Post not found'
      });
    }
    crate.execute("REFRESH TABLE guestbook.posts").then(() => {
      crate.execute("SELECT p.*, c.name as country, c.geometry as area "
                    + "FROM guestbook.posts AS p, guestbook.countries AS c "
                    + "WHERE within(p.\"user\"['location'], c.geometry) "
                    + "AND p.id = ?", [req.params.id]).then((res) => {
        response.status(200).json(res.json);
      });
    });
  });
});

// Increments the number of likes for a given post
app.put('/post/:id/like', (req, response) => {
  crate.execute("SELECT * FROM guestbook.posts WHERE id=?",
    [req.params.id]).then((res) => {
    if (res.rowcount === 0) {
      return response.status(404).json({
        message: 'Post not found'
      });
    }
    crate.execute("UPDATE guestbook.posts SET like_count = like_count + 1 "
                  + "WHERE id=?", [req.params.id]).then((res) => {
      if (res.rowcount === 0) {
        return response.status(500).json({
          message: 'Update statement went wrong'
        });
      }
      crate.execute("REFRESH TABLE guestbook.posts").then(() => {
        crate.execute("SELECT p.*, c.name as country, c.geometry as area "
                      + "FROM guestbook.posts AS p, guestbook.countries AS c "
                      + "WHERE within(p.\"user\"['location'], c.geometry) "
                      + "AND p.id = ?", [req.params.id]).then((res) => {
          response.status(200).json(res.json[0]);
        });
      });
    });
  });
});

// Delete a post
app.delete('/post/:id', (req, response) => {
  crate.execute('SELECT * FROM guestbook.posts WHERE id = ?',
    [req.params.id]).then((res) => {
    if (res.rowcount === 0) {
      return response.status(404).json({
        message: 'Post not found'
      });
    }
    crate.execute('DELETE FROM guestbook.posts WHERE id=?',
      [req.params.id]).then((res2) => {
      response.status(200).json({
        message: 'Post deleted'
      });
    });
  });
});

// Get all images
app.get('/images', (req, response) => {
  crate.execute("SELECT digest, last_modified FROM blob.guestbook_images")
    .then((res) => {
      response.status(200).json(res.json);
    });
});

// Get image by id
app.get('/image/:digest', (req, response) => {
  let url = 'http://localhost:4200/_blobs/guestbook_images/'
            + req.params.digest;
  request({
    url: url,
    encoding: null
  }, (error, res, body) => {
    if (body.length === 0) {
      return response.json({
        message: 'Digest not found',
      });
    }
    response.writeHead(200, {
        'Content-Type': 'image/gif',
        'Content-Length': body.length
    });
    response.end(new Buffer(body));
  });
});

// Delete image by id
app.delete('/image/:digest', (req, response) => {
  let url = 'http://localhost:4200/_blobs/guestbook_images/'
            + req.params.digest;
  request({
    url: url,
    method: 'DELETE'
  }, (error, res, body) => {
    if (res.statusCode === 204) {
      response.json({
        message: 'Image deleted successfully',
      });
    } else {
      return response.status(500).json({
        message: 'Image deleted successfully',
      });
    }
  });
});

// Upload image
app.post('/images', (req, response) => {
  if (!req.body.blob) {
    response.status(400).json({
      message: 'Argument "blob" is required'
    });
  }

  let data = Buffer.from(req.body.blob, 'base64');
  let digest = sha1(data);

  let url = 'http://localhost:4200/_blobs/guestbook_images/' + digest;

  request({
    headers: {
      'Content-Type': 'image/gif'
    },
    url: url,
    method: 'PUT',
    body: data
  }, (error, res, body) => {
    if (res.statusCode === 201 || res.statusCode === 409) {
      response.status(201).json({
        url: '/image/' + digest,
        digest: digest
      });
    } else {
      console.log(error);
      response.status(500).json({
        message: 'Something went wrong with the upload'
      });
    }
  });
});

// Search
app.post('/search', (req, response) => {
  if (req.body.query_string === "" || !req.body.query_string) {
    return response.status(404).json({
      message: 'Argument "query_string" is required'
    });
  }
  crate.execute("SELECT p.*, p._score as _score, c.name as country, c.geometry "
                + "as area FROM guestbook.posts AS p, guestbook.countries AS c "
                + "WHERE within(p.\"user\"['location'], "
                + "c.geometry) AND match(p.text, ?)",
    [req.body.query_string]).then((res) => {
    response.status(200).json(res.json);
  });
});
