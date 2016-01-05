# Backend API Spec for Crate Sample App

## Content Type

### Requests
The API uses JSON for requests and responses. This means that all requests that contain a payload (`POST` and `PUT` requests) must have the `Content-Type` and `Accept` headers set to `application/json`.

### Responses
All successful API calls return as content type `application/json`, except the `GET /image/<digest>` request.

```
HTTP/1.1 ...
Content-Length: ...
Content-Type: application/json
...
```

## Posts

### `POST /posts`
Create a new post.

#### Request:

```json
{
  "image_ref": <str | null>,
  "text": <str>,
  "user": {
    "location": [<double>, <double>],
    "name": <str | null>
  }
}
```

#### Response:

```json
[
  {
    "area": {
      "coordinates": [
        [
          [<double>, <double>],
          ...
        ]
      ],
      "type": "Polygon"
    },
    "country": <str>,
    "created": <long>,
    "id": <str>,
    "image_ref": <str | null>,
    "like_count": <int>,
    "text": <str>,
    "user": {
      "location": [<double>, <double>],
      "name": <str | null>
    }
  },
  ...
]
```

Returns HTTP status `201` if the post was successfully created.

### `GET /posts`
Retrieve a list of all posts.

#### Response:

```json
[
  {
    "area": {
      "coordinates": [
        [
          [<double>, <double>],
          ...
        ]
      ],
      "type": "Polygon"
    },
      "country": <str>,
      "created": <long>,
      "id": <str>,
      "image_ref": <str | null>,
      "like_count": <int>,
      "text": <str>,
      "user": {
        "location": [<double>, <double>],
        "name": <str | null>
      }
  },
  ...
]
```

### `PUT /post/<id>`
Update an existing post with given `id`.

#### Request:

```json
{
  "text": <str>
}
```

#### Response:

```json
{
  "area": {
    "coordinates": [
      [
        [<double>, <double>],
        ...
      ]
    ],
    "type": "Polygon"
  },
  "country": <str>,
  "created": <long>,
  "id": <str>,
  "image_ref": <str | null>,
  "like_count": <int>,
  "text": <str>,
  "user": {
    "location": [<double>, <double>],
    "name": <str | null>
  }
}
```

Returns HTTP status `200` if the post was successfully updated.

Returns HTTP status `404` if the post with the given `id` does not exist.

### `GET /post/<id>`
Retrieve a single post with given `id`.

```json
{
  "area": {
    "coordinates": [
      [
        [<double>, <double>],
        ...
      ]
    ],
    "type": "Polygon"
  },
  "country": <str>,
  "created": <long>,
  "id": <str>,
  "image_ref": <str | null>,
  "like_count": <int>,
  "text": <str>,
    "user": {
        "location": [<double>, <double>],
        "name": <str | null>
    }
}
```

Returns HTTP status `200` if the post with given `id` was found.

Returns HTTP status `404` if the post with given `id` does not exist.

### `DELETE /post/<id>`
Delete a post with given `id`.

#### No Response

Returns HTTP status `204` if the post with given `id` was successfully deleted.

Returns HTTP status `404` if the post with given `id` does not exist.

## Like

### `PUT /post/<id>/like`
Increments the like count for a given post by one.

#### Response:

```json
{
    "area": {
        "coordinates": [
            [
                [<double>, <double>],
                ...
            ]
        ],
        "type": "Polygon"
    },
    "country": <str>,
    "created": <long>,
    "id": <str>,
    "image_ref": <str | null>,
    "like_count": <int>,
    "text": <str>,
    "user": {
      "location": [<double>, <double>],
      "name": <str | null>
    }
}
```

Returns HTTP status `200` if the `like_count` of the post with the given `id` was sucessfully incremented.

Returns HTTP status `404` if the post with given `id` does not exist.

## Search

### `POST /search`
Issue a search request to fetch a list of posts whose ``text`` matches a given query string.

#### Request:

```json
{
  "query_string": <str>
}
```

#### Response:

```json
[
  {
    "area": {
      "coordinates": [
        [
          [<double>, <double>],
          ...
        ]
      ],
      "type": "Polygon"
    },
    "country": <str>,
    "created": <long>,
    "id": <str>,
    "image_ref": <str | null>,
    "like_count": <int>,
    "text": <str>,
    "user": {
      "location": [<double>, <double>],
      "name": <str | null>
    }
  },
  ...
]
```

## Images

### `POST /images`
Create/upload a new image.

#### Request:

```json
{
  "blob": <base64 encoded binary data>
}
```

#### Response:

```json
{
  "url": <str>,
  "digest": <str>
}
```

Returns HTTP status `201` if the image was successfully created/uploaded.

The ``url`` points to the URL at which to retrieve the image via this REST API.
It is recommended to only include the path in that url.

Returns HTTP status `409` (Conflict) if a blob with the same digest already exists.

### `GET /images`
Retrieve a list of all available images.

#### Response:

```json
[
  {
    "last_modified": <long>,
    "digest": <str>
  },
  ...
]
```

### `GET /image/<digest>`
Retrieve an image with given `digest`.

Returns a gif (`Content-Type: image/gif`).

```
HTTP/1.1 200 OK
Content-Length: ...
Content-Type: image/gif
<binary-data>
```

Returns HTTP status `200` if the image with given `digest` was found.

The response may be sent with ``Transfer-Encoding: chunked``.

Returns HTTP status `404` if the image with given `digest` does not exist.

### `DELETE /image/<digest>`
Delete an image with given `digest`.

#### No Response

Returns HTTP status `204` if the image with given `digest` was successfully deleted.

Returns HTTP status `404` if the image with given `digest` does not exist.

## Error Responses
Error responses contain an error message: ``error`` and an error code: ``status`` 
(which is the same as the HTTP response status).

```json
{
  "error": <str>,
  "status": <int>
}
```
