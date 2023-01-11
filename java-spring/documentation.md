# The CrateDB JDBC Sample Application

**Note:** The application is written in Java 11.

## Installation

In this sample application, we use the [CrateDB JDBC driver][3], which
uses [PostgreSQL wire protocol v3][1]. To obtain the [CrateDB JDBC driver][4],
please follow [instructions][5] for the build tool of your choice.

## Usage

### Connecting to Crate

The application establishes the connection with CrateDB using a Spring JDBC datasource configured in the `application.properties` file:

```text
spring.datasource.url=jdbc:crate://localhost:5432/
spring.datasource.username=crate
spring.datasource.password=
spring.datasource.driver-class-name=io.crate.client.jdbc.CrateDriver
```

Please, take a look at the CrateDB JDBC driver documentation to see the
[possible forms][6] of URLs supported by the driver.

### Executing statements

For all statements Spring's JDBC template is used. E.g reading a blog post from CrateDB:

```java
    public Optional<BlogPost> getPost(@NonNull String id) {
        List<BlogPost> queryResult = this.jdbcTemplate.query(
                "SELECT p.id as id, p.text as text, "
                        + "p.\"user\"['name'] as username, p.\"user\"['location'] as userlocation, "
                        + "p.created as created, c.name as country, c.geometry as area, "
                        + "p.image_ref as imageRef, p.like_count as likes " + "FROM guestbook.posts AS p, guestbook.countries AS c "
                        + "WHERE within(p.\"user\"['location'], c.geometry) " + "AND p.id = ?"
                , blogPostRowMapper, id);

        return queryResult.size() == 1 ? Optional.of(queryResult.get(0)) : Optional.empty();
    }
```

### Handling BLOBs

CrateDB does not support handling of BLOBs via the JDBC driver. Therefore,
in the sample application we use the CrateDB [RESTful BLOB API][7] and
Java's Http Client class to handle uploading, removing
and retrieving BLOB data.

To upload a blob, the _sha1_ hash of the blob needs to be known as this
will be used as the `id` of the new blob. In the app we decode the
Base64 encoded string from the JSON payload which contains the blob
into a newly allocated byte array and compute its _sha1_ digest:

```java
    @PostMapping("/images")
    public Map<String, String> insertImage(@RequestBody Map<String, Object> imageProps, HttpServletResponse response) {
        logger.debug("Inserting image into database");
        if (imageProps == null) {
            throw new ArgumentRequiredException("Request body is required");
        } else if (!imageProps.containsKey("blob")) {
            throw new ArgumentRequiredException("Argument \"blob\" is required");
        }

        var decodedBytes = Base64.getDecoder().decode((String) imageProps.get("blob"));
        var digest = DigestUtils.sha1Hex(decodedBytes);

        var responseMap = dao.insertImage(digest, decodedBytes);
        response.setStatus(Integer.parseInt(responseMap.get("status")));
        return responseMap;
    }
```

The blob can now be uploaded by issuing a HTTP PUT request where
body is the decoded _Base64_ encoded string:

```java
    @Override
    public Map<String, String> insertImage(final String digest, byte[] decoded) {
        var blobUri = this.createBlobUri(digest);
        var request = HttpRequest.newBuilder(blobUri).
                PUT(BodyPublishers.ofByteArray(decoded)).build();
        try {
            var response = client.send(request, HttpResponse.BodyHandlers.ofString());
            var result = new HashMap<String, String>();
            result.put("digest", digest);
            result.put("url", "/image/" + digest);
            result.put("status", String.valueOf(response.statusCode()));
            return result;
        } catch (IOException | InterruptedException e) {
            throw new DataIntegrityViolationException("Failed to call blob endpoint", e);
        }
    }
```

If a blob already exists with the given hash the
'HTTP status code 409 Conflict' error message is returned.

To download a blob use the HTTP GET request:

```java
    @Override
    public InputStream getImageAsInputStream(final String digest) {
        var request = HttpRequest.newBuilder(this.createBlobUri(digest)).GET().build();
        try {
            var response = client.send(request, HttpResponse.BodyHandlers.ofInputStream());
            return response.body();
        } catch (IOException | InterruptedException e) {
            throw new DataIntegrityViolationException("Failed to call blob endpoint", e);
        }
    }
```

To determine if a blob exists without downloading it use the HTTP HEAD
request:

```java
    @Override
    public boolean imageExists(@NonNull final String digest) {
        var request = HttpRequest
                .newBuilder(this.createBlobUri(digest))
                .method("HEAD", HttpRequest.BodyPublishers.noBody())
                .build();
        try {
            var response = client.send(request, HttpResponse.BodyHandlers.discarding());
            return response.statusCode() == HttpStatus.OK.value();
        } catch (IOException | InterruptedException e) {
            throw new DataIntegrityViolationException("Failed to call blob endpoint", e);
        }
    }
```

To delete a blob use the HTTP DELETE request:

```java
    @Override
    public boolean deleteImage(@NonNull final String digest) {
        var request = HttpRequest.newBuilder(this.createBlobUri(digest)).DELETE().build();
        try {
            var response = client.send(request, HttpResponse.BodyHandlers.ofString());
            return response.statusCode() == HttpStatus.OK.value();
        } catch (IOException | InterruptedException e) {
            throw new DataIntegrityViolationException("Failed to call blob endpoint", e);
        }
    }
```

For all HTTP PUT, DELETE, and GET requests in the application the URI
should have the following format:

```html
http://localhost:4200/_blobs/guestbook_images/<digest>
```

If a blob does not exist, an HTTP status code `404 Not Found` is returned.

[1]: https://www.postgresql.org/docs/current/static/protocol.html
[2]: https://crate.io/docs/reference/en/latest/protocols/postgres.html
[3]: https://crate.io/docs/reference/jdbc/en/latest/
[4]: https://crate.io/docs/clients/jdbc/
[5]: https://mvnrepository.com/artifact/io.crate/crate-jdbc/latest
[6]: https://crate.io/docs/reference/jdbc/en/latest/#jdbc-url-format
[7]: https://crate.io/docs/reference/blob.html
[8]: https://hc.apache.org/httpcomponents-client-ga
