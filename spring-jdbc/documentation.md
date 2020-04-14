# The Crate JDBC Sample Application

**Note:** The application is written in Java 11.

## Installation

Staring from 0.57.0 Crate supports for the [PostgreSQL wire protocol v3][1].
To get more detail information on the protocol support by Crate,
please have a look at the [documentation][2].

In the sample application, we use the [Crate JDBC driver 2.1.7][3] which
uses [PostgreSQL wire protocol v3][1]. To obtain the [Crate JDBC driver][4],
please follow [instructions][5] for the build tool of your choice.

## Usage

### Connecting to Crate

The application establishes the connection with Crate using a Spring JDBC datasource configured in the `application.properties` file:

```text
spring.datasource.url=jdbc:crate://localhost:5432/
spring.datasource.username=crate
spring.datasource.password=
spring.datasource.driver-class-name=io.crate.client.jdbc.CrateDriver
```

Please, take a look at the Crate JDBC driver documentation to see the
[possible forms][6] of URLs supported by the driver.

### Executing statements

For all statements Spring's JDBC template is used. E.g reading a blog post from CrateDB:

```java
    public Optional<BlogPost> getPost(@NonNull String id) {

        List<BlogPost> queryResult = this.jdbcTemplate.query(String.format(
                "SELECT p.id as id, p.text as text, "
                        + "p.\"user\"['name'] as username, p.\"user\"['location'] as userlocation, "
                        + "p.created as created, c.name as country, c.geometry as area, "
                        + "p.image_ref as imageRef, p.like_count as likes " + "FROM %s AS p, %s AS c "
                        + "WHERE within(p.\"user\"['location'], c.geometry) " + "AND p.id = ?",
                POST_TABLE, COUNTRIES_TABLE), (rs, rowNum) -> stdRowMapper.apply(rs), id);

        return queryResult != null && queryResult.size() == 1 ? Optional.of(queryResult.get(0)) : Optional.empty();
    }
```

### Handling BLOBs

Crate does not support handling of BLOBs via the JDBC driver. Therefore,
in the sample application we use the Crate [RESTful BLOB API][7] and
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
        if (imageProps == null)
            throw new ArgumentRequiredException("Request body is required");
        else if (!imageProps.containsKey("blob"))
            throw new ArgumentRequiredException("Argument \"blob\" is required");

        byte[] decodedBytes = Base64.getDecoder().decode((String)imageProps.get("blob"));
        String digest = DigestUtils.sha1Hex(decodedBytes);

        Map<String, String> responseMap = this.dao.insertImage(digest, decodedBytes);
        response.setStatus(Integer.parseInt(responseMap.get("status")));

        return responseMap;
    }
```

The blob can now be uploaded by issuing a HTTP PUT request where
body is the decoded _Base64_ encoded string:

```java
    @Override
    public Map<String, String> insertImage(final String digest, byte[] decoded) {

        var request = HttpRequest.newBuilder(this.createBlobUri(digest)).
            PUT(BodyPublishers.ofByteArray(decoded)).build();

        try {
            var response = client.send(request, HttpResponse.BodyHandlers.ofString());
            Map<String, String> result = new HashMap<String, String>();
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

        InputStream result = null;

        var request = HttpRequest.newBuilder(this.createBlobUri(digest)).GET().build();
        try {
            HttpResponse<InputStream> response = client.send(request, HttpResponse.BodyHandlers.ofInputStream());
            result = response.body();
        } catch (IOException | InterruptedException e) {
            throw new DataIntegrityViolationException("Failed to call blob endpoint", e);
        }

        return result;
    }
```

To determine if a blob exists without downloading it use the HTTP HEAD
request:

```java
    @Override
    public boolean imageExists(@NonNull final String digest) {

        boolean result = false;

        var request = HttpRequest.newBuilder(this.createBlobUri(digest))
                .method("HEAD", HttpRequest.BodyPublishers.noBody()).build();

        try {
            HttpResponse<Void> response = client.send(request, HttpResponse.BodyHandlers.discarding());
            result = response.statusCode() == HttpStatus.OK.value();
        } catch (IOException | InterruptedException e) {
            throw new DataIntegrityViolationException("Failed to call blob endpoint", e);
        }

        return result;
    }
```

To delete a blob use the HTTP DELETE request:

```java
    @Override
    public boolean deleteImage(@NonNull final String digest) {

        boolean result = false;

        var request = HttpRequest.newBuilder(this.createBlobUri(digest)).DELETE().build();
        try {
            var response = client.send(request, HttpResponse.BodyHandlers.ofString());
            result = response.statusCode() == HttpStatus.OK.value();
        } catch (IOException | InterruptedException e) {
            throw new DataIntegrityViolationException("Failed to call blob endpoint", e);
        }

        return result;
    }
```

For all HTTP PUT, DELETE, and GET requests in the application the URI
should have the following format:

```html
http://localhost:4200/_blobs/guestbook_images/<digest>
```

If a blob does not exist an HTTP status code 404 Not Found is returned.

[1]: https://www.postgresql.org/docs/current/static/protocol.html
[2]: https://crate.io/docs/reference/en/latest/protocols/postgres.html
[3]: https://crate.io/docs/reference/jdbc/en/latest/
[4]: https://crate.io/docs/clients/jdbc/
[5]: https://bintray.com/crate/crate/crate-jdbc/view
[6]: https://crate.io/docs/reference/jdbc/en/latest/#jdbc-url-format
[7]: https://crate.io/docs/reference/blob.html
[8]: https://hc.apache.org/httpcomponents-client-ga
