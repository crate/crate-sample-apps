# The CrateDB JDBC Sample Application
**Note:** The application is written in Java 8.

## Installation
Staring from 0.57.0, CrateDB supports for the [PostgreSQL wire protocol v3][1].
To get more detail information on the protocol support by Crate,
please have a look at the [documentation][2].

In the sample application, we use the [CrateDB JDBC driver 2.0.x][3] which
uses [PostgreSQL wire protocol v3][1]. To obtain the [CrateDB JDBC driver][4],
please follow [instructions][5] for the build tool of your choice.


## Usage

### Connecting to CrateDB

The application establishes the connection with CrateDB using the
`DriverManager.getConnection()` method.

```java
Connection connection = DriverManager.getConnection("jdbc:crate://<host>:5432/");
```

Please, take a look at the CrateDB JDBC driver documentation to see the
[possible forms][6] of URLs supported by the driver.

### Executing statements
#### Executing single statement
The `createStatement()` method creates a single `Statement` object for
sending SQL statements to a database. We can execute the statement by
calling `execute()` on the created statement. Each method call returns
`true` if the first result in `ResultSet` is an object or `false` if the
result is empty:

```java
boolean created = connection.createStatement().execute(
    "CREATE TABLE guestbook.posts (" +
    "    id STRING, " +
    "    text STRING INDEX USING FULLTEXT WITH (analyzer='english'), " +
    "    user OBJECT(strict) AS ( " +
    "        name STRING, " +
    "        location GEO_SHAPE " +
    "    ), " +
    "    image_ref STRING)"
);
```

To send parameterized SQL queries to the database, create a
`PreparedStatement` object and set designated parameters of the
statement to their corresponding values.

`PreparedStatement` is used in the sample application when we insert
a new blog post entry into Crate:

```java
...
PreparedStatement statement = connection.prepareStatement(
    "INSERT INTO guestbook.posts (id, user, text, image_ref, created, like_count) " +
    "VALUES (?, ?, ?, ?, ?, ?)");
String id = UUID.randomUUID().toString();
statement.setString(1, id);

// objects can be streamed as json strings,
// https://crate.io/docs/reference/en/latest/protocols/postgres.html#jdbc
PGobject userObject = new PGobject();
userObject.setType("json");
userObject.setValue(gson.toJson(post.get("user")));
statement.setObject(2, userObject);

statement.setString(3, (String) post.get("text"));
statement.setString(4, (String) post.get("image_ref"));
statement.setLong(5, System.currentTimeMillis());
statement.setLong(6, 0);
statement.executeUpdate();
...
```

The `executeUpdate()` method executes a SQL statement which must be one
of the Data Manipulation Language (DML) statements, such as `INSERT`,
`UPDATE` or `DELETE`.

#### Executing multiple statements (bulk operations)
It's also possible to use the parameter substitution in multiple
parameterized statements at once.

It can be done by adding multiple statements to the batch and
executing them with the `executeBatch()` method. For instance:

```java
...
PreparedStatement statement = connection.prepareStatement(
    "UPDATE guestbook.posts " +
    "SET text = ? " +
    "WHERE id = ?");

    statement.setString(1, "record");
    statement.setString(2, 1);
    statement.addBatch();

    statement.setString(1, "another record");
    statement.setString(2, 2);
    statement.addBatch();

    statement.setString(1, "yet another record");
    statement.setString(2, 3);
    statement.addBatch();

    int[] results = statement.executeBatch();
    ...
```

The resulting array contains one element for each command in the batch.
The elements of the array are set in the order in which the statements
were added to the batch.

### Fetching query results
The `executeQuery()` method executes a prepared query and returns
a `ResultSet` object. The data in `ResultSet` can be accessed through
a cursor. Initially, the cursor points on the first row.

Using the sample application as an example, we can demonstrate the above
described operations. The following code is a part of the application
logic which build a map from the result set:

```java
public Map getPost(String id) throws SQLException {
    PreparedStatement statement = connection.prepareStatement(
        "SELECT p.*, c.name as country, c.geometry as area " +
        "FROM guestbook.posts AS p, guestbook.countries AS c " +
        "WHERE within(p.user['location'], c.geometry) " +
        "AND p.id = ?");

    statement.setString(1, id);
    ResultSet resultSet = statement.executeQuery();
    return resultSetToMap.apply(resultSet);
}

private final CheckedFunction<ResultSet, Map<String, Object>> resultSetToMap = rs -> {
    PgResultSetMetaData metaData = (PgResultSetMetaData) rs.getMetaData();
    int resultSetSize = metaData.getColumnCount();

    Map<String, Object> map = new HashMap<>(resultSetSize);
    for (int i = 1; i <= resultSetSize; i++) {
        Object value = rs.getObject(i);
        if (metaData.getColumnTypeName(i).equals("json")) {
            map.put(metaData.getColumnName(i), gson.fromJson((String) value, Map.class));
         } else {
            map.put(metaData.getColumnName(i), value);
         }
    }
    return map;
};
```

### Handling BLOBs
CrateDB does not support handling of BLOBs via the JDBC driver. Therefore,
in the sample application we use the CrateDB [RESTful BLOB API][7] and
[Apache HTTP components][8] library to handle uploading, removing
and retrieving BLOB data.

To upload a blob, the _sha1_ hash of the blob needs to be known as this
will be used as the `id` of the new blob. In the app we decode the
Base64 encoded string from the JSON payload which contains the blob
into a newly allocated byte array and compute its _sha1_ digest:

```java
   post("/images", (request, response) -> {
       ...
       byte[] decoded = Base64.getDecoder().decode(blobMap.get("blob"));
       String digest = DigestUtils.shaHex(decoded);
       ...
   }, gson::toJson);
```

The blob can now be uploaded by issuing the `HttpPut` request where
`body` is the decoded _Base64_ encoded string:

```java
   public Map<String, String> insertBlob(String digest, byte[] body) throws IOException {
       String uri = blobUri(digest);
       HttpPut put = new HttpPut(uri);
       if (body != null) {
           put.setEntity(new ByteArrayEntity(body));
       }
       CloseableHttpResponse response = httpClient.execute(put);
       ...
   }
```

If a blob already exists with the given hash the
'HTTP status code 409 Conflict' error message is returned.

To download a blob use the `HttpGet` request:

```java
   public CloseableHttpResponse getBlob(String digest) throws IOException {
       HttpGet get = new HttpGet(blobUri(digest));
       return httpClient.execute(get);
   }
```

To determine if a blob exists without downloading it use the `HttpHead`
request:

```java
   public boolean blobExists(String digest) throws IOException {
       HttpHead head = new HttpHead(blobUri(digest));
       CloseableHttpResponse response = httpClient.execute(head);
       ...
   }
```

To delete a blob use the `HttpDelete` request:

```java
   public CloseableHttpResponse deleteBlob(String digest) throws IOException {
       HttpDelete delete = new HttpDelete(blobUri(digest));
       return httpClient.execute(delete);
  }
```

For all HTTP PUT, DELETE, and GET requests in the application the URI
should have the following format:

```
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
