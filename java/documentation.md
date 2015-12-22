# Getting Started with Crate JDBC

## Installation

The Crate.IO JDBC driver is hosted on [Bintray Repository](https://bintray.com/crate/crate/crate-jdbc/view)
and available via [JCenter](https://bintray.com/bintray/jcenter).
In order to use the driver in your Maven project, you should add the Bintray repository and `crate-jdbc`
dependency to the `pom.xml` file.

#### Maven

```xml
...
<repositories>
    ...
    <repository>
        <snapshots>
            <enabled>false</enabled>
        </snapshots>
        <id>central</id>
        <name>bintray</name>
        <url>http://dl.bintray.com/crate/crate</url>
    </repository>
</repositories>
...
<dependencies>
    ...
    <dependency>
        <groupId>io.crate</groupId>
        <artifactId>crate-jdbc</artifactId>
        <version>...</version>
    </dependency>
</dependencies>
...
```

To use `crate-jdbc` with Gradle build tool, add the following configuration to the Gradle file.

#### Gradle

```gradle
   repositories {
        ...
        jcenter()
    }

    dependencies {
        compile 'io.crate:crate-jdbc:...'
        ...
    }
```

Alternatively, the [Maven](https://maven.apache.org/) and [Gradle](http://gradle.org/) build configuration
for the driver can be obtained by clicking the _Set Me Up_ button on the Bintray drivers page.

Additionally, there is also a standalone version of the Crate.IO JDBC driver
named `crate-jdbc-standalone` which already includes its dependencies.
You can download the latest standalone version directly from the
[Bintray Repository](<https://bintray.com/crate/crate/crate-jdbc/view/files/io/crate/crate-jdbc-standalone>).

## Usage

### Connecting to Crate Server

The Crate JDBC driver class is `io.crate.client.jdbc.CrateDriver`. The following line of code loads the driver:

```java
    Class.forName("io.crate.client.jdbc.CrateDriver");
```

After the driver is loaded, you can establish a connection using the `DriverManager.getConnection()` method.

```java
     Connection connection = DriverManager.getConnection("crate://localhost:4300");
```

The `getConnection()` method always requires a database URL parameter. The database URL for `crate-jdbc`
can take one of the following forms:

    [jdbc:]crate://<host>:<transport-port>[,<host>:<transport-port> , ...][/<schemaName>]

To connect to multiple Crate servers, list `host` and `transport-port` pairs and delimit them by comma:

    jdbc:crate://host1.example.com:4300,host2.example.com:4300

The `jdbc:` prefix is optional and can be omitted.

### Executing statements

#### Executing single statement

The `createStatement()` method creates a single `Statement` object for sending SQL statements to the database.
We can execute the statement by calling `execute()` method on the statement. Each method call returns true if
the first result in `ResultSet` is an object or false if it is an update count or there are no result:

```java
boolean created = connection.createStatement().execute(
    "CREATE TABLE guestbook.posts (" +
    "    id STRING, " +
    "    text STRING INDEX USING FULLTEXT WITH (analyzer='english'), " +
    "    user OBJECT('strict') AS ( " +
    "        name STRING, " +
    "        location GEO_SHAPE " +
    "    ), " +
    "    image_ref STRING)"
);
```

In order to send parameterized SQL queries to the database, a `PreparedStatement` object should be created
and designated parameters of the statement must be set to their corresponding values.

One of the cases in the Crate JDBC sample application where a prepared statement used is the
insertion of a new blog post entry to the database:

```java
...
PreparedStatement statement = connection.prepareStatement(
    "INSERT INTO guestbook.posts (id, user, text, image_ref, created, like_count) " +
    "VALUES (?, ?, ?, ?, ?, ?)");
    String id = UUID.randomUUID().toString();
    statement.setString(1, id);
    statement.setObject(2, post.get("user"));
    statement.setString(3, post.get("text"));
    statement.setString(4, post.get("image_ref"));
    statement.setLong(5, System.currentTimeMillis());
    statement.setLong(6, 0);
    statement.executeUpdate();
    ...
```

The `executeUpdate()` method executes the SQL statement in a `PreparedStatement` object which must be an
SQL Data Manipulation Language (DML) statement, such as `INSERT`, `UPDATE` or `DELETE`.

#### Executing multiple statements (bulk operations)

The same way as it is possible to use parameter substitution in a single execute statement it is also
possible to execute multiple parameterized statements at once.

This can be done by adding multiple statements to the batch and executing it with the `executeBatch()` method.

Let's have a look how it would look if we want to update few records at once:

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

The resulting array contains one element for each command in the batch. The elements of the array are set in order
in which the statements were added to the batch.

### Fetching query results

The `executeQuery()` method executes the SQL query in the `PreparedStatement` object
and returns the `ResultSet` object generated by the query. The `ResultSet` object represents a database result set.
The data in `ResultSet` can be accessed through a cursor. Initially, the cursor points before the first row.

As an example from the sample application, we can demonstrate the call of `executeQuery()` on
`PreparedStatement`, extracting metadata from the `ResultSet` object and iterating through the data in it, in order
to build a map which represent a single record obtained from the database.

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

private final Function<ResultSet, Map> resultSetToMap = rs -> {
    ResultSetMetaData rsMetaData = rs.getMetaData();
    int columns = rsMetaData.getColumnCount();
    HashMap<String, Object> map = new HashMap<>();
    while (rs.next()) {
        for (int i = 1; i <= columns; i++) {
            map.put(rsMetaData.getColumnName(i), rs.getObject(i));
        }
    }
    return map;
};
```

### Handling BLOBs

The Crate.IO JDBC Driver does not support operations with BLOBs. Therefore, in the sample
application we use Crate.IO [RESTful BLOB API](https://crate.io/docs/reference/blob.html)
and [Apache HTTP components](https://hc.apache.org/httpcomponents-client-ga/) library to handle uploading,
removing and retrieving BLOB data.

1. To upload a blob, the _sha1_ hash of the blob has to be known upfront since this has
to be used as the id of the new blob. In the app we decode the Base64 encoded string from the
JSON payload which contains blob into a newly allocated byte array and compute its _sha1_ digest:

    ```java
    post("/images", (request, response) -> {
        ...
        byte[] decoded = Base64.getDecoder().decode(blobMap.get("blob"));
        String digest = DigestUtils.shaHex(decoded);
        ...
    }, gson::toJson);
    ```

2. The blob can now be uploaded by issuing a `HttpPut` request where _body_ is the
decoded _Base64_ encoded string:

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

    If a blob already exists with the given hash HTTP status code 409 Conflict is returned:

3. To download a blob use `HttpGet` request:

    ```java
    public CloseableHttpResponse getBlob(String digest) throws IOException {
        HttpGet get = new HttpGet(blobUri(digest));
        return httpClient.execute(get);
    }
    ```
5. To determine if a blob exists without downloading it use `HttpHead` request:

    ```java
    public boolean blobExists(String digest) throws IOException {
    	HttpHead head = new HttpHead(blobUri(digest));
    	CloseableHttpResponse response = httpClient.execute(head);
    	...
    }
    ```
4. To delete a blob use `HttpDelete` request:

    ```java
    public CloseableHttpResponse deleteBlob(String digest) throws IOException {
        HttpDelete delete = new HttpDelete(blobUri(digest));
        return httpClient.execute(delete);
   }
    ```

For all HTTP PUT, DELETE, and GET requests in the application the URI would have the following
format:

```
http://localhost:4200/_blobs/guestbook_images/<digest>
```

If a blob does not exist HTTP status code 404 Not Found is returned.

**Note:** The application is written in Java 8.
