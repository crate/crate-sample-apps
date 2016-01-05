package io.crate.jdbc.sample;

import io.crate.shade.com.google.common.collect.ImmutableList;
import io.crate.shade.com.google.common.collect.ImmutableMap;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpHead;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;

import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;

public class DataProvider {

    private static final String POST_TABLE = "guestbook.posts";
    private static final String COUNTRIES_TABLE = "guestbook.countries";
    private static final String IMAGE_TABLE = "guestbook_images";

    private static String host = "localhost";
    private static int transportPort = 4300;
    private static int httpPort = 4200;

    private CloseableHttpClient httpClient = HttpClients.createSystem();
    private Connection connection;

    public DataProvider() throws SQLException {
        try {
            Class.forName("io.crate.client.jdbc.CrateDriver");
            String hostAndPort = String.format(Locale.ENGLISH, "%s:%d", host, transportPort);
            connection = DriverManager.getConnection("crate://" + hostAndPort);
        } catch (SQLException | ClassNotFoundException e) {
            throw new SQLException("Cannot connect to the database", e);
        }
    }

    public List<Map<String, Object>> getPosts() throws SQLException {
        PreparedStatement statement = connection.prepareStatement(String.format(
                "SELECT p.*, c.name as country, c.geometry as area " +
                "FROM %s AS p, %s AS c " +
                "WHERE within(p.user['location'], c.geometry)" +
                "ORDER BY p.created DESC", POST_TABLE, COUNTRIES_TABLE));
        ResultSet rs = statement.executeQuery();
        return resultSetToListOfMaps(rs);
    }

    private List<Map<String, Object>> resultSetToListOfMaps(ResultSet rs) throws SQLException {
        List<Map<String, Object>> posts = new ArrayList<>();
        while (rs.next()) {
            posts.add(resultSetToMap.apply(rs));
        }
        return posts;
    }

    private final CheckedFunction<ResultSet, Map<String, Object>> resultSetToMap = rs -> {
        ResultSetMetaData rsMetaData = rs.getMetaData();
        int numCols = rsMetaData.getColumnCount();
        HashMap<String, Object> map = new HashMap<>();
        for (int i = 1; i <= numCols; i++) {
            map.put(rsMetaData.getColumnName(i), rs.getObject(i));
        }
        return map;
    };

    public Map<String, Object> getPost(String id) throws SQLException {
        PreparedStatement statement = connection.prepareStatement(String.format(
                "SELECT p.*, c.name as country, c.geometry as area " +
                "FROM %s AS p, %s AS c " +
                "WHERE within(p.user['location'], c.geometry) " +
                "AND p.id = ?", POST_TABLE, COUNTRIES_TABLE));
        statement.setString(1, id);
        ResultSet results = statement.executeQuery();
        if (results.next()) {
            return resultSetToMap.apply(results);
        } else {
            return ImmutableMap.of();
        }
    }

    public List<Map<String, Object>> insertPost(Map<String, Object> post) throws SQLException {
        PreparedStatement statement = connection.prepareStatement(String.format(
                "INSERT INTO %s " +
                "(id, user, text, image_ref, created, like_count) " +
                "VALUES (?, ?, ?, ?, ?, ?)", POST_TABLE));
        String id = UUID.randomUUID().toString();
        statement.setString(1, id);
        statement.setObject(2, post.get("user"));
        statement.setObject(3, post.get("text"));
        statement.setObject(4, post.get("image_ref"));
        statement.setLong(5, System.currentTimeMillis());
        statement.setLong(6, 0);
        if (statement.executeUpdate() == 0) {
            return ImmutableList.of();
        }
        connection.createStatement()
                .execute(String.format("REFRESH TABLE %s", POST_TABLE));
        return ImmutableList.of(getPost(id));
    }

    public Map<String, Object> updatePost(String id, String val) throws SQLException {
        PreparedStatement statement = connection.prepareStatement(String.format(
                "UPDATE %s " +
                "SET text = ? " +
                "WHERE id = ?", POST_TABLE));
        statement.setString(1, val);
        statement.setString(2, id);
        if (statement.executeUpdate() == 0) {
            return ImmutableMap.of();
        }
        connection.createStatement()
                .execute(String.format("REFRESH TABLE %s", POST_TABLE));
        return getPost(id);
    }

    public Map<String, Object> incrementLike(String id) throws SQLException {
        PreparedStatement statement = connection.prepareStatement(String.format(
                "UPDATE %s " +
                "SET like_count = like_count + 1 " +
                "WHERE id = ?", POST_TABLE));
        statement.setString(1, id);
        if (statement.executeUpdate() == 0) {
            return ImmutableMap.of();
        }
        connection.createStatement()
                .execute(String.format("REFRESH TABLE %s", POST_TABLE));
        return getPost(id);
    }

    public boolean deletePost(String id) throws SQLException {
        PreparedStatement statement = connection.prepareStatement(String.format(
                "DELETE FROM %s " +
                "WHERE id = ?", POST_TABLE));
        statement.setString(1, id);
        return statement.executeUpdate() == 1;
    }

    public List<Map<String, Object>> getBlobs() throws SQLException {
        ResultSet rs = connection.createStatement().executeQuery(String.format(
                "SELECT digest, last_modified " +
                "FROM %s " +
                "ORDER BY 2 DESC", String.format("blob.%s", IMAGE_TABLE)));
        return resultSetToListOfMaps(rs);
    }

    public CloseableHttpResponse getBlob(String digest) throws IOException {
        HttpGet get = new HttpGet(blobUri(digest));
        return httpClient.execute(get);
    }

    public Map<String, String> insertBlob(String digest, byte[] body) throws IOException {
        String uri = blobUri(digest);
        HttpPut put = new HttpPut(uri);
        if (body != null) {
            put.setEntity(new ByteArrayEntity(body));
        }
        CloseableHttpResponse response = httpClient.execute(put);
        return ImmutableMap.of(
                "digest", digest,
                "url", "/image/" + digest,
                "status", String.valueOf(response.getStatusLine().getStatusCode())
        );
    }

    public CloseableHttpResponse deleteBlob(String digest) throws IOException {
        HttpDelete delete = new HttpDelete(blobUri(digest));
        return httpClient.execute(delete);
    }

    public boolean blobExists(String digest) throws IOException {
        HttpHead head = new HttpHead(blobUri(digest));
        CloseableHttpResponse response = httpClient.execute(head);
        return response.getStatusLine().getStatusCode() == 200;
    }

    private String blobUri(String digest) {
        return String.format(Locale.ENGLISH,
                "http://%s:%s/_blobs/%s", host, httpPort, blobResourceUri(IMAGE_TABLE, digest));
    }

    private String blobResourceUri(String index, String digest) {
        return String.format(Locale.ENGLISH, "%s/%s", index, digest);
    }

    public List<Map<String, Object>> searchPosts(String query) throws SQLException {
        PreparedStatement statement = connection.prepareStatement(String.format(
                "SELECT p.*, p._score as _score, c.name as country, c.geometry as area " +
                "FROM %s AS p, %s AS c " +
                "WHERE within(p.user['location'], c.geometry)" +
                "AND match(text, ?) " +
                "ORDER BY _score DESC", POST_TABLE, COUNTRIES_TABLE));
        statement.setString(1, query);
        ResultSet results = statement.executeQuery();
        return resultSetToListOfMaps(results);
    }

    @FunctionalInterface
    public interface CheckedFunction<T, R> {
        R apply(T t) throws SQLException;
    }

}
