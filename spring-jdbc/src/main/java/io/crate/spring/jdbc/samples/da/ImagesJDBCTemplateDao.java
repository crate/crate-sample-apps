package io.crate.spring.jdbc.samples.da;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpRequest.BodyPublishers;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import io.crate.spring.jdbc.samples.domain.Image;

@Component
public class ImagesJDBCTemplateDao implements ImagesDao {

    private static final String IMAGE_TABLE = "guestbook_images";

    @Value("${cratesample.cratedb.blobEndpointURL:http://localhost:4200/_blobs/}")
    private String blobEndpointURL;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    private HttpClient client = HttpClient.newHttpClient();

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

    @Override
    public List<Image> getImages() {
        return this.jdbcTemplate.query(String.format("SELECT digest, last_modified " + "FROM %s " + "ORDER BY 2 DESC",
                String.format("blob.%s", IMAGE_TABLE)), (rs, rowNum) -> stdRowMapper.apply(rs));
    }

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

    private final Function<ResultSet, Image> stdRowMapper = rs -> {

        Image result = new Image();

        try {
            result.setDigest(rs.getString("digest"));
            result.setLast_modified(rs.getDate("last_modified"));
        } catch (SQLException e) {
            return null;
        }

        return result;
    };

    private URI createBlobUri(final String forBlobDigest) {

        return URI.create(this.blobEndpointURL + IMAGE_TABLE + "/" + forBlobDigest);
    }

}