package io.crate.spring.jdbc.samples.dao;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpRequest.BodyPublishers;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import io.crate.spring.jdbc.samples.model.Image;

@Component
public class ImagesJDBCTemplateDao implements ImagesDao {

    @Value("${cratesample.cratedb.blobEndpointURL:http://localhost:4200/_blobs/}")
    private String blobEndpointURL;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    private HttpClient client = HttpClient.newHttpClient();

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

    @Override
    public List<Image> getImages() {
        return this.jdbcTemplate.query("SELECT digest, last_modified " + "FROM blob.guestbook_images " + "ORDER BY 2 DESC",
                (rs, rowNum) -> new Image(rs.getString("digest"), rs.getDate("last_modified")));
    }

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

    private URI createBlobUri(final String forBlobDigest) {
        return URI.create(blobEndpointURL + "guestbook_images" + "/" + forBlobDigest);
    }
}