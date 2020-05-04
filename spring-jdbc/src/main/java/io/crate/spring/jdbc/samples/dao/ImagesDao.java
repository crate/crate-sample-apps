package io.crate.spring.jdbc.samples.dao;

import java.io.InputStream;
import java.util.List;
import java.util.Map;

import io.crate.spring.jdbc.samples.model.Image;

public interface ImagesDao {

    boolean imageExists(String digest);

    List<Image> getImages();

    InputStream getImageAsInputStream(String digest);

    boolean deleteImage(String digest);

    Map<String, String> insertImage(String digest, byte[] decoded);
}