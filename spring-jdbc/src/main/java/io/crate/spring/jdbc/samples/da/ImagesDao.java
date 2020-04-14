package io.crate.spring.jdbc.samples.da;

import java.io.InputStream;
import java.util.List;
import java.util.Map;

import io.crate.spring.jdbc.samples.domain.Image;

public interface ImagesDao {

    public boolean imageExists(String digest);
    public List<Image> getImages();
    public InputStream getImageAsInputStream(String digest);
    public boolean deleteImage(String digest);
    public Map<String, String> insertImage(String digest, byte[] decoded);
}