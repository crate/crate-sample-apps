package io.crate.spring.jdbc.samples;

import java.util.Map;

import org.springframework.lang.NonNull;

import io.crate.spring.jdbc.samples.model.Image;

public class ImagesSerializer {

    public Map<String, Object> serialize(@NonNull final Image image) {
        return Map.of("digest", image.getDigest(), "last_modified", image.getLastModified());
    }
}