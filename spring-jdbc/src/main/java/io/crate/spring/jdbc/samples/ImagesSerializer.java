package io.crate.spring.jdbc.samples;

import java.util.HashMap;
import java.util.Map;

import org.springframework.lang.NonNull;

import io.crate.spring.jdbc.samples.domain.Image;

public class ImagesSerializer {

    public Map<String, Object> serialize(@NonNull final Image image) {

        Map<String, Object> imageProps = new HashMap<String, Object>();


        return imageProps;
    }
}