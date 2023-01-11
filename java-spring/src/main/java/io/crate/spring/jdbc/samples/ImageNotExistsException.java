package io.crate.spring.jdbc.samples;

public class ImageNotExistsException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public ImageNotExistsException(String imageDigest) {
        super("Image with digest=\"" + imageDigest + "\" not found");
    }
}