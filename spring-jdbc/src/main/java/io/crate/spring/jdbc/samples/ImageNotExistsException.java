package io.crate.spring.jdbc.samples;

public class ImageNotExistsException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public ImageNotExistsException(String imageDigest) {
        super("Blob with digest '" + imageDigest + "' does not exist");
    }
}