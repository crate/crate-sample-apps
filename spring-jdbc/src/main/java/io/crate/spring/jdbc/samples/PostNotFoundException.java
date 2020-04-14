package io.crate.spring.jdbc.samples;

public class PostNotFoundException extends RuntimeException {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    public PostNotFoundException(String forID) {
        super(String.format("Post with id=\"%s\" not found", forID));
    }
}