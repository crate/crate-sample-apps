package io.crate.spring.jdbc.samples;

public class ArgumentRequiredException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public ArgumentRequiredException(String withMsg) {
        super(withMsg);
    }
}