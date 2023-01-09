package io.crate.spring.jdbc.samples;

import java.util.Map;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

@ControllerAdvice
public class ArgumentRequiredAdvice {

    @ResponseBody
    @ExceptionHandler(ArgumentRequiredException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    Map<String, Object> postNotFoundHandler(ArgumentRequiredException ex) {
        return Map.of("status", HttpStatus.BAD_REQUEST, "error", ex.getMessage());
    }

}