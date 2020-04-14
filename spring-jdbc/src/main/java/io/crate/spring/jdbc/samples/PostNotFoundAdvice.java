package io.crate.spring.jdbc.samples;

import java.util.HashMap;
import java.util.Map;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

@ControllerAdvice
public class PostNotFoundAdvice {

    @ResponseBody
    @ExceptionHandler(PostNotFoundException.class)
    @ResponseStatus(HttpStatus.NOT_FOUND)
    Map<String, Object> postNotFoundHandler(PostNotFoundException ex) {
        Map<String, Object> responseMap = new HashMap<String, Object>();
        responseMap.put("status", HttpStatus.NOT_FOUND);
        responseMap.put("error", ex.getMessage());
        return responseMap;
    }
}