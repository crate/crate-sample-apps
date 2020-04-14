package io.crate.spring.jdbc.samples;

import java.util.HashMap;
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
        Map<String, Object> responseMap = new HashMap<String, Object>();
        responseMap.put("status", HttpStatus.BAD_REQUEST);
        responseMap.put("error", ex.getMessage());
        return responseMap;
    }

}