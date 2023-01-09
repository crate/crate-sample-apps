package io.crate.spring.jdbc.samples.dao;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.springframework.dao.DataIntegrityViolationException;

import io.crate.shade.org.postgresql.util.PGobject;
import io.crate.spring.jdbc.samples.model.BlogPost;

public class PostgresJDBCDBObjectsFactory implements JDBCDBObjectsFactory {

    private ObjectMapper mapper = new ObjectMapper();

    @Override
    public Object createUserObject(BlogPost post) {
        if (post != null && post.getUser() != null) {
            // objects can be streamed as json strings,
            // https://crate.io/docs/reference/en/latest/protocols/postgres.html#jdbc
            var userObject = new PGobject();
            userObject.setType("json");
            var userValue = new HashMap<String, Object>();
            userValue.put("name", post.getUser().getName());
            var locValues = new ArrayList<Double>();
            locValues.add(post.getUser().getLat());
            locValues.add(post.getUser().getLon());
            userValue.put("location", locValues);
            try {
                userObject.setValue(mapper.writeValueAsString(userValue));
            } catch (JsonProcessingException | SQLException e) {
                throw new DataIntegrityViolationException("Can not create Postgres object for user", e);
            }
            return userObject;
        } else
            return null;
    }
}