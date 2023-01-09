package io.crate.spring.jdbc.samples.dao;

import io.crate.spring.jdbc.samples.model.BlogPost;

public interface JDBCDBObjectsFactory {

    Object createUserObject(final BlogPost post);
}