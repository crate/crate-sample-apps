package io.crate.spring.jdbc.samples.da;

import io.crate.spring.jdbc.samples.domain.BlogPost;

public interface JDBCDBObjectsFactory {

    public Object createUserObject(final BlogPost post);
}