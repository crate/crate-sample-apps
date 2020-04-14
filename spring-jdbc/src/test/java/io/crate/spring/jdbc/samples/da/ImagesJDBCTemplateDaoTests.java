package io.crate.spring.jdbc.samples.da;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
public class ImagesJDBCTemplateDaoTests {

    @Autowired
    private ImagesDao dao;

    @Test
    public void testImageExist() {

        Assertions.assertTrue(this.dao.imageExists("777e4f867ef3382f02bc7f0fc31dc62dd29e962a"));
    }

    @Test
    public void testImageReading() {

        Assertions.assertNotNull(this.dao.getImageAsInputStream("777e4f867ef3382f02bc7f0fc31dc62dd29e962a"));
    }
}