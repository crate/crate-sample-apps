package io.crate.spring.jdbc.samples.da;

import com.google.common.hash.Hashing;
import io.crate.spring.jdbc.samples.dao.ImagesDao;
import io.crate.spring.jdbc.samples.model.Image;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Random;

@SpringBootTest
public class ImagesDaoTest {

    @Autowired
    private ImagesDao dao;

    @Test
    public void testImageCrud() throws Exception {
        var data = new byte[10];
        new Random().nextBytes(data);
        var hash = Hashing.sha1().hashBytes(data).toString();

        dao.insertImage(hash, data);
        Assertions.assertTrue(dao.imageExists(hash));
        var result = dao.getImageAsInputStream(hash).readAllBytes();;
        Assertions.assertArrayEquals(data, result);

        List<Image> images = dao.getImages();
        Assertions.assertEquals(1, images.size());
        Assertions.assertEquals(hash, images.get(0).getDigest());

        dao.deleteImage(hash);
        Assertions.assertFalse(dao.imageExists(hash));
    }
}