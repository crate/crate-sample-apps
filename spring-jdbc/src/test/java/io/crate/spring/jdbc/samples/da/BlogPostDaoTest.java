package io.crate.spring.jdbc.samples.da;

import java.util.Date;
import java.util.UUID;

import io.crate.spring.jdbc.samples.dao.BlogPostDao;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import io.crate.spring.jdbc.samples.model.BlogPost;
import io.crate.spring.jdbc.samples.model.User;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotNull;


@SpringBootTest
public class BlogPostDaoTest {

    @Autowired
    private BlogPostDao dao;

    @Test
    public void testPostCrudOperations() {
        BlogPost newPost = new BlogPost();
        String id = UUID.randomUUID().toString();
        newPost.setId(id);
        newPost.setText("The force will be with you");
        User anUser = new User("Darth Vader", 16.3552942, 48.32648349999999);
        newPost.setUser(anUser);
        newPost.setCreated(new Date());

        var result = dao.insertPost(newPost);

        assertTrue(result.isPresent());
        assertEquals(newPost.getId(), result.get().getId());
        assertEquals(newPost.getText(), result.get().getText());
        assertEquals(newPost.getUser().getName(), result.get().getUser().getName());

        var posts = dao.getPosts();
        assertNotNull(posts);
        assertEquals(1, posts.size());

        var updatedText = "Your powers are weak, old man";
        result = dao.updatePostText(id, updatedText);
        assertTrue(result.isPresent());
        assertEquals(updatedText, result.get().getText());

        result = dao.incrementLikeCount(id);
        assertTrue(result.isPresent());
        assertEquals(1, result.get().getLikeCount());

        assertTrue(dao.deletePost(id));
    }
}