package io.crate.spring.jdbc.samples.da;

import java.util.Date;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import io.crate.spring.jdbc.samples.domain.BlogPost;
import io.crate.spring.jdbc.samples.domain.User;

@SpringBootTest
public class BlogPostJDBCTemplateDaoTests {

    @Autowired
    private BlogPostDao dao;

    @Test
    public void testReadingAllPosts() {

        List<BlogPost> posts = this.dao.getPosts();
        Assertions.assertNotNull(posts);
    }

    @Test
    public void testInsertNewPost() {

        BlogPost newPost = new BlogPost();
        newPost.setId(UUID.randomUUID().toString());
        newPost.setText("Und schon wieder ein Test");
        User anUser = new User("Max Mustermann");
        anUser.setLat(16.3552942); anUser.setLon(48.326483499999995);
        newPost.setUser(anUser);
        newPost.setCreated(new Date());
        
        BlogPost dbPost = this.dao.insertPost(newPost).get();
        Assertions.assertNotNull(dbPost);
    }

    @Test
    public void testDeletePost() {

        Assertions.assertTrue(this.dao.deletePost("ceb6a539-59f7-46b9-8d19-d5acc56e94c5"));
    }

    @Test
    public void testUpdatePost() {

        Assertions.assertTrue(this.dao.updatePostText("c6720513-06f6-4dec-b9c1-b19084ea749c", "Ein neuer Text").isPresent());
    }

    @Test
    public void testIncrementLikeCount() {

        Assertions.assertTrue(this.dao.incrementLikeCount("c6720513-06f6-4dec-b9c1-b19084ea749c").isPresent());
    }
}