package io.crate.spring.jdbc.samples;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import io.crate.spring.jdbc.samples.da.BlogPostDao;
import io.crate.spring.jdbc.samples.domain.BlogPost;

@RestController
public class PostsController {

    private static final Log logger = LogFactory.getLog(PostsController.class);

    @Autowired
    private BlogPostDao dao;

    @Autowired
    private BlogPostSerializer postsSerializer;

    @Autowired
    private BlogPostDeserializer postsDeserializer;

    @GetMapping("/posts")
    public List<Map<String, Object>> getPosts() {
        logger.debug("Searching for posts in database");
        
        List<BlogPost> posts = this.dao.getPosts();      
        logger.info("Found " + posts.size() + " in database");

        return this.convertToStdResult(posts);
    }

    @PostMapping("/posts")
    public List<Map<String, Object>> insertPosts(@RequestBody Map<String, Object> postProps) {
        logger.debug("Inserting post in database");

        if (postProps == null)
            throw new ArgumentRequiredException("Request body is required");
        else if (!postProps.containsKey("text"))
            throw new ArgumentRequiredException("Argument \"text\" is required");
        else {
            @SuppressWarnings("unchecked")
            Map<String, Object> user = (Map<String, Object>) postProps.get("user");
            if (!user.containsKey("location"))
                throw new ArgumentRequiredException("Argument \"location\" is required");
        }

        BlogPost newPost = this.dao.insertPost(
            this.postsDeserializer.fromMap(postProps)).orElseThrow(()-> new PostNotFoundException("new from insert"));

        List<BlogPost> posts = new ArrayList<BlogPost>();
        posts.add(newPost);
        return this.convertToStdResult(posts);
    }

    @GetMapping("/post/{id}")
    public Map<String, Object> getPost(@PathVariable String id) {
        logger.debug("Searching for post with id '" + id + "' in database");

        return this.postsSerializer.serialize(
            this.dao.getPost(id).orElseThrow(()-> new PostNotFoundException(id) ));
    }

    @PutMapping("/post/{id}")
    public Map<String, Object> updatePost(@PathVariable String id, @RequestBody Map<String, Object> postProps) {
        logger.debug("Updating post with id '" + id + "' in database");

        if (postProps == null)
            throw new ArgumentRequiredException("Request body is required");
        else if (!postProps.containsKey("text"))
            throw new ArgumentRequiredException("Argument \"text\" is required");

        String text = postProps != null ? postProps.get("text").toString() : null;
        return this.postsSerializer.serialize(
            this.dao.updatePostText(id, text).orElseThrow(()-> new PostNotFoundException(id) ));
    }

    @PutMapping("/post/{id}/like")
    public Map<String, Object> incrementLikeCount(@PathVariable String id) {
        logger.debug("Incrementing like count of post with id " + id);

        BlogPost post = this.dao.incrementLikeCount(id).orElseThrow(()-> new PostNotFoundException(id) );
        return this.postsSerializer.serialize(post);
    }

    @DeleteMapping("/post/{id}")
    public void deletePost(@PathVariable String id) {
        logger.debug("Deleting post with id '" + id + "' in database");

        if (! this.dao.deletePost(id)) {
            throw new PostNotFoundException(id);
        }
    }

    @PostMapping("/search")
    public List<Map<String, Object>> searchPosts(@RequestBody Map<String, Object> params) {

        logger.debug("Searching posts");

        if (params == null)
            throw new ArgumentRequiredException("Request body is required");
        else if (!params.containsKey("query_string"))
            throw new ArgumentRequiredException("Argument \"query_string\" is required");

        return this.convertToStdResult(this.dao.searchPostsWithQuery((String)params.get("query_string")));
    }

    private List<Map<String, Object>> convertToStdResult(final List<BlogPost> posts) {

        List<Map<String, Object>> result = new ArrayList<Map<String, Object>>();

        if (posts != null && !posts.isEmpty()) {
            for(BlogPost post : posts) {
                result.add(this.postsSerializer.serialize(post));
            }
        }
        return result;
    }
}