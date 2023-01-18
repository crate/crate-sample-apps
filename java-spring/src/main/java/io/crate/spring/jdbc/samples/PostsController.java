package io.crate.spring.jdbc.samples;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import io.crate.spring.jdbc.samples.dao.BlogPostDao;
import io.crate.spring.jdbc.samples.model.BlogPost;

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

        var posts = dao.getPosts();
        logger.info("Found " + posts.size() + " in database");

        return convertToStdResult(posts);
    }

    @PostMapping("/posts")
    public List<Map<String, Object>> insertPost(@RequestBody(required=false) Map<String, Object> postProps, HttpServletResponse response) {
        logger.debug("Inserting post in database");

        if (postProps == null) {
            throw new ArgumentRequiredException("Request body is required");
        } else if (!postProps.containsKey("text")) {
            throw new ArgumentRequiredException("Argument \"text\" is required");
        } else {
            @SuppressWarnings("unchecked")
            var user = (Map<String, Object>) postProps.get("user");
            if (!user.containsKey("location"))
                throw new ArgumentRequiredException("Argument \"location\" is required");
        }

        var newPost = dao.insertPost(postsDeserializer.fromMap(postProps)).orElseThrow(() -> new PostNotFoundException("new from insert"));
        response.setStatus(HttpStatus.CREATED.value());
        return List.of(postsSerializer.serialize(newPost));
    }

    @GetMapping("/post/{id}")
    public Map<String, Object> getPost(@PathVariable String id) {
        logger.debug("Searching for post with id '" + id + "' in database");
        return postsSerializer.serialize(dao.getPost(id).orElseThrow(() -> new PostNotFoundException(id)));
    }

    @PutMapping("/post/{id}")
    public Map<String, Object> updatePost(@PathVariable String id, @RequestBody(required=false) Map<String, Object> postProps) {
        logger.debug("Updating post with id '" + id + "' in database");

        if (postProps == null) {
            throw new ArgumentRequiredException("Request body is required");
        } else if (!postProps.containsKey("text")) {
            throw new ArgumentRequiredException("Argument \"text\" is required");
        }

        String text = postProps != null ? postProps.get("text").toString() : null;
        return postsSerializer.serialize(dao.updatePostText(id, text).orElseThrow(() -> new PostNotFoundException(id)));
    }

    @PutMapping("/post/{id}/like")
    public Map<String, Object> incrementLikeCount(@PathVariable String id) {
        logger.debug("Incrementing like count of post with id " + id);
        BlogPost post = dao.incrementLikeCount(id).orElseThrow(() -> new PostNotFoundException(id));
        return postsSerializer.serialize(post);
    }

    @DeleteMapping("/post/{id}")
    public void deletePost(@PathVariable String id, HttpServletResponse response) {
        logger.debug("Deleting post with id '" + id + "' in database");
        if (!dao.deletePost(id)) {
            throw new PostNotFoundException(id);
        }
        response.setStatus(HttpStatus.NO_CONTENT.value());
    }

    @PostMapping("/search")
    public List<Map<String, Object>> searchPosts(@RequestBody Map<String, Object> params) {
        logger.debug("Searching posts");
        if (params == null) {
            throw new ArgumentRequiredException("Request body is required");
        } else if (!params.containsKey("query_string")) {
            throw new ArgumentRequiredException("Argument \"query_string\" is required");
        }
        return convertToStdResult(dao.searchPostsWithQuery((String) params.get("query_string")));
    }

    private List<Map<String, Object>> convertToStdResult(final List<BlogPost> posts) {
        var result = new ArrayList<Map<String, Object>>();
        for (var post : posts) {
            result.add(postsSerializer.serialize(post));
        }
        return result;
    }
}