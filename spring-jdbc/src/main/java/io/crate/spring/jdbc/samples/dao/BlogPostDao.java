package io.crate.spring.jdbc.samples.dao;

import java.util.List;
import java.util.Optional;

import io.crate.spring.jdbc.samples.model.BlogPost;

public interface BlogPostDao {

    List<BlogPost> getPosts();

    Optional<BlogPost> getPost(String id);

    Optional<BlogPost> insertPost(BlogPost newPost);

    Optional<BlogPost> updatePostText(String id, String newText);

    Optional<BlogPost> incrementLikeCount(String id);

    boolean deletePost(String id);

    List<BlogPost> searchPostsWithQuery(String query);
}