package io.crate.spring.jdbc.samples.da;

import java.util.List;
import java.util.Optional;

import io.crate.spring.jdbc.samples.domain.BlogPost;

public interface BlogPostDao {

    public List<BlogPost> getPosts();
    public Optional<BlogPost> getPost(String id);
    public Optional<BlogPost> insertPost(BlogPost newPost);
    public Optional<BlogPost> updatePostText(String id, String newText);
    public Optional<BlogPost> incrementLikeCount(String id);
    public boolean deletePost(String id);
    public List<BlogPost>searchPostsWithQuery(String query);
}