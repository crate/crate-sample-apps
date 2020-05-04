package io.crate.spring.jdbc.samples;

import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.lang.NonNull;

import io.crate.spring.jdbc.samples.model.BlogPost;
import io.crate.spring.jdbc.samples.model.User;

public class BlogPostDeserializer {

    @SuppressWarnings("unchecked")
    public BlogPost fromMap(@NonNull final Map<String, Object> postProps) {
        var post = new BlogPost();
        var id = postProps.get("id");

        if (id != null) {
            post.setId(id.toString());
        }
       var userData = (Map<String, Object>) postProps.get("user");

        if (userData != null) {
            String name = (String) userData.get("name");
            var location = (List<Double>) userData.get("location");
            if (name != null && location != null && location.size() == 2) {
                post.setUser(new User(name, location.get(0), location.get(1)));
            }
        }

        post.setText((String) postProps.get("text"));
        post.setImageRef((String) postProps.get("image_ref"));
        post.setCreated(new Date());
        post.setLikeCount(0);

        return post;
    }
}