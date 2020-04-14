package io.crate.spring.jdbc.samples;

import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.lang.NonNull;

import io.crate.spring.jdbc.samples.domain.BlogPost;
import io.crate.spring.jdbc.samples.domain.User;

public class BlogPostDeserializer {

    @SuppressWarnings("unchecked")
    public BlogPost fromMap(@NonNull final Map<String, Object> postProps) {

        BlogPost post = new BlogPost();
        if (postProps.containsKey("id"))
            post.setId(postProps.get("id").toString());
        if (postProps.containsKey("user")) {
            Map<String, Object> user = (Map<String, Object>) postProps.get("user");
            if (user != null)
            {
                User anUser = new User((String)user.get("name"));
                List<Double> locationValues = (List<Double>)user.get("location");
                anUser.setLon(locationValues.get(0)); anUser.setLat(locationValues.get(1));
                post.setUser(anUser);
            }
        }
        post.setText((String)postProps.get("text"));
        post.setImage_ref((String)postProps.get("image_ref"));
        post.setCreated(new Date());
        post.setLike_count(0);

        return post;
    }
}