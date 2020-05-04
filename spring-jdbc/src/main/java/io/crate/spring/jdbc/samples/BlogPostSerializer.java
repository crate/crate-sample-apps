package io.crate.spring.jdbc.samples;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.springframework.lang.NonNull;

import io.crate.spring.jdbc.samples.model.BlogPost;

public class BlogPostSerializer {

    private final static SimpleDateFormat SIMPLE_DATE_FORMAT = new SimpleDateFormat("MMM d, yyyy hh:mm:ss a", Locale.US);

    public Map<String, Object> serialize(@NonNull final BlogPost post) {

        var postProps = new HashMap<String, Object>();

        if (post.getArea() != null) {
            var areaMap = new HashMap<String, Object>();
            areaMap.put("coordinates", post.getArea().getCoordinates());
            areaMap.put("type", post.getArea().getType());
            postProps.put("area", areaMap);
        }
        postProps.put("country", post.getCountry());
        postProps.put("image_ref", post.getImageRef());
        postProps.put("like_count", post.getLikeCount());

        if (post.getCreated() != null) {
            postProps.put("created", SIMPLE_DATE_FORMAT.format(post.getCreated()));
        } else {
            postProps.put("created", null);
        }
        postProps.put("id", post.getId());
        postProps.put("text", post.getText());

        if (post.getUser() != null) {
            var userMap = new HashMap<String, Object>();
            userMap.put("name", post.getUser().getName());
            var locList = new ArrayList<Double>(2);
            locList.add(post.getUser().getLon());
            locList.add(post.getUser().getLat());
            userMap.put("location", locList);
            postProps.put("user", userMap);
        }

        return postProps;
    }
}