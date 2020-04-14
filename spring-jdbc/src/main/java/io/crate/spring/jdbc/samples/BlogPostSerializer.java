package io.crate.spring.jdbc.samples;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.springframework.lang.NonNull;

import io.crate.spring.jdbc.samples.domain.BlogPost;

public class BlogPostSerializer {

    public Map<String, Object> serialize(@NonNull final BlogPost post) {

        Map<String, Object> postProps = new HashMap<String, Object>();

        if (post.getArea() != null) {
            Map<String, Object> areaMap = new HashMap<String, Object>();
            areaMap.put("coordinates", post.getArea().getCoordinates());
            areaMap.put("type", post.getArea().getType());
            postProps.put("area", areaMap);
        }
        postProps.put("country", post.getCountry());
        postProps.put("image_ref", post.getImage_ref());
        postProps.put("like_count", post.getLike_count());
        if (post.getCreated() != null) {
            String pattern = "MMM d, yyyy hh:mm:ss a";
            SimpleDateFormat simpleDateFormat = new SimpleDateFormat(pattern, Locale.US);
            postProps.put("created", simpleDateFormat.format(post.getCreated()));
        } else
            postProps.put("created", null);
        postProps.put("id", post.getId());
        postProps.put("text", post.getText());
        if (post.getUser() != null) {
            Map<String, Object> userMap = new HashMap<String, Object>();
            userMap.put("name", post.getUser().getName());
            List<Double> locList = new ArrayList<Double>(2);
            locList.add(post.getUser().getLon());
            locList.add(post.getUser().getLat());
            userMap.put("location", locList);
            postProps.put("user", userMap);
        }

        return postProps;
    }
}