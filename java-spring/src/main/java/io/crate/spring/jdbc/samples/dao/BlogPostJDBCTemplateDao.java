package io.crate.spring.jdbc.samples.dao;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import io.crate.shade.org.postgresql.geometric.PGpoint;
import io.crate.spring.jdbc.samples.model.Area;
import io.crate.spring.jdbc.samples.model.BlogPost;
import io.crate.spring.jdbc.samples.model.User;

@Component
public class BlogPostJDBCTemplateDao implements BlogPostDao {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Autowired
    private JDBCDBObjectsFactory objectsFactory;

    public List<BlogPost> getPosts() {
        return this.jdbcTemplate.query(
                "SELECT p.id as id, p.text as text, "
                        + "p.\"user\"['name'] as username, p.\"user\"['location'] as userlocation, "
                        + "p.created as created, c.name as country, c.geometry as area, "
                        + "p.image_ref as imageRef, p.like_count as likes " + "FROM guestbook.posts AS p, guestbook.countries AS c "
                        + "WHERE within(p.\"user\"['location'], c.geometry) " + "ORDER BY p.created DESC"
               , blogPostRowMapper);
    }

    public Optional<BlogPost> getPost(@NonNull String id) {
        List<BlogPost> queryResult = this.jdbcTemplate.query(
                "SELECT p.id as id, p.text as text, "
                        + "p.\"user\"['name'] as username, p.\"user\"['location'] as userlocation, "
                        + "p.created as created, c.name as country, c.geometry as area, "
                        + "p.image_ref as imageRef, p.like_count as likes " + "FROM guestbook.posts AS p, guestbook.countries AS c "
                        + "WHERE within(p.\"user\"['location'], c.geometry) " + "AND p.id = ?"
                , blogPostRowMapper, id);

        return queryResult.size() == 1 ? Optional.of(queryResult.get(0)) : Optional.empty();
    }

    public Optional<BlogPost> insertPost(@NonNull BlogPost newPost) {
        if (newPost.getId() == null) {
            newPost.setId(UUID.randomUUID().toString());
        }
        if (this.jdbcTemplate.update(
                "INSERT INTO guestbook.posts " +
                        "(id, \"user\", text, image_ref, created, like_count) " +
                        "VALUES (?, ?, ?, ?, ?, ?)",
                newPost.getId(), this.objectsFactory.createUserObject(newPost), newPost.getText(),
                newPost.getImageRef(), newPost.getCreated(), newPost.getLikeCount()) != 1)
            return Optional.empty();
        else {
            // Since CrateDB is 'only' eventually consistent, we'll have to refresh the table before reading
            this.jdbcTemplate.execute("REFRESH TABLE guestbook.posts");
            return this.getPost(newPost.getId());
        }
    }

    public Optional<BlogPost> updatePostText(@NonNull String id, String newText) {
        if (this.jdbcTemplate.update("UPDATE guestbook.posts SET text = ? WHERE id = ?", newText, id) != 1)
            return Optional.empty();
        else {
            // Since CrateDB is 'only' eventually consistent, we'll have to refresh the table before reading
            this.jdbcTemplate.execute("REFRESH TABLE guestbook.posts");
            return this.getPost(id);
        }
    }

    public Optional<BlogPost> incrementLikeCount(@NonNull String id) {
        if (this.jdbcTemplate.update("UPDATE guestbook.posts SET like_count = like_count + 1 WHERE id = ?", id) != 1)
            return Optional.empty();
        else {
            // Since CrateDB is 'only' eventually consistent, we'll have to refresh the table before reading
            this.jdbcTemplate.execute("REFRESH TABLE guestbook.posts");
            return this.getPost(id);
        }
    }

    public boolean deletePost(@NonNull String id) {
        return this.jdbcTemplate.update(
                "DELETE FROM guestbook.posts WHERE id = ?", id) == 1;
    }

    public List<BlogPost> searchPostsWithQuery(@NonNull final String query) {
        return jdbcTemplate.query(
                "SELECT p.id as id, p.text as text, p._score as _score, "
                        + "p.\"user\"['name'] as username, p.\"user\"['location'] as userlocation, "
                        + "p.created as created, c.name as country, c.geometry as area, "
                        + "p.image_ref as imageRef, p.like_count as likes " +
                        "FROM guestbook.posts AS p, \"guestbook.countries\" AS c " +
                        "WHERE within(p.\"user\"['location'], c.geometry)" +
                        "AND match(text, ?) ORDER BY _score DESC",
                blogPostRowMapper, query);
    }

    @SuppressWarnings("unchecked")
    private final RowMapper<BlogPost> blogPostRowMapper = (rs, s) -> {
        var result = new BlogPost();
        result.setId(rs.getString("id"));
        result.setText(rs.getString("text"));
        result.setCountry(rs.getString("country"));
        var area = (Map<String, Object>) rs.getObject("area");
        result.setArea(new Area((String) area.get("type"), (List<List<Double>>) area.get("coordinates")));
        var loc = (PGpoint) (rs.getObject("userlocation"));
        result.setUser(new User(rs.getString("username"), loc.x, loc.y));
        result.setCreated(rs.getDate("created"));
        result.setImageRef(rs.getString("imageRef"));
        result.setLikeCount(rs.getLong("likes"));
        return result;
    };
}