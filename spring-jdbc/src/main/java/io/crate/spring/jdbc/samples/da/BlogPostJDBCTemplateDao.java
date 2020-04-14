package io.crate.spring.jdbc.samples.da;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Function;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import io.crate.shade.org.postgresql.geometric.PGpoint;
import io.crate.spring.jdbc.samples.domain.Area;
import io.crate.spring.jdbc.samples.domain.BlogPost;
import io.crate.spring.jdbc.samples.domain.User;

@Component
public class BlogPostJDBCTemplateDao implements BlogPostDao {

    private static final String POST_TABLE = "guestbook.posts";
    private static final String COUNTRIES_TABLE = "guestbook.countries";

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Autowired
    private JDBCDBObjectsFactory objectsFactory;

    public List<BlogPost> getPosts() {

        return this.jdbcTemplate.query(String.format(
                "SELECT p.id as id, p.text as text, "
                        + "p.\"user\"['name'] as username, p.\"user\"['location'] as userlocation, "
                        + "p.created as created, c.name as country, c.geometry as area, "
                        + "p.image_ref as imageRef, p.like_count as likes " + "FROM %s AS p, %s AS c "
                        + "WHERE within(p.\"user\"['location'], c.geometry) " + "ORDER BY p.created DESC",
                POST_TABLE, COUNTRIES_TABLE), (rs, rowNum) -> stdRowMapper.apply(rs));
    }

    public Optional<BlogPost> getPost(@NonNull String id) {

        List<BlogPost> queryResult = this.jdbcTemplate.query(String.format(
                "SELECT p.id as id, p.text as text, "
                        + "p.\"user\"['name'] as username, p.\"user\"['location'] as userlocation, "
                        + "p.created as created, c.name as country, c.geometry as area, "
                        + "p.image_ref as imageRef, p.like_count as likes " + "FROM %s AS p, %s AS c "
                        + "WHERE within(p.\"user\"['location'], c.geometry) " + "AND p.id = ?",
                POST_TABLE, COUNTRIES_TABLE), (rs, rowNum) -> stdRowMapper.apply(rs), id);

        return queryResult != null && queryResult.size() == 1 ? Optional.of(queryResult.get(0)) : Optional.empty();
    }

    public Optional<BlogPost> insertPost(@NonNull BlogPost newPost) {

        if (newPost.getId() == null) {
            newPost.setId(UUID.randomUUID().toString());
        }

        if (this.jdbcTemplate.update(
            String.format(
                "INSERT INTO %s " +
                "(id, \"user\", text, image_ref, created, like_count) " +
                "VALUES (?, ?, ?, ?, ?, ?)", POST_TABLE), 
                newPost.getId(), this.objectsFactory.createUserObject(newPost), newPost.getText(), 
                newPost.getImage_ref(), newPost.getCreated(), newPost.getLike_count()) != 1)
            return Optional.empty();
        else {
            // Since CrateDB is 'only' eventually consistent, we'll have to refresh the table before reading
            this.jdbcTemplate.execute(String.format("REFRESH TABLE %s", POST_TABLE));
            return this.getPost(newPost.getId());
        }
    }

    public Optional<BlogPost> updatePostText(@NonNull String id, String newText) {

       if (this.jdbcTemplate.update(
           String.format("UPDATE %s SET text = ? WHERE id = ?", POST_TABLE), newText, id) != 1)
            return Optional.empty();
        else {
            // Since CrateDB is 'only' eventually consistent, we'll have to refresh the table before reading
            this.jdbcTemplate.execute(String.format("REFRESH TABLE %s", POST_TABLE));
            return this.getPost(id);
        }
    }

    public Optional<BlogPost> incrementLikeCount(@NonNull String id) {

        if (this.jdbcTemplate.update(
            String.format("UPDATE %s SET like_count = like_count + 1 WHERE id = ?", POST_TABLE), id) != 1)
            return Optional.empty();
        else {
            // Since CrateDB is 'only' eventually consistent, we'll have to refresh the table before reading
            this.jdbcTemplate.execute(String.format("REFRESH TABLE %s", POST_TABLE));
            return this.getPost(id);
        }
    }

    public boolean deletePost(@NonNull String id) {

        return this.jdbcTemplate.update(
                    String.format("DELETE FROM %s", POST_TABLE) + " WHERE id = ?", id) == 1;
    }

    public List<BlogPost>searchPostsWithQuery(@NonNull final String query) {

        List<BlogPost> result = this.jdbcTemplate.query((String.format(
            "SELECT p.id as id, p.text as text, p._score as _score, "
                        + "p.\"user\"['name'] as username, p.\"user\"['location'] as userlocation, "
                        + "p.created as created, c.name as country, c.geometry as area, "
                        + "p.image_ref as imageRef, p.like_count as likes " +
            "FROM %s AS p, %s AS c " +
            "WHERE within(p.\"user\"['location'], c.geometry)" +
            "AND match(text, ?) " +
            "ORDER BY _score DESC", POST_TABLE, COUNTRIES_TABLE)), 
            (rs, rowNum) -> stdRowMapper.apply(rs), query);

        return result;
    }

    @SuppressWarnings("unchecked")
    private final Function<ResultSet, BlogPost> stdRowMapper = rs -> {

        BlogPost result = new BlogPost();

        try {
            result.setId(rs.getString("id"));
            result.setText(rs.getString("text"));
            result.setCountry(rs.getString("country"));
            Map<String, Object> area = (Map<String, Object>)rs.getObject("area");
            result.setArea(new Area((String)area.get("type"), (List<List<Double>>)area.get("coordinates")));
            PGpoint loc = (PGpoint)(rs.getObject("userlocation"));
            result.setUser(new User(rs.getString("username"), loc.x, loc.y));
            result.setCreated(rs.getDate("created"));
            result.setImage_ref(rs.getString("imageRef"));
            result.setLike_count(rs.getLong("likes"));
        } catch (SQLException e) {
            return null;
        }

        return result;
    };

}