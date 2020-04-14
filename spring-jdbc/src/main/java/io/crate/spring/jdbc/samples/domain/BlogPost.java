package io.crate.spring.jdbc.samples.domain;

import java.util.Date;

public class BlogPost {

    private String id;

    private User user;

    private String text;

    private Date created;

    private String image_ref;

    private long like_count;

    private String country;

    private Area area;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public Date getCreated() {
        return created;
    }

    public void setCreated(Date created) {
        this.created = created;
    }

    public String getImage_ref() {
        return image_ref;
    }

    public void setImage_ref(String image_ref) {
        this.image_ref = image_ref;
    }

    public long getLike_count() {
        return like_count;
    }

    public void setLike_count(long like_count) {
        this.like_count = like_count;
    }

    @Override
    public String toString() {
        return "BlogPost [created=" + created + ", id=" + id + ", image_ref=" + image_ref + ", like_count=" + like_count
                + ", text=" + text + ", user=" + user + "]";
    }

    public String getCountry() {
        return country;
    }

    public void setCountry(String country) {
        this.country = country;
    }

    public Area getArea() {
        return area;
    }

    public void setArea(Area area) {
        this.area = area;
    }
}