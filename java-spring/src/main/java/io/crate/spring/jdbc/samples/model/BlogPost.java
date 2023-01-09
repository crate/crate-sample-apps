package io.crate.spring.jdbc.samples.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Date;
import java.util.Objects;

public class BlogPost {

    private String id;

    private User user;

    private String text;

    private Date created;

    @JsonProperty("image_ref")
    private String imageRef;

    @JsonProperty("like_count")
    private long likeCount;

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

    public String getImageRef() {
        return imageRef;
    }

    public void setImageRef(String imageRef) {
        this.imageRef = imageRef;
    }

    public long getLikeCount() {
        return likeCount;
    }

    public void setLikeCount(long likeCount) {
        this.likeCount = likeCount;
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        BlogPost blogPost = (BlogPost) o;
        return likeCount == blogPost.likeCount &&
                Objects.equals(id, blogPost.id) &&
                Objects.equals(user, blogPost.user) &&
                Objects.equals(text, blogPost.text) &&
                Objects.equals(created, blogPost.created) &&
                Objects.equals(imageRef, blogPost.imageRef) &&
                Objects.equals(country, blogPost.country) &&
                Objects.equals(area, blogPost.area);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, user, text, created, imageRef, likeCount, country, area);
    }

    @Override
    public String toString() {
        return "BlogPost{" +
                "id='" + id + '\'' +
                ", user=" + user +
                ", text='" + text + '\'' +
                ", created=" + created +
                ", imageRef='" + imageRef + '\'' +
                ", likeCount=" + likeCount +
                ", country='" + country + '\'' +
                ", area=" + area +
                '}';
    }
}