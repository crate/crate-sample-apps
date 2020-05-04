package io.crate.spring.jdbc.samples.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Date;
import java.util.Objects;

public class Image {

    private final String digest;

    @JsonProperty("last_modified")
    private final Date lastModified;

    public Image(String digest, Date last_modified) {
        this.digest = digest;
        this.lastModified = last_modified;
    }

    public String getDigest() {
        return digest;
    }

    public Date getLastModified() {
        return lastModified;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Image image = (Image) o;
        return Objects.equals(digest, image.digest) &&
                Objects.equals(lastModified, image.lastModified);
    }

    @Override
    public int hashCode() {
        return Objects.hash(digest, lastModified);
    }
}