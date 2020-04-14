package io.crate.spring.jdbc.samples.domain;

import java.util.Date;

public class Image {

    private String digest;

    private Date last_modified;

    public String getDigest() {
        return digest;
    }

    public void setDigest(String digest) {
        this.digest = digest;
    }

    public Date getLast_modified() {
        return last_modified;
    }

    public void setLast_modified(Date last_modified) {
        this.last_modified = last_modified;
    }
}