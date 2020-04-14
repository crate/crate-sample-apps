package io.crate.spring.jdbc.samples.domain;

public class User {

    private String name;

    private double lon;
    private double lat;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public double getLon() {
        return lon;
    }

    public void setLon(double lon) {
        this.lon = lon;
    }

    public double getLat() {
        return lat;
    }

    public void setLat(double lat) {
        this.lat = lat;
    }

    @Override
    public String toString() {
        return "User [lat=" + lat + ", lon=" + lon + ", name=" + name + "]";
    }

    public User(String name, double lon, double lat) {
        this.name = name;
        this.lon = lon;
        this.lat = lat;
    }

    public User(String name) {
        this.name = name;
    }

}