package io.crate.spring.jdbc.samples.model;

import java.util.Objects;

public class User {

    private final String name;
    private final double lon;
    private final double lat;

    public User(String name, double lat, double lon) {
        this.name = name;
        this.lat = lat;
        this.lon = lon;
    }

    public String getName() {
        return name;
    }

    public double getLon() {
        return lon;
    }

    public double getLat() {
        return lat;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        User user = (User) o;
        return Double.compare(user.lon, lon) == 0 &&
                Double.compare(user.lat, lat) == 0 &&
                Objects.equals(name, user.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, lon, lat);
    }

    @Override
    public String toString() {
        return "User{" +
                "name='" + name + '\'' +
                ", lon=" + lon +
                ", lat=" + lat +
                '}';
    }
}