package io.crate.spring.jdbc.samples.domain;

import java.util.List;

public class Area {

    private String type;

    private List<List<Double>> coordinates;

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public List<List<Double>> getCoordinates() {
        return coordinates;
    }

    public void setCoordinates(List<List<Double>> coordinates) {
        this.coordinates = coordinates;
    }

    public Area(String type, List<List<Double>> coordinates) {
        this.type = type;
        this.coordinates = coordinates;
    }
}