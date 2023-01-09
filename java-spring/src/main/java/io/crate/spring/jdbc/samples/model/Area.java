package io.crate.spring.jdbc.samples.model;

import java.util.List;
import java.util.Objects;

public class Area {

    private final String type;

    private final List<List<Double>> coordinates;

    public Area(String type, List<List<Double>> coordinates) {
        this.type = type;
        this.coordinates = coordinates;
    }

    public List<List<Double>> getCoordinates() {
        return coordinates;
    }

    public String getType() {
        return type;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Area area = (Area) o;
        return Objects.equals(type, area.type) &&
                Objects.equals(coordinates, area.coordinates);
    }

    @Override
    public int hashCode() {
        return Objects.hash(type, coordinates);
    }
}