package io.crate.jdbc.sample;


import java.sql.SQLException;

public class App {

    public static void main(String[] args) throws SQLException {
        new Controller(new DataProvider());
    }

}
