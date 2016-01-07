package io.crate.jdbc.sample;


import java.sql.SQLException;

import static spark.Spark.port;

public class App {

    public static void main(String[] args) throws SQLException {
        if (args.length == 1) {
            port(Integer.parseInt(args[0]));
        } else {
            port(Integer.parseInt(DataProvider.getProperty("web.server.port")));
        }
        new Controller(new DataProvider());
    }

}
