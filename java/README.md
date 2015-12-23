# Crate.IO JDBC Backend

This Crate.IO JDBC backend uses [Spark][1] as web framework and
the Crate.IO [JDBC driver][2].

## Requirements

* [Java 8][3]

## Usage

To build and run the sample application we use [Apache Maven][4] build tool.

### Build the sample application

```console
$ mvn clean install
```

See `README.txt` in root folder of the project on instructions how to create
table schemas and populate country data.

### Run backend application

By default the sample application runs on port `8080` 
```console
$ mvn exec:java -Dexec.mainClass="io.crate.jdbc.sample.App"
```

The port also can be specified explicitly:

```console
$ mvn exec:java -Dexec.mainClass="io.crate.jdbc.sample.App"  -Dexec.args="8080"
```

[1]: http://sparkjava.com/
[2]: https://github.com/crate/crate-jdbc
[3]: http://www.oracle.com/technetwork/java/javase/overview/java8-2100321.html
[4]: https://maven.apache.org/index.html
