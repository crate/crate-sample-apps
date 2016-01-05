# The JDBC Sample App
This Crate JDBC backend uses [Spark][1] as web framework and the Crate [JDBC driver][2].

## Requirements
- [Java 8][3]

## Usage
To build and run the sample application we use the [Apache Maven][4] build tool.

### Build the Sample Application

```console
mvn clean install
```

See _README.txt_ in root folder of the project for instructions on how to create table schemas and populate the country data.

### Run Backend Application
By default the sample application runs on port _8080_.

```console
mvn exec:java -Dexec.mainClass="io.crate.jdbc.sample.App"
```

The port can be explicitly specified:

```console
$ mvn exec:java -Dexec.mainClass="io.crate.jdbc.sample.App"  -Dexec.args="8080"
```

[1]: http://sparkjava.com/
[2]: https://github.com/crate/crate-jdbc
[3]: http://www.oracle.com/technetwork/java/javase/overview/java8-2100321.html
[4]: https://maven.apache.org/index.html
