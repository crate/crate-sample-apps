# Getting Started with the Crate JDBC Sample Application
This sample application uses [Spark][1] as web framework and the
[Crate JDBC driver][2] to access the Crate database.

## Requirements
- [Java 8][3]

## Usage
To build and run the sample application we use the [Apache Maven][4]
build tool.

### Run Crate Server
The sample application requires version `0.57.0` or higher.

### Build the Sample Application

```console
mvn clean install
```

See *README.txt* in root folder of the project for instructions on how
to create table schemas and populate the country data.

### Run Application
By default the sample application runs on port *8080*.

Run the application:

```console
mvn exec:java -Dexec.mainClass="io.crate.jdbc.sample.App"
```

Run the application on specific port:

```console
$ mvn exec:java -Dexec.mainClass="io.crate.jdbc.sample.App" -Dexec.args="8080"
```

[1]: http://sparkjava.com/
[2]: https://crate.io/docs/clients/jdbc/
[3]: http://www.oracle.com/technetwork/java/javase/overview/java8-2100321.html
[4]: https://maven.apache.org/index.html
