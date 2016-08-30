# Getting Started with the Crate JDBC Sample Application
This sample application uses [Spark][1] as web framework and the
[PostgreSQL JDBC driver][2] to access the Crate database.

## Requirements
- [Spark Framework][1]
- [PostgreSQL JDBC][2]
- [Java 8][3]

## Usage
To build and run the sample application we use the [Apache Maven][4]
build tool.

### Run Crate Server
The sample application requires version 0.56.0 or higher. The postgres
wire protocol is disabled by default. Therefore, please make sure that
it is enabled, by setting it to `true` in the `crate.yml`:

```yaml
psql.enabled: true
```

or providing it as a command line argument:

```bash
./bin/crate -Des.psql.enabled=true
```

for more detailed information, please see the Crate [documentation][5].

### Build the Sample Application

```console
mvn clean install
```

See _README.txt_ in root folder of the project for instructions on how
to create table schemas and populate the country data.

### Run Application
By default the sample application runs on port _8080_.

Run the application:

```console
mvn exec:java -Dexec.mainClass="io.crate.jdbc.sample.App"
```

Run the application on specific port:

```console
$ mvn exec:java -Dexec.mainClass="io.crate.jdbc.sample.App" -Dexec.args="8080"
```

[1]: http://sparkjava.com/
[2]: https://jdbc.postgresql.org/
[3]: http://www.oracle.com/technetwork/java/javase/overview/java8-2100321.html
[4]: https://maven.apache.org/index.html
[5]: https://crate.io/docs/reference/en/latest/protocols/postgres.html#jdbc
