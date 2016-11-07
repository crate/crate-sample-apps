# Crate Sample Apps
## Overview
This repository contains implementations of a simple guestbook example app that
uses [Crate][1] as its database backend. The frontend is written in
Javascript and communicates with the backend over a [REST API][2].
The REST API that connects to Crate is written in various programming
languages using different [client libraries available for Crate][3].

## Frontend
The frontend communicates via a REST API with the backend. The interface is the
same across all implementations of the backend which allows for simple exchange
of backends.

The code and usage instructions are in the _frontend_ subfolder of this project.

## Backends
The example application backends available are:

- [x] Python (using [dbapi][4], [crate-python][5])
- [x] PHP (using [PDO][6], [crate-pdo][7])
- [x] Java (using [JDBC][8], [pgjdbc][9])
- [x] Erlang (using [Erlang][10], [craterl][11])

They are inside their respective subfolders and contain both usage instructions
and commented application code.

### Download and Install Crate
For all backends you will need to install and run Crate first. This sample
app requires Crate `0.54.1` or higher. The Java sample app requires Crate
`0.57.0` or higher. Several ways exist to get an instance of Crate running,
visit our [Getting Started][12] section to find the one that works best for you.

### Import Country Data
Once the Crate instance in running, create the schema and import country data.
You need [crash][13] (Crate Shell) to connect to Crate.

```bash
crash < sql/schemas.sql
crash -c "COPY guestbook.countries FROM '$(pwd)/sql/countries.json'"
```

## Develop
### Backend API Spec
See the [API spec](SPEC.md).

### Running Integration Tests

```bash
cd tests
python3.4 tests.py --host SERVER_IP --port 4200
```

[1]: https://crate.io
[2]: https://crate.io/docs/clients/rest/
[3]: https://crate.io/docs/clients/
[4]: https://www.python.org/dev/peps/pep-0249/
[5]: https://github.com/crate/crate-python
[6]: http://at2.php.net/manual/en/book.pdo.php
[7]: https://github.com/crate/crate-pdo
[8]: http://www.oracle.com/technetwork/java/overview-141217.html
[9]: https://github.com/pgjdbc/pgjdbc
[10]: http://www.erlang.org/
[11]: https://github.com/crate/craterl
[12]: https://crate.io/docs/getting-started/
[13]: https://github.com/crate/crash
