# Crate Sample Apps
## Overview
This repository contains implementations of a simple guestbook example app that uses [Crate.IO][1] as its database backend. The frontend is written in Javascript and communicates with the backend over a [REST API](https://crate.io/docs/getting-started/clients/rest/). The REST API that connects to Crate.IO is written in various programming languages using different [client libraries available for Crate](https://crate.io/docs/getting-started/).

## Frontend
The frontend communicates via a REST API with the backend. The interface is the same across all implementations of the backend which allows for simple exchange of backends.

The code and usage instructions are in the _frontend_ subfolder of this project.

## Backends
The example application backends available are:

- [x] Python (using [dbapi][2], [crate-pyton][3])
- [x] PHP (using [PDO][4], [crate-pdo][5])
- [x] Java (using [JDBC][6], [crate-jdbc][7])
- [x] Erlang (using [Erlang][10], [craterl][11])

They are inside their respective subfolders and contain both usage instructions and commented application code.

### Download and Install Crate.IO
For all backends you will need to install and run Crate.IO first. This sample app requires version **0.54.0** or higher. Several ways exist to get an instance of Crate running, visit our [Getting Started](https://crate.io/docs/getting-started/) section to find the one that works best for you.

### Import Country Data
Once the Crate instance in running, create the schema and import country data. You need [crash][9] (Crate Shell) to connect to Crate.

```bash
crash < sql/schemas.sql
crash -c "COPY guestbook.countries FROM 'PATH_TO_SAMLE_APPS/sql/countries.json'"
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
[2]: https://www.python.org/dev/peps/pep-0249/
[3]: https://github.com/crate/crate-python
[4]: http://at2.php.net/manual/en/book.pdo.php
[5]: https://github.com/crate/crate-pdo
[6]: http://www.oracle.com/technetwork/java/overview-141217.html
[7]: https://github.com/crate/crate-jdbc
[8]: https://cdn.crate.io/downloads/releases/
[9]: https://github.com/crate/crash/
[10]: http://www.erlang.org/
[11]: https://github.com/crate/craterl