# crate-sample-apps

## Overview

This repository contains implementations of a simple example app (guestbook)
that uses [Crate.IO][1] as its database backend. The frontend is written in
Javascript and communicates with the backend over a REST API. The REST API that
connects to Crate.IO is written in various programming languages using
different client libraries.

## Frontend

The frontend communicates via a REST API with the backend. The interface is
the same across all implementations of the backend which means that backend
implementations can be exchanged.

The code and usage instructions are located in the `frontend` subfolder of this
project.

## Backends

Available example application backends are:

* [x] Python (using [dbapi][2], [crate-pyton][3])
* [ ] PHP (using [PDO][4], [crate-pdo][5])
* [x] Java (using [JDBC][6], [crate-jdbc][7])

They are located inside their respective subfolders and contain both usage
instructions and commented application code.

### Download and install Crate.IO

For all backends you will neeed to download and run Crate.IO first.
This sample app requires `0.54.0` or higher. You can download it from our
[CDN][8].

After extracting the tarball run

```console
$ crate-*/bin/crate
```

to start a single instance which listens on HTTP port `4200` and transport port
`4300` for incoming client requests.

### Import country data

Once the Crate instance in running you need to create the schema and import
country data. You need [crash][9] (Crate Shell) to connect to Crate.

```console
$ crash < sql/schemas.sql
$ crash -c "COPY guestbook.countries FROM '$(pwd)/sql/countries.json'"
```

## Develop

### Backend API Spec

See [API SPEC](SPEC.md).

### Running Integration Tests

```console
$ cd tests
$ python3.4 tests.py --help
usage: Universal tests for example app backends [-h] [--port PORT]
                                                [--host HOST]
                                                [--protocol PROTOCOL]

optional arguments:
  -h, --help           show this help message and exit
  --port PORT          HTTP port on which the backend is listening.
  --host HOST          Hostname or IP on which the backend is listening.
  --protocol PROTOCOL  HTTP protocol of the backend.
$ python3.4 tests.py
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
