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
* [ ] Java (using [JDBC][6], [crate-jdbc][7])

They are located inside their respective subfolders and contain both usage
instructions and commented application code.

[1]: https://crate.io
[2]: https://www.python.org/dev/peps/pep-0249/
[3]: https://github.com/crate/crate-python
[4]: http://at2.php.net/manual/en/book.pdo.php
[5]: https://github.com/crate/crate-pdo
[6]: http://www.oracle.com/technetwork/java/overview-141217.html
[7]: https://github.com/crate/crate-jdbc
