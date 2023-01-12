.. highlight:: sh

===================================
CrateDB guestbook demo applications
===================================

About
=====

A gallery of applications, each implementing the same HTTP API for a guestbook
backend, using SQL and CrateDB. Each implementation uses a different `client
library`_ to communicate with CrateDB_.

Accompanied with the applications, there is a dedicated test suite to verify
the corresponding implementation automatically, and a guestbook frontend
application for interactively communicating with the backend service.

Prerequisites
=============

- You need install and run CrateDB before proceeding with the software found
  within this repository.
- You will also need to install crash_, the CrateDB CLI tool, to work with CrateDB
  on the command line.
- See the CrateDB `Getting Started`_ guide for help.

Setup
=====

Once a CrateDB instance is running, create the required schema and import the
country data::

    crash < sql/schemas.sql
    crash -c "COPY guestbook.countries FROM '$(pwd)/sql/countries.json' RETURN SUMMARY;"

Please note that the ``countries.json`` file needs to be accessible by CrateDB.
See the `developer docs`_ about how to run and provision CrateDB using Docker.


Components
==========

Frontend
--------

The frontend_ is shared by all apps and is written in JavaScript. This frontend
communicates with the backend over a `REST API`_.

Backends
--------

The are several implementations of the backend REST API.

- `Erlang (Cowboy)`_ (using Erlang_, craterl_)
- `Go (Gin)`_ (using Go_, Gin_, pgx_, pgxscan_)
- `Java (Spark)`_ (using Spark_, JDBC_, `CrateDB JDBC driver`_)
- `Java (Spring)`_ (using  `Spring Boot`_, `Spring Data JDBC`_, `CrateDB JDBC driver`_)
- `Node.js (Express)`_ (using  `Node.js`_, Express_, `node-crate`_)
- `PHP (Slim)`_ (using PHP_, Slim_, PDO_, `crate-pdo`_)
- `Python (Flask)`_ (using Python_, Flask_, DBAPI2_, `crate-python`_)


Contributing
============

This project is community-maintained, any contributions are welcome.
See the `developer docs`_, `api specification`_, and the `contribution docs`_
documents for more information.

Help
====

Looking for more help?

- Check out our `support channels`_


.. _api specification: SPEC.md
.. _client library: https://crate.io/docs/clients/
.. _contribution docs: CONTRIBUTING.rst
.. _crash: https://github.com/crate/crash
.. _crate-pdo: https://github.com/crate/crate-pdo
.. _crate-python: https://github.com/crate/crate-python
.. _Crate.io: https://crate.io/
.. _CrateDB: https://github.com/crate/crate
.. _CrateDB JDBC driver: https://crate.io/docs/clients/jdbc/
.. _craterl: https://github.com/crate/craterl
.. _DBAPI2: https://www.python.org/dev/peps/pep-0249/
.. _developer docs: DEVELOP.rst
.. _Erlang: https://www.erlang.org/
.. _Erlang (Cowboy): erlang-cowboy
.. _Express: https://expressjs.com/
.. _Flask: https://flask.palletsprojects.com/
.. _frontend: frontend
.. _Getting Started: https://crate.io/docs/getting-started/
.. _Gin: https://github.com/gin-gonic/gin
.. _Go: https://go.dev/
.. _Go (Gin): go-gin
.. _HTTPie: https://httpie.io/
.. _Java (Spark): java-spark
.. _Java (Spring): java-spring
.. _JDBC: https://docs.oracle.com/javase/tutorial/jdbc/
.. _Node.js: https://nodejs.org/
.. _Node.js (Express): nodejs-express
.. _node-crate: https://www.npmjs.com/package/node-crate
.. _PDO: https://www.php.net/manual/en/book.pdo.php
.. _pgjdbc: https://github.com/pgjdbc/pgjdbc
.. _PHP: https://www.php.net/
.. _PHP (Slim): php-slim
.. _pgx: https://github.com/jackc/pgx
.. _pgxscan: https://github.com/georgysavva/scany
.. _Python: https://www.python.org/
.. _Python (Flask): python-flask
.. _REST API: https://en.wikipedia.org/wiki/Representational_state_transfer
.. _Slim: https://www.slimframework.com/
.. _Spark: https://sparkjava.com/
.. _Spring Boot: https://spring.io/projects/spring-boot
.. _Spring Data JDBC: https://spring.io/projects/spring-data-jdbc
.. _support channels: https://crate.io/support/
