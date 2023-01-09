.. highlight:: sh

===================
CrateDB Sample Apps
===================

Overview
========

A JavaScript guestbook app with a number of different backend implementations,
each using a different `client library`_ to communicate with CrateDB_.

Prerequisites
=============

- You need install and run CrateDB 0.54.1 or higher before you set things up.
- If you're using the Java backend, you will need 0.57.0 or higher.
- You will also need to install crash_, the CrateDB CLI tool, to work with CrateDB
  on the command line.
- See the CrateDB `Getting Started`_ guide for help.

Setup
=====

Once a CrateDB instance is running, create the required schema and import the
country data::

    crash < sql/schemas.sql
    crash -c "COPY guestbook.countries FROM '$(pwd)/sql/countries.json' RETURN SUMMARY;"

If you choose to use Docker for running CrateDB on your workstation, those
commands outline the process::

    # Define CrateDB version.
    export CRATEDB_VERSION=5.1.2
    export CRATEDB_IMAGE=crate:${CRATEDB_VERSION}

    # Start CrateDB.
    docker run -it --rm \
        --volume=$(pwd)/sql:/sql \
        --publish=4200:4200 --publish=5432:5432 ${CRATEDB_IMAGE}

    # Populate schema and data.
    docker run --rm -it --network=host --volume=$(pwd)/sql:/sql ${CRATEDB_IMAGE} \
        sh -c 'cat /sql/schemas.sql | crash'
    docker run --rm --network=host --volume=$(pwd)/sql:/sql ${CRATEDB_IMAGE} \
        crash -c "COPY guestbook.countries FROM 'file:///sql/countries.json' RETURN SUMMARY;"

    # Validate data.
    docker run --rm -it --network=host --volume=$(pwd)/sql:/sql ${CRATEDB_IMAGE} \
        crash -c "SELECT id, name FROM guestbook.countries"


Components
==========

Frontend
--------

The frontend_ is shared by all apps and is written in JavaScript. This frontend
communicates with the backend over a `REST API`_.

Backends
--------

The are several implementations of the backend REST API:

- Python_ (using dbapi_, crate-python_)
- PHP_ (using PDO_, crate-pdo_)
- Java_ (using JDBC_, pgjdbc_)
- Java-Spring_ (using  `Spring Boot`_, `Spring Data JDBC`_)
- Erlang_ (using Erlang_, craterl_)

Contributing
============

This project is community maintained, any contributions are welcome.

See the `developer docs`_ and the `contribution docs`_ for more information.

Help
====

Looking for more help?

- Check out our `support channels`_

.. _client library: https://crate.io/docs/clients/
.. _contribution docs: CONTRIBUTING.rst
.. _crash: https://github.com/crate/crash
.. _crate-pdo: https://github.com/crate/crate-pdo
.. _crate-python: https://github.com/crate/crate-python
.. _Crate.io: https://crate.io/
.. _CrateDB: https://github.com/crate/crate
.. _craterl: https://github.com/crate/craterl
.. _dbapi: https://www.python.org/dev/peps/pep-0249/
.. _developer docs: DEVELOP.rst
.. _Erlang: erlang
.. _frontend: frontend
.. _Getting Started: https://crate.io/docs/getting-started/
.. _Java: java
.. _JDBC: https://docs.oracle.com/javase/tutorial/jdbc/
.. _Java-Spring: https://spring.io/
.. _Spring Data JDBC: https://spring.io/projects/spring-data-jdbc
.. _Spring Boot: https://spring.io/projects/spring-boot
.. _PDO: https://www.php.net/manual/en/book.pdo.php
.. _pgjdbc: https://github.com/pgjdbc/pgjdbc
.. _PHP: php
.. _Python: python
.. _REST API: https://crate.io/docs/clients/rest/
.. _support channels: https://crate.io/support/
