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

If you choose to use Docker for running CrateDB on your workstation, use those
commands::

    # Define CrateDB version.
    export CRATEDB_VERSION=latest
    export CRATEDB_IMAGE=crate:${CRATEDB_VERSION}

    # Start CrateDB.
    docker run -it --rm \
        --volume=$(pwd)/sql:/sql \
        --publish=4200:4200 --publish=5432:5432 \
        ${CRATEDB_IMAGE} \
        -Cdiscovery.type=single-node \
        -Ccluster.routing.allocation.disk.threshold_enabled=false

    # Populate schema and data, and verify it's there.
    docker run --rm --network=host --volume=$(pwd)/sql:/sql ${CRATEDB_IMAGE} sh -c "$(cat <<EOT
        crash < /sql/schemas.sql;
        crash <<SQL
            COPY guestbook.countries FROM 'file:///sql/countries.json' RETURN SUMMARY;
            REFRESH TABLE guestbook.countries;
            SELECT id, name FROM guestbook.countries LIMIT 10;
    SQL
    EOT)"


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
- `Java (Spark)`_ (using Spark_, JDBC_, `CrateDB JDBC driver`_)
- `Java (Spring)`_ (using  `Spring Boot`_, `Spring Data JDBC`_, `CrateDB JDBC driver`_)
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
.. _CrateDB JDBC driver: https://crate.io/docs/clients/jdbc/
.. _craterl: https://github.com/crate/craterl
.. _dbapi: https://www.python.org/dev/peps/pep-0249/
.. _developer docs: DEVELOP.rst
.. _Erlang: erlang
.. _frontend: frontend
.. _Getting Started: https://crate.io/docs/getting-started/
.. _Java (Spark): java-spark
.. _Java (Spring): java-spring
.. _JDBC: https://docs.oracle.com/javase/tutorial/jdbc/
.. _Spark: https://sparkjava.com/
.. _Spring Boot: https://spring.io/projects/spring-boot
.. _Spring Data JDBC: https://spring.io/projects/spring-data-jdbc
.. _PDO: https://www.php.net/manual/en/book.pdo.php
.. _pgjdbc: https://github.com/pgjdbc/pgjdbc
.. _PHP: php
.. _Python: python
.. _REST API: https://crate.io/docs/clients/rest/
.. _support channels: https://crate.io/support/
