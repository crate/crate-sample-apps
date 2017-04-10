===================
CrateDB Sample Apps
===================

Overview
========

A JavaScript guestbook app with a number of different backend implementations,
each using a different `client library`_ to communicate with CrateDB_.

Prerequisites
=============

You need install and run CrateDB 0.54.1 or higher before you set things up.

If you're using the Java backend, you will need 0.57.0 or higher.

You will also need to install crash_ (the CrateDB CLI tool) to work with CrateDB
on the command line.

See the CrateDB `Getting Started`_ guide for help.

Setup
=====

Once CrateDB instance is running, create the required schema and import the
country data:

.. code-block:: sh

    $ crash < sql/schemas.sql
    $ crash -c "COPY guestbook.countries FROM '$(pwd)/sql/countries.json'"

Components
==========

Frontend
--------

The  frontend_ is shared by all apps and is written in JavaScript. This frontend
communicates with the backend over a `REST API`_.

Backends
--------

The are several implementations of the backend REST API:

- Python_ (using dbapi_, crate-python_)
- PHP_ (using PDO_, crate-pdo_)
- Java_ (using JDBC_, pgjdbc_)
- Erlang_ (using Erlang_, craterl_)

Contributing
============

This project is primarily maintained by Crate.io_, but we welcome community
contributions!

See the `developer docs`_ and the `contribution docs`_ for more information.

Help
====

Looking for more help?

- Check `StackOverflow`_ for common problems
- Chat with us on `Slack`_
- Get `paid support`_

.. _client library: https://crate.io/docs/clients/
.. _contribution docs: CONTRIBUTING.rst
.. _crash: https://github.com/crate/crash
.. _crate-pdo: https://github.com/crate/crate-pdo
.. _crate-python: https://github.com/crate/crate-python
.. _Crate.io: http://crate.io/
.. _CrateDB: https://github.com/crate/crate
.. _craterl: https://github.com/crate/craterl
.. _dbapi: https://www.python.org/dev/peps/pep-0249/
.. _developer docs: DEVELOP.rst
.. _Erlang: erlang
.. _frontend: frontend
.. _Getting Started: https://crate.io/docs/getting-started/
.. _Java: java
.. _JDBC: http://www.oracle.com/technetwork/java/overview-141217.html
.. _paid support: https://crate.io/pricing/
.. _PDO: http://at2.php.net/manual/en/book.pdo.php
.. _pgjdbc: https://github.com/pgjdbc/pgjdbc
.. _PHP: php
.. _Python: python
.. _REST API: https://crate.io/docs/clients/rest/
.. _Slack: https://crate.io/docs/support/slackin/
.. _StackOverflow: https://stackoverflow.com/tags/crate
