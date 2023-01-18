.. highlight: sh

==========
Go backend
==========


About
=====

A Go backend application, demonstrating different features of CrateDB.
It uses both the HTTP, and the PostgreSQL wire protocol.

Both protocols are implementing the same functionality and are placed in
``http.go`` and ``postgresql.go`` files.
The protocol can be selected using the corresponding command line flags
``--http`` or ``--postgresql``.

The `Gin Web Framework`_ is used for implementing the HTTP endpoints.
For interacting with the database in PostgreSQL mode, the application
uses the excellent `pgx`_ and `pgxscan`_ libraries.
For generating unique IDs to be used as primary keys, the `ksuid`_
library is used.


Prerequisites
=============

The minimum required Go version is 1.18.

Install the application and its dependencies::

    git clone https://github.com/crate/crate-sample-apps
    cd go-gin
    go build


Run
===

This section describes how you can run the application.

To use HTTP protocol, please use::

    go run . --http

To use PostgreSQL protocol, please use::

    go run . --postgresql

Then, open the app::

    open http://localhost:8080/


Update dependencies
===================
::

    go get -t -u ./...
    go mod tidy


.. _Gin Web Framework: https://github.com/gin-gonic/gin
.. _ksuid: https://github.com/segmentio/ksuid
.. _pgx: https://github.com/jackc/pgx
.. _pgxscan: https://github.com/georgysavva/scany
