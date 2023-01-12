.. highlight:: sh

===============
Developer guide
===============


About
=====

This section of the documentation describes the API specification document, the
corresponding test cases, and outlines how to provision a CrateDB sandbox
environment using Docker.


API specification
=================

The HTTP API of all the applications hosted within this repository is described
within the `Backend API specification for the CrateDB guestbook demo application`_
document.


Integration tests
=================

You can use a set of test cases to validate a backend application for
compatibility with the specification. To invoke those integration tests, run::

    cd tests
    python3 test.py --host localhost --port 8080


Docker setup
============

If you choose to use Docker for running CrateDB on your workstation, use those
commands to run and provision your database instance::

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


Development
===========

You can submit HTTP requests from the console to the backend service,
for example by using the excellent `HTTPie`_ program::

    http localhost:8080/posts user:='{"name": "John Doe", "location": [9.744417, 47.413417]}' text="Hello, world." --print HBhb

In order to debug the HTTP conversations, you can easily watch the HTTP traffic
to the backend application, and to CrateDB, using the excellent ``ngrep`` program::

    # Frontend <-> Backend
    sudo ngrep -d lo0 -Wbyline port 8080

    # Backend <-> CrateDB
    sudo ngrep -d lo0 -Wbyline port 4200

Please make sure to adjust the ``-d`` option according to your needs,
in order to select the appropriate network device on your system.


.. _Backend API specification for the CrateDB guestbook demo application: SPEC.md
