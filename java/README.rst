================
Java Backend App
================

A Java backend app using Spark_ and the `CrateDB JDBC driver`_.

Prerequisites
=============

You will need `Java 8`_ and Apache Maven installed.

You will need CrateDB 0.57.0 or higher.

Build
=====

You can build the app like so:

.. code-block:: sh

    $ mvn clean install

Run
===

You can the app like so:

.. code-block:: sh

    $ mvn exec:java -Dexec.mainClass="io.crate.jdbc.sample.App"

Then, open the app:

    http://localhost:8080/

To run the application on a specific port, do this:

.. code-block:: sh

    $ mvn exec:java -Dexec.mainClass="io.crate.jdbc.sample.App" -Dexec.args="8080"

.. _Spark: http://sparkjava.com/
.. _CrateDB JDBC driver: https://crate.io/docs/clients/jdbc/
.. _Java 8: http://www.oracle.com/technetwork/java/javase/overview/java8-2100321.html
.. _Apache Maven: https://maven.apache.org/index.html
