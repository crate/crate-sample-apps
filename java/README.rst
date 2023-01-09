.. highlight:: sh

================
Java Backend App
================

A backend application written in Java, using Spark_ and the `CrateDB JDBC driver`_.


Prerequisites
=============

- You will need `Java 8`_ and Apache Maven installed.
- You will need CrateDB 0.57.0 or higher

Build
=====

Build the application::

    mvn clean install

Run
===

Invoke the application::

    mvn exec:java -Dexec.mainClass="io.crate.jdbc.sample.App"

Then, open the application::

    open http://localhost:8080/

Run the application on a specific port::

    mvn exec:java -Dexec.mainClass="io.crate.jdbc.sample.App" -Dexec.args="8080"


.. _Apache Maven: https://maven.apache.org/index.html
.. _CrateDB JDBC driver: https://crate.io/docs/clients/jdbc/
.. _Java 8: https://www.oracle.com/java/technologies/java8.html
.. _Spark: https://sparkjava.com/
