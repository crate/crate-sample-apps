============================
Java Spring JDBC Backend App
============================

A Java backend app using `Spring Data JDBC`_ , `Spring Boot`_ and the `CrateDB JDBC driver`_.

Prerequisites
=============

You will just have `Java 11`_ installed.

CrateDB 4.1.4 with CrateDB JDBC driver 2.6.0 was used for testing.

Build
=====

You can build the app with `Apache Maven`_ like so:

.. code-block:: sh

    $ mvn clean install

Run
===

You can the app like so:

.. code-block:: sh

    $ mvn spring-boot:run

Then, open the app:

    http://localhost:8080/

To run the application on a specific port, do this:

.. code-block:: sh

    $ mvn spring-boot:run -Drun.arguments="--server.port=9000"

.. _Spring Boot: https://spring.io/projects/spring-boot
.. _Spring Data JDBC: https://spring.io/projects/spring-data-jdbc
.. _CrateDB JDBC driver: https://crate.io/docs/clients/jdbc/
.. _Java 11: https://adoptopenjdk.net/?variant=openjdk11&jvmVariant=hotspot
.. _Apache Maven: https://maven.apache.org/index.html
