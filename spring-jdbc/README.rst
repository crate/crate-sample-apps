============================
Java Spring JDBC Backend App
============================

A Java backend app using `Spring Data JDBC`_ , `Spring Boot`_ and the `CrateDB JDBC driver`_ contributed by
Jürgen Mayrbäurl jurgenma@microsoft.com


Prerequisites
=============

Make sure `Java 11`_ installed.

CrateDB 4.1.4 with CrateDB JDBC driver 2.6.0 was used for testing.


Build
=====

You can build the app with `Gradle`_ like so:

.. code-block:: sh

    $ gradle clean build

Run
===

Startup CrateDB and insert the sample data:

.. code-block:: sh

    $ crash < ../sql/schemas.sql
    $ crash -c "COPY guestbook.countries FROM '$(pwd)/../sql/countries.json'"

Start up the application:

.. code-block:: sh

    $ gradle bootRun

Then, open the app:

        http://localhost:8080/

To run the application on a specific port, do this:

.. code-block:: sh

    $ gradle bootRun --args='--server.port=9000'

.. _Spring Boot: https://spring.io/projects/spring-boot
.. _Spring Data JDBC: https://spring.io/projects/spring-data-jdbc
.. _CrateDB JDBC driver: https://crate.io/docs/clients/jdbc/
.. _Java 11: https://adoptopenjdk.net/?variant=openjdk11&jvmVariant=hotspot
.. _Gradle: https://gradle.org/
