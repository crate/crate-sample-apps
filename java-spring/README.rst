.. highlight:: sh

============================
Java Spring JDBC Backend App
============================

A Java backend app using `Spring Data JDBC`_ , `Spring Boot`_ and the
`CrateDB JDBC driver`_ contributed by Jürgen Mayrbäurl <jurgenma@microsoft.com>.


Prerequisites
=============

- Make sure `Java 11`_ installed.
- CrateDB 4.1.4 with CrateDB JDBC driver 2.6.0 was used for testing.


Build
=====

Build the application with `Gradle`_::

    ./gradlew clean build

Run
===

Start the application::

    ./gradlew bootRun

Then, open the application URL::

    open http://localhost:8080/

Run the application on a specific port::

    gradle bootRun --args='--server.port=9000'


.. _CrateDB JDBC driver: https://crate.io/docs/clients/jdbc/
.. _Gradle: https://gradle.org/
.. _Java 11: https://adoptopenjdk.net/?variant=openjdk11&jvmVariant=hotspot
.. _Spring Boot: https://spring.io/projects/spring-boot
.. _Spring Data JDBC: https://spring.io/projects/spring-data-jdbc
