.. highlight: sh

====================================================================
Backend API implementation of the CrateDB guestbook demo application
====================================================================

This project is community maintained, any contributions are welcome.

A Python backend app using Flask_ and the crate_ Python package, which is an
implementation of the standard `Python DB API`_ with support for SQLAlchemy_.

Prerequisites
=============

- You will need Python 3.

Setup
=====

Set up your environment and install the dependencies::

    pip3 install poethepoet
    poe install

Run
===

Start the application::

    poe run

Then, open the application URL::

    open http://localhost:8080/


.. _Flask: https://flask.palletsprojects.com/
.. _crate: https://pypi.python.org/pypi/crate
.. _Python DB API: https://www.python.org/dev/peps/pep-0249/
.. _SQLAlchemy: https://www.sqlalchemy.org/
