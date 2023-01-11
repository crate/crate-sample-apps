.. highlight: sh

==================
Python Backend App
==================

This project is community maintained, any contributions are welcome.

A Python backend app using Flask_ and the crate_ Python package, which is an
implementation of the standard `Python DB API`_ with support for SQLAlchemy_.

Prerequisites
=============

- You will need Python 3.

Setup
=====

Set up your environment and install the dependencies::

    python3 -m venv .venv
    source .venv/bin/activate
    pip install -r requirements.txt

Run
===

Start the application::

    python app.py

Then, open the application URL::

    open http://localhost:8080/


.. _Flask: https://flask.palletsprojects.com/
.. _crate: https://pypi.python.org/pypi/crate
.. _Python DB API: https://www.python.org/dev/peps/pep-0249/
.. _SQLAlchemy: https://www.sqlalchemy.org/
