==================
Python Backend App
==================

A Python backend app using Flask_ and the crate_ Python library, which is an
implementation of the standard `Python DB API`_ with support for SQLAlchemy_.

Prerequisites
=============

You will need Python 3 (>3.2) and virtualenv_.

Setup
=====

Set up your environment and install the dependencies like this:

.. code-block:: sh

    $ virtualenv env
    $ source ./env/bin/activate
    $ ./env/bin/pip install -r requirements.txt

Run
===

You can run the app like so:

.. code-block:: sh

    $ ./env/bin/python app.py

.. _Flask: http://flask.pocoo.org/
.. _crate: https://pypi.python.org/pypi/crate
.. _Python DB API: https://www.python.org/dev/peps/pep-0249/
.. _virtualenv: https://virtualenv.readthedocs.org/en/latest/
.. _SQLAlchemy: http://www.sqlalchemy.org/
