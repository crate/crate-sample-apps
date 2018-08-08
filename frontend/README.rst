========
Frontend
========

The frontend is written in AngularJS_ 1.4.8.

The single file, app.js_, contains all the application logic.

Serving The Application
=======================

(Please make sure that python3 is installed)

The simplest way to serve the frontend is like so:

.. code-block:: sh

    $ cd frontend
    $ python -m http.server

Then, open the application in your browser:

    http://localhost:8000/index.html

.. _AngularJS: https://angularjs.org/
.. _app.js: app.js
.. _http.server: https://docs.python.org/3/library/http.server.html
