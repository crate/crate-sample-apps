.. highlight:: sh

========
Frontend
========

- The frontend is written in `AngularJS`_ 1.4.8.
- Python 3 is needed to run a minimal web server, `http.server`_.
- The single file, `app.js`_, contains all the application logic.


Serving the application
=======================

(Please make sure that python3 is installed)

Serve the frontend::

    cd frontend
    python3 -m http.server

Then, open the application in your browser::

    open http://localhost:8000/index.html


.. _AngularJS: https://angularjs.org/
.. _app.js: app.js
.. _http.server: https://docs.python.org/3/library/http.server.html
