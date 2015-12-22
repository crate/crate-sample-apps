# Frontend

The frontend is written in [AngularJS](https://angularjs.org/) (1.4.8).

The single file `app.js` contains all the application logic.

## Serve application

The simplest way to serve the frontend is by using the Python
[SimpleHTTPServer](https://docs.python.org/2/library/simplehttpserver.html).

```console
$ cd frontend
$ python -m SimpleHTTPServer
```

Then open the application on port `8000` in the browser:
`http://localhost:8000/index.html`
