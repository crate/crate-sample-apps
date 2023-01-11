# CrateDB Python Flask Sample Application

## Installation
The Python client library for CrateDB is available on [PyPi](https://pypi.python.org/pypi/crate) and can be installed using `pip`.

```bash
pip install crate
```

The package `crate` contains both the Python Database API implementation and the CrateDB dialect for SQLAlchemy.

In this example we only cover the usage of the CrateDB client.

## Usage
### Connecting to a CrateDB Cluster
It's simple to connect to a single CrateDB node or cluster.

Import the `connect` method from the `crate.client` module and provide the `<host>:<port>` of a single CrateDB instance or a list to connect to. The port is the `HTTP` port (default `4200`) of the CrateDB node.

```python
from crate.client import connect
connection = connect('SERVER_IP:4200')
```

To comply to the [PEP 0249](https://www.python.org/dev/peps/pep-0249/) (Python Database API Specification v2.0) the `CrateConnection` object provides a cursor factory via the `cursor()` method.

```python
cursor = connection.cursor()
```

In the example application we store the connection as an app global and obtain a new cursor for each request. The cursor is set in the constructor of the `CrateResource` instance which serves as a base class for all resources.

```python
class CrateResource(Resource):

    def __init__(self):
        super(CrateResource, self).__init__()
        self.cursor = self.connection.cursor()

    @property
    def connection(self):
        if not 'conn' in app_globals:
            app_globals.conn = connect(app.config['CRATE_HOST'], error_trace=True)
        return app_globals.conn
```

To get a traceback from CrateDB in case of something wrong, we set the `error_trace` argument to `True`.

### Executing Statements
#### Executing a Single Statement
SQL statements can be executed using the `execute()` method on the cursor. Each `execute()` call results in a new HTTP request to the CrateDB Cluster. The response of the request (such as `rowcount`, `duration`, etc.) is written directly to the executing cursor object.

```python
cursor.execute("""
    CREATE TABLE guestbook.posts (
        id STRING,
        text STRING INDEX USING FULLTEXT WITH (analyzer='english'),
        user OBJECT('strict') AS (
            name STRING,
            location GEO_SHAPE
        ),
        image_ref STRING
    )""")
print('CREATE TABLE took {}ms'.format(cursor.duration))
```

The `execute()` method allows for parameter substitution. Parameters must be passed using a tuple as the second argument of the method.

```python
cursor.execute("""
    INSERT INTO guestbook.posts (
        id, text, user, image_ref
    ) VALUES (
        ?, ?, ?, ?
    )""", (id, text, user, image_ref,))
```

For the example app we compile the `INSERT` statement using the list of keys and values of a `payload` dict.

```python
class PostList(PostResource):

    def post(self):
        ...
        post_id = str(uuid.uuid1())
        values = dict(
            id = post_id,
            created = datetime.now().isoformat(),
            user = data.user,
            text = data.text,
            image_ref = data.image_ref,
            like_count = 0,
        )
        k = list(values.keys())
        v = list(values.values())
        self.cursor.execute("""INSERT INTO {} ({}) VALUES ({})""".format(
            self.__table__,
            ', '.join(k),
            ', '.join('?' * len(v))
        ), v)
        ...
```

#### Executing Multiple Statements (Bulk Operations)
Parameter substitution is also possible when executing multiple statements using the `executemany()` method. The second argument requires a list of tuples where each tuple represents the parameter of a single statement.

```python
cursor.executemany("""
    INSERT INTO guestbook.posts (
        id, text, user, image_ref
    ) VALUES (
        ?, ?, ?, ?
    )""", [
        (id1, text1, user1, image_ref1,),
        (id2, text2, user2, image_ref2,),
        ...
    ])
```

### Fetching Query Results
To fetch data from row returning statements (DQL statements) there are multiple methods:

- `fetchone()`: Fetch next row of result set
- `fetchmany()`: Fetch next x rows of result set
- `fetchall()`: Fetch all remaining rows of result set

These methods can be called after `execute()` to retrieve rows. For detailed documentation read [PEP 0249 Cursor methods](https://www.python.org/dev/peps/pep-0249/#cursor-methods).

```python
cursor.execute("""
    SELECT COUNT(*) FROM sys.nodes""")
row = cursor.fetchone()
```

The example application uses the data obtained from `fetchall()` and the `description` (which holds the column names) from the cursor and converts it into a list of dicts, which is then JSON serializable.

```python
class PostList(PostResource):

    def convert(self, description, results):
        cols = [c[0] for c in description]
        return [dict(zip(cols, r)) for r in results]

    def post(self):
        post_id = str(uuid.uuid1())
        ...
        self.cursor.execute("""
            SELECT p.*, c.name as country, c.geometry as area
            FROM guestbook.posts AS p, guestbook.countries AS c
            WHERE within(p.user['location'], c.geometry)
              AND p.id = ?
        """, (post_id,))
        response = self.convert(self.cursor.description,
                                self.cursor.fetchall())
        ...
```

### Handling BLOBs
The CrateDB Python client library also contains an API for handling BLOBs.

A blob container is an easy to use wrapper around the BLOB API of the CrateDB Cluster. It can be obtained from the `CrateConnection` object via the `get_blob_container()` method.

```python
container = connection.get_blob_container('my_blobs')
```

In the sample app the blob container is a property in the `ImageResource` class.

```python
class ImageResource(CrateResource):

    __table__ = 'guestbook_images'
    _blob_container = None

    @property
    def blob_container(self):
        if not self._blob_container:
            self._blob_container = self.connection.get_blob_container(
                self.__table__
            )
        return self._blob_container
```

The blob container provides various methods to handle blobs:

- `put()`: To create/upload blobs
- `exists()`: To verify if a blob already exists
- `get()`: To retrieve a blob
- `delete()`: To remove a blob from the database

The `post()` method on the `ImageList` class demonstrates how to generate a `sha1` digest from a JSON payload that contains a base64 encoded binary file and how to use it to create the blob on the CrateDB cluster.

```python
class ImageList(ImageResource):

    def post(self):
        ...
        tmp = base64.b64decode(data.blob)
        digest = hashlib.sha1(tmp).hexdigest()
        f = TemporaryFile()
        _ = f.write(tmp)
        f.flush()
        _ = f.seek(0)
        created = self.blob_container.put(f, digest=digest)
        ...
```

For detailed documentation of all blob container methods see [CrateDB Python Blob API](https://crate.io/docs/python/en/latest/blobs.html).

### Closing the Connection
Both the connection and the cursor can be closed on demand, rather then whenever `.__del__()` is called. Cursors and connections that have been closed cannot be used any more and will raise an error if any operation is attempted.

```python
cursor.close()
connection.close()
```
