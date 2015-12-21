# vim: set fileencodings=utf-8
# -*- coding: utf-8; -*-
#
# Licensed to CRATE Technology GmbH ("Crate") under one or more contributor
# license agreements.  See the NOTICE file distributed with this work for
# additional information regarding copyright ownership.  Crate licenses
# this file to you under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.  You may
# obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
# License for the specific language governing permissions and limitations
# under the License.
#
# However, if you have executed another commercial license agreement
# with Crate these terms will supersede the license and you may use the
# software solely pursuant to the terms of the relevant commercial agreement.

import json
import uuid
import base64
import hashlib

from datetime import datetime

from urllib.request import urlopen, Request
from urllib.error import HTTPError, URLError

from crate.client import connect

from flask import (
    Flask,
    g as app_globals,
    make_response,
)
from flask.ext.restful import Api, Resource
from flask.ext.restful.reqparse import RequestParser


# app configuration
CRATE_HOST = 'localhost:4200'
MAX_CONTENT_LENGTH = 5 * 1024 * 1024  # 5mb because GIFs are big ;)

# create Flask app
app = Flask(__name__)
app.config.from_object(__name__)


class CrateResource(Resource):

    __name__ = ''
    __table__ = ''

    def __init__(self):
        super(CrateResource, self).__init__()
        self.cursor = self.connection.cursor()

    @property
    def connection(self):
        if not 'conn' in app_globals:
            app_globals.conn = connect(app.config['CRATE_HOST'],
                                       error_trace=True)
        return app_globals.conn

    def error(self, message, status=404):
        return (dict(
            error=message,
            status=status,
        ), status)

    def refresh_table(self):
        self.cursor.execute("REFRESH TABLE {}".format(self.__table__))

    def convert(self, description, results):
        cols = [c[0] for c in description]
        return [dict(zip(cols, r)) for r in results]

    def not_found(self, **kw):
        keys = ', '.join(('{}="{}"'.format(k,v) for k,v in kw.items()))
        return self.error('{} with {} not found'.format(self.__name__, keys), 404)

    def argument_required(self, argument):
        return self.error('Argument "{}" is required'.format(argument), 400)


class PostResource(CrateResource):

    __name__ = 'Post'
    __table__ = 'guestbook.posts'


class Post(PostResource):
    """
    Resource for guestbook.posts
    Supported methods: GET, PUT, DELETE
    """

    def get(self, id):
        self.cursor.execute("""
            SELECT p.*, c.name as country, c.geometry as area
            FROM guestbook.posts AS p, guestbook.countries AS c
            WHERE within(p.user['location'], c.geometry)
              AND p.id = ?
        """, (id,))
        # convert response from Crate into
        # json-serializable object array
        response = self.convert(self.cursor.description,
                                self.cursor.fetchall())
        if self.cursor.rowcount == 1:
            return response[0], 200
        else:
            return self.not_found(id=id)

    def put(self, id):
        reqparse = RequestParser()
        reqparse.add_argument('text', type=str, location='json')
        data = reqparse.parse_args()
        if not data.text:
            return self.argument_required('text')
        self.cursor.execute("""
            UPDATE {}
            SET text = ?
            WHERE id = ?
        """.format(self.__table__), (data.text, id))
        self.refresh_table()
        return self.get(id)

    def delete(self, id):
        self.cursor.execute("""
            DELETE FROM {}
            WHERE id = ?
        """.format(self.__table__), (id,))
        if self.cursor.rowcount == 1:
            return dict(success=True), 204
        else:
            return self.not_found(id=id)


class PostList(PostResource):
    """
    Resource for guestbook.posts
    Supported methods: POST, GET
    """

    def __init__(self):
        super(PostList, self).__init__()

    def get(self):
        self.cursor.execute("""
            SELECT p.*, c.name as country, c.geometry as area
            FROM guestbook.posts AS p, guestbook.countries AS c
            WHERE within(p.user['location'], c.geometry)
        """)
        # convert response from Crate into
        # json-serializable object array
        response = self.convert(self.cursor.description,
                                self.cursor.fetchall())
        return (response, 200)

    def post(self):
        # parse incoming POST data
        reqparse = RequestParser()
        reqparse.add_argument('user', type=dict, location='json')
        reqparse.add_argument('text', type=str, location='json')
        reqparse.add_argument('image_ref', type=str, location='json')
        data = reqparse.parse_args()
        # check for required data
        if not data.text:
            return self.argument_required('text')
        if not data.get('user', {}).get('location'):
            return self.argument_required('location')
        # generate a new time based UUID
        post_id = str(uuid.uuid1())
        # payload for querying Crate
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
        # compile and execute INSERT statement
        self.cursor.execute("""
            INSERT INTO {} ({}) VALUES ({})
        """.format(self.__table__,
                   ', '.join(k),
                   ', '.join('?' * len(v))), v)
        # refresh table to make sure new record is immediately available
        self.refresh_table()
        # fetch new record
        self.cursor.execute("""
            SELECT p.*, c.name as country, c.geometry as area
            FROM guestbook.posts AS p, guestbook.countries AS c
            WHERE within(p.user['location'], c.geometry)
              AND p.id = ?
        """, (post_id,))
        # convert response from Crate into
        # json-serializable object array
        response = self.convert(self.cursor.description,
                                self.cursor.fetchall())
        return response, 200


class Like(PostResource):
    """
    Resource for guestbook.posts
    Supported methods: PUT
    """

    def put(self, id):
        self.cursor.execute("""
            UPDATE {}
            SET like_count = like_count + 1
            WHERE id = ?""".format(self.__table__), (id,))
        if self.cursor.rowcount == 1:
            self.refresh_table()
            self.cursor.execute("""
                SELECT p.*, c.name as country, c.geometry as area
                FROM guestbook.posts AS p, guestbook.countries AS c
                WHERE within(p.user['location'], c.geometry)
                  AND p.id = ?
            """, (id,))
            # convert response from Crate into
            # json-serializable object array
            response = self.convert(self.cursor.description,
                                    self.cursor.fetchall())
            if self.cursor.rowcount == 1:
                return response[0], 200
        return self.not_found(id=id)


class ImageResource(CrateResource):

    __name__ = 'Image'
    __table__ = 'blob.guestbook_images'
    CONTENT_TYPE = 'image/gif'

    def _blob_url(self, digest):
        return '{}://{}/_blobs/{}/{}'.format('http', CRATE_HOST,
                                             self.__table__.split('.')[1],
                                             digest)


class Image(ImageResource):
    """
    Resource for blob.guestbook table
    Supported methods: GET, DELETE
    """

    def get(self, digest):
        self.cursor.execute("""
            SELECT digest, last_modified
            FROM {}
            WHERE digest = ?
        """.format(self.__table__), (digest,))
        res = self.cursor.fetchall()
        if self.cursor.rowcount == 1:
            content = None
            blob_url = self._blob_url(digest)
            with urlopen(blob_url) as fp:
                content = fp.read()
            response = make_response(content, 200)
            response.headers['Content-Type'] = self.CONTENT_TYPE
            return response
        else:
            return self.not_found(digest=digest)

    def delete(self, digest):
        self.cursor.execute("""
            SELECT digest, last_modified
            FROM {}
            WHERE digest = ?
        """.format(self.__table__), (digest,))
        res = self.cursor.fetchall()
        if self.cursor.rowcount == 1:
            # generate blob url
            blob_url = self._blob_url(digest)
            # default response status
            code = 204
            try:
                # perform DELETE request on Crate
                req = Request(blob_url, method='DELETE')
                with urlopen(req) as res:
                    # set success response
                    response = dict(success=(res.status == 200))
                    code = res.status
            except HTTPError as e:
                # return error response
                return self.error(str(e), e.code)
            return response, code
        else:
            return self.not_found(digest=digest)


class ImageList(ImageResource):
    """
    Resource for blob.guestbook_images
    Supported methods: POST, GET
    """

    def __init__(self):
        super(ImageList, self).__init__()

    def get(self):
        # query all records in blob table
        self.cursor.execute("""
            SELECT digest, last_modified
            FROM {}
            ORDER BY 2 DESC
        """.format(self.__table__))
        # convert response from Crate into
        # json-serializable object array
        response = self.convert(self.cursor.description,
                                self.cursor.fetchall())
        return response, 200

    def post(self):
        # parse incoming POST data
        reqparse = RequestParser()
        reqparse.add_argument('blob', type=str, location='json')
        data = reqparse.parse_args()
        if not data.blob:
            return self.argument_required('blob')
        # encode content as bytestring
        tmp = base64.b64decode(data.blob)
        # calculate sha1 digest
        digest = hashlib.sha1(tmp).hexdigest()
        # generate blob url
        blob_url = self._blob_url(digest)
        # default response status
        code = 201
        try:
            # perform PUT request on Crate
            req = Request(blob_url, data=tmp, method='PUT')
            with urlopen(req) as res:
                # set success response
                response = dict(digest=digest, url=blob_url)
                code = res.status
        except HTTPError as e:
            # return error response
            return self.error(str(e), e.code)
        except URLError as e:
            return self.error(str(e), 500)
        return response, code


@app.errorhandler(404)
def not_found(error):
    return make_response(json.dumps({'error': 'Not found'}), 404)


def run():
    api = Api(app)
    api.add_resource(PostList, '/posts', endpoint='posts')
    api.add_resource(Post, '/post/<id>', endpoint='post')
    api.add_resource(Like, '/post/<id>/like', endpoint='like')
    api.add_resource(ImageList, '/images', endpoint='images')
    api.add_resource(Image, '/image/<digest>', endpoint='image')
    app.run(host='localhost', port=8080, debug=True)


if __name__ == '__main__':
    run()
