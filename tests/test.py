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

import os
import sys
import json
import base64
import unittest
from argparse import ArgumentParser
from urllib.request import urlopen, Request
from urllib.error import HTTPError

COLOR_TEMPL = '\033[{}m{}\033[0m'


def red(t):
    return COLOR_TEMPL.format(31, t)

def green(t):
    return COLOR_TEMPL.format(32, t)

def yellow(t):
    return COLOR_TEMPL.format(33, t)

def blue(t):
    return COLOR_TEMPL.format(34, t)


def parametrize(klass, options):
    testloader = unittest.TestLoader()
    testnames = testloader.getTestCaseNames(klass)
    suite = unittest.TestSuite()
    for name in testnames:
        suite.addTest(klass(options, name))
    return suite


class CrateTestCase(unittest.TestCase):

    def __init__(self, options, methodName=None):
        super(CrateTestCase, self).__init__(methodName)
        self.options = options
        self.options.fqn = '{0}://{1}:{2}'.format(options.protocol,
                                                  options.host,
                                                  options.port)

    def get_url(self, path):
        return '{}{}'.format(self.options.fqn, path)

    def req(self, method, path, data=None, content_type=None):
        payload = None
        headers = {
            'Accept': '*/*',
            'User-Agent': 'CrateTestCase/0.1',
        }
        if data and type(data) == dict:
            payload = json.dumps(data).encode('utf-8')
            headers.update({
                'Accept': 'application/json',
                'Content-Type': 'application/json',
            })
        print('\n')
        print('>>> {} {}'.format(blue(method), path))
        r = Request(self.get_url(path), data=payload,
                    method=method, headers=headers)
        try:
            with urlopen(r) as response:
                print(' -> {}'.format(green(response.code)))
                return response.read().decode('utf-8'), response.status
        except HTTPError as e:
            res = e.fp.read().decode('utf-8')
            print(' -> {}'.format(red(e.code)))
            print(' -> {}'.format(yellow(res)))
            return res, e.code

    def assertPostNotFound(self, res, code,
                           id='does-not-exist'):
        d = json.loads(res)
        self.assertEqual(code, 404)
        self.assertEqual(d['error'],
                         'Post with id="{}" not found'.format(id))
        self.assertEqual(d['status'], 404)

    def assertImageNotFound(self, res, code,
                            digest='0000000000000000000000000000000000000000'):
        d = json.loads(res)
        self.assertEqual(code, 404)
        self.assertEqual(d['error'],
                         'Image with digest="{}" not found'.format(digest))
        self.assertEqual(d['status'], 404)

    def assertArgumentRequired(self, res, code, a):
        d = json.loads(res)
        self.assertEqual(code, 400)
        self.assertEqual(d['error'], 'Argument "{}" is required'.format(a))
        self.assertEqual(d['status'], 400)


class PostsTestCase(CrateTestCase):

    def test_posts(self):
        post_id = self._create_post()
        self._retrieve_post(post_id)
        self._update_post(post_id)
        self._like_post(post_id)
        self._delete_post(post_id)

    def _create_post(self):
        # request without POST data
        res, code = self.req('POST', '/posts')
        self.assertArgumentRequired(res, code, 'text')

        # request with POST data
        payload = dict(
            text = 'Far far away, behind the word mountains ...',
            user = dict(
                name = 'Alex',
                location = [9.74379 , 47.4124],
            )
        )
        res, code = self.req('POST', '/posts', data=payload)
        d = json.loads(res)
        self.assertEqual(code, 200)
        self.assertEqual(len(d), 1)
        self.assertEqual(d[0]['user']['name'], payload['user']['name'])
        self.assertEqual(d[0]['text'], payload['text'])
        self.assertEqual(d[0]['image_ref'], None)
        self.assertEqual(d[0]['like_count'], 0)

        return d[0]['id']

    def _retrieve_post(self, post_id):
        res, code = self.req('GET', '/post/{}'.format(post_id))
        d = json.loads(res)
        self.assertEqual(code, 200)
        self.assertEqual(d['id'], post_id)
        self.assertEqual(d['country'], 'Austria')
        self.assertEqual(d['area']['type'], 'Polygon')
        self.assertEqual(len(d['area']['coordinates'][0]), 37)

        # fetch post with invalid id
        res, code = self.req('GET', '/post/{}'.format('does-not-exist'))
        self.assertPostNotFound(res, code)

        # fetch all posts
        res, code = self.req('GET', '/posts')
        d = json.loads(res)
        self.assertEqual(code, 200)
        self.assertTrue(len(d) >= 1)

    def _update_post(self, post_id):
        res, code = self.req('PUT', '/post/{}'.format(post_id), data=None)
        self.assertArgumentRequired(res, code, 'text')

        payload = dict(
            text = 'not a lorem ipsum'
        )
        res, code = self.req('PUT', '/post/{}'.format(post_id), data=payload)
        d = json.loads(res)
        self.assertEqual(code, 200)
        self.assertEqual(d['text'], payload['text'])

    def _like_post(self, post_id):
        for x in range(3):
            res, code = self.req('PUT', '/post/{}/like'.format(post_id))
            d = json.loads(res)
            self.assertEqual(code, 200)
            self.assertEqual(d['like_count'], x+1)

        res, code = self.req('PUT', '/post/{}/like'.format('does-not-exist'))
        self.assertPostNotFound(res, code)

    def _delete_post(self, post_id):
        res, code = self.req('DELETE', '/post/{}'.format(post_id))
        self.assertEqual(code, 204)

        res, code = self.req('DELETE', '/post/{}'.format(post_id))
        self.assertPostNotFound(res, code, id=post_id)


class ImagesTestCase(CrateTestCase):

    def test_images(self):
        digest, url = self._create_image()
        self._retrieve_image(digest)
        self._delete_image(digest)

    def _create_image(self):
        res, code = self.req('POST', '/images')
        self.assertArgumentRequired(res, code, 'blob')

        payload = dict()
        with open('awsm.gif', 'rb') as fp:
            payload.update(
                blob = base64.b64encode(fp.read()).decode('utf-8')
            )
        res, code = self.req('POST', '/images', data=payload)
        d = json.loads(res)
        self.assertEqual(code, 201)
        self.assertEqual(d['digest'],
                         '27ee51bf00e6d7379b902c9ee05ec9f9d56722b4')
        self.assertEqual(d['url'],
                         'http://localhost:4200/_blobs/guestbook_images/27ee51bf00e6d7379b902c9ee05ec9f9d56722b4')

        res, code = self.req('POST', '/images', data=payload)
        self.assertEqual(code, 409)

        return d['digest'], d['url']

    def _retrieve_image(self, digest):
        with urlopen(self.get_url('/image/{}'.format(digest))) as response:
            self.assertEqual(response.status, 200)
            h = response.headers
            self.assertEqual(h['Content-Type'], 'image/gif')
            self.assertEqual(h['Content-Length'], '1702902')

        invalid_digest = '0' * 40
        res, code = self.req('GET', '/image/{}'.format(invalid_digest))
        self.assertImageNotFound(res, code, digest=invalid_digest)

    def _delete_image(self, digest):
        res, code = self.req('DELETE', '/image/{}'.format(digest))
        self.assertEqual(code, 204)

        res, code = self.req('DELETE', '/image/{}'.format(digest))
        self.assertImageNotFound(res, code, digest=digest)


def parse_args():
    parser = ArgumentParser('Universal tests for example app backends')
    parser.add_argument('--port', type=int, default=8080,
                        help='HTTP port on which the backend is listening.')
    parser.add_argument('--host', type=str, default='localhost',
                        help='Hostname or IP on which the backend is listening.')
    parser.add_argument('--protocol', type=str, default='http',
                        help='HTTP protocol of the backend.')
    args = parser.parse_args()
    return args


def main():
    args = parse_args()
    wd = os.getcwd()
    suite = unittest.TestSuite()
    suite.addTest(parametrize(PostsTestCase, options=args))
    suite.addTest(parametrize(ImagesTestCase, options=args))
    unittest.TextTestRunner(verbosity=2).run(suite)


if __name__ == '__main__':
    sys.exit(main())
