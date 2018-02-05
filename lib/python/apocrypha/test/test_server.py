#!/usr/bin/env python3

import apocrypha.client
import json
import threading
import unittest

from apocrypha.server import ApocryphaServer, ApocryphaHandler, Server


def query(args, produce_json=False):
    ''' list of string -> string
    '''
    return apocrypha.client.query(
        args, port=49999, produce_json=produce_json)


class TestServer(unittest.TestCase):

    database = None
    server = None
    server_thread = None

    @classmethod
    def setUpClass(cls):
        '''
        create an Apocrypha instance and server to handle connections
        run the server in a thread so test cases may run
        '''
        # create the ApocryphaServer instance, which inherits from Apocrypha
        TestServer.database = ApocryphaServer(
                'test/test-db.json')

        # Create the tcp server
        host, port = '0.0.0.0', 49999
        TestServer.server = Server(
            (host, port), ApocryphaHandler,
            TestServer.database, quiet=True)

        # start the server
        TestServer.server_thread = threading.Thread(
                target=TestServer.server.serve_forever)

        TestServer.server_thread.daemon = True
        TestServer.server_thread.start()

    @classmethod
    def tearDownClass(cls):
        '''
        shutdown the server
        '''
        TestServer.server.shutdown()
        TestServer.server.server_close()

    def test_assign(self):
        query(['apple', '=', 'sauce'])
        result = query(['apple'])

        self.assertEqual(result, ['sauce'])

    def test_cache_hit(self):
        query(['pizza', '=', 'sauce'])
        query(['pizza'])
        result = query(['pizza'])

        self.assertEqual(result, ['sauce'])
        self.assertTrue(len(TestServer.database.cache) > 0)

    def test_cache_invalidate(self):
        query(['pizza', '=', 'sauce'])

        query(['pizza'])
        query([])
        self.assertIn(('pizza',), TestServer.database.cache)
        self.assertIn((), TestServer.database.cache)

        query(['pizza', '-d'])
        self.assertNotIn(('pizza',), TestServer.database.cache)
        self.assertNotIn((), TestServer.database.cache)

    def test_strict(self):
        result = query(['-s', 'gadzooks'])

        self.assertEqual(result, ['error: gadzooks not found'])

    def test_context(self):
        result = query(['-c', '@', 'red'])
        self.assertEqual(result, ['sub = apple'])

    def test_query_json(self):
        result = query(['octopus'], produce_json=True)
        self.assertEqual(result, {'legs': 8})

    def test_timing(self):
        result = query(['-t', 'wolf', 'legs'])
        self.assertEqual(result, ['0'])

        query(['wolf', 'legs', '=', '4'])

        result = query(['-t', 'wolf', 'legs'])
        self.assertNotEqual(result, ['0'])


if __name__ == '__main__':
    unittest.main()
