#!/usr/bin/env python3

from apocrypha.client import Client
from apocrypha.server import ApocryphaServer, ApocryphaHandler, Server

import threading
import unittest


class TestClient(unittest.TestCase):

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
        TestClient.database = ApocryphaServer(
                'test/test-db.json')

        # Create the tcp server
        host, port = '0.0.0.0', 49999
        TestClient.server = Server(
            (host, port), ApocryphaHandler,
            TestClient.database, quiet=True)

        # start the server
        TestClient.server_thread = threading.Thread(
                target=TestClient.server.serve_forever)

        TestClient.server_thread.daemon = True
        TestClient.server_thread.start()

        TestClient.db = Client(port=49999)

    @classmethod
    def tearDownClass(cls):
        '''
        shutdown the server
        '''
        TestClient.server.shutdown()
        TestClient.server.server_close()

    def test_get(self):
        self.assertEqual(
            TestClient.db.get('green'), 'nice')


if __name__ == '__main__':
    unittest.main()
