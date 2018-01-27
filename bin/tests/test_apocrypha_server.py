#!/usr/bin/env python3

import unittest
import threading
import socket
import apocrypha_server


class TestServer(unittest.TestCase):

    database = None
    server = None
    server_thread = None

    @classmethod
    def setUpClass(cls):
        # create the ApocryphaServer instance, which inherits from Apocrypha
        TestServer.database = apocrypha_server.ApocryphaServer(
                'resources/test-db.json')

        # Create the tcp server
        host, port = '0.0.0.0', 49999
        TestServer.server = apocrypha_server.Server(
            (host, port), apocrypha_server.Handler,
            TestServer.database, quiet=True)

        # start the server
        TestServer.server_thread = threading.Thread(
                target=TestServer.server.serve_forever)

        TestServer.server_thread.daemon = True
        TestServer.server_thread.start()

    @classmethod
    def tearDownClass(cls):
        TestServer.server.shutdown()
        TestServer.server.server_close()

    def communicate(self, args):
        ''' list of string -> string

        send some arguments to the server and receive a reply
        '''
        remote = ('localhost', 49999)
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect(remote)

        query = '\n'.join(args) + '\n'
        sock.sendall(query.encode('utf-8'))
        sock.shutdown(socket.SHUT_WR)

        result = ''
        while True:
            data = sock.recv(1024)

            if not data:
                sock.close()
                break
            else:
                try:
                    result += data.decode('utf-8')

                except UnicodeDecodeError:
                    result = 'ERROR'

        return result

    def test_assign(self):
        self.communicate(['apple', '=', 'sauce'])
        result = self.communicate(['apple'])
        self.assertEqual(result, 'sauce\n')

    def test_cache_hit(self):
        self.communicate(['apple', '=', 'sauce'])
        result = self.communicate(['apple'])
        result = self.communicate(['apple'])
        self.assertEqual(result, 'sauce\n')
        self.assertTrue(len(TestServer.database.cache) > 0)


if __name__ == '__main__':
    unittest.main()
