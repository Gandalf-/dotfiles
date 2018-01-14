#!/usr/bin/env python3

import apocrypha
import json
import os
import pprint
import socketserver

db_server = None
db_path = os.path.expanduser('~') + '/.db.json'


class ApocryphaServer(apocrypha.Apocrypha):

    def __init__(self, path, context=False):
        ''' filepath, maybe bool -> ApocryphaServer

        headless=True means that results get saved to self.output, which we can
        return to the client
        '''
        apocrypha.Apocrypha.__init__(
            self, path, context=context, headless=True)

    def action(self, args, read_only=False):
        ''' list of string -> None

        this overrides the default, which saves the database after giving the
        response to the user to only give the response
        '''
        self._action(self.db, self.db, args, create=True)

    def save_db(self):
        ''' None -> None

        normalize and write out the database
        '''

        if self.flush:
            self.normalize(self.db)

            # write the updated values back out
            with open(self.path, 'w') as fd:
                json.dump(self.db, fd)


class Handler(socketserver.BaseRequestHandler):
    """
    The request handler class for our server.

    It is instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.
    """
    def handle(self):
        # self.request is the TCP socket connected to the client
        self.data = self.request.recv(1024).strip().decode("utf-8")

        if self.data:
            args = self.data.split('\n')
        else:
            args = []

        print('query: ', args)

        def printer(base):
            ''' dict, list, string -> string

            recursive type aware printer
            '''
            result = ''

            if isinstance(base, str):
                result += base + '\n'

            elif isinstance(base, list):
                for elem in base:
                    result += printer(elem)

            else:
                result += pprint.pformat(base) + '\n'

            return result

        try:
            db_server.action(args, read_only=True)
            result = printer(db_server.output)

        except apocrypha.ApocryphaError as error:
            result = str(error) + '\n'

        self.request.sendall(result.encode('utf-8'))

        db_server.output = []
        db_server.save_db()


class Server(socketserver.TCPServer):
    '''
    TCPServer that allows address reuse
    '''
    allow_reuse_address = True


if __name__ == "__main__":
    HOST, PORT = "localhost", 9999

    # Create the server, binding to localhost on port 9999
    db_server = ApocryphaServer(db_path)
    server = Server((HOST, PORT), Handler)

    try:
        server.serve_forever()

    except KeyboardInterrupt:
        print('exiting')
