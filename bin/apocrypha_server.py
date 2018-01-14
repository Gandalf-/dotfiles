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

        @path       full path to the database json file
        @context    add context to output, instead of "value", "key = value"

        headless=True means that results get saved to self.output, which we
        return to the client instead of printing to stdout
        '''
        apocrypha.Apocrypha.__init__(
            self, path, context=context, headless=True)

    def action(self, args, read_only=False):
        ''' list of string, maybe bool -> none

        @args       arguments from the user
        @read_only  do not write changes to the database file

        this overrides the default, which saves the database after giving the
        response to the user to only give the response
        '''
        self._action(self.db, self.db, args, create=True)


class Handler(socketserver.BaseRequestHandler):
    """
    The request handler class for our server.

    It is instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.
    """
    def handle(self):
        ''' none -> none

        self.request is the TCP socket connected to the client
        '''

        # get query, parse into arguments
        self.data = self.request.recv(1024).strip().decode("utf-8")
        args = self.data.split('\n') if self.data else []
        print('query: ', args)

        if len(args) > 0 and args[0] == '-c':
            args = args[1:]
            db_server.add_context = True

        result = ''
        try:
            # send the arguments to the Apocrypha instance, read_only=True
            # means that we don't write out the changes immediately

            db_server.action(args, read_only=True)
            result = '\n'.join(db_server.output)

        except apocrypha.ApocryphaError as error:
            # user, usage error
            result = str(error)

        finally:
            if result:
                result += '\n'

        # send reply to client
        self.request.sendall(result.encode('utf-8'))

        # reset output, save changes if needed
        db_server.add_context = False
        db_server.output = []
        db_server.save_db()


class Server(socketserver.TCPServer):
    '''
    TCPServer that allows address reuse
    '''
    allow_reuse_address = True


if __name__ == "__main__":
    host, port = "localhost", 9999

    # create the ApocryphaServer instance
    db_server = ApocryphaServer(db_path)

    # Create the server, binding to localhost on port 9999
    server = Server((host, port), Handler)

    try:
        server.serve_forever()

    except KeyboardInterrupt:
        print('exiting')
