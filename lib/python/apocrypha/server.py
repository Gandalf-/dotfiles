#!/usr/bin/env python3

import apocrypha.core as apocrypha
import os
import socketserver
import time


class ApocryphaServer(apocrypha.Apocrypha):

    def __init__(self, path):
        ''' filepath -> ApocryphaServer

        @path       full path to the database json file
        '''
        apocrypha.Apocrypha.__init__(self, path)

    def action(self, args):
        ''' list of string, maybe bool -> none

        @args       arguments from the user

        This overrides the default, which saves the database after giving the
        response to the user to only give the response

        caching is not allowed for queries that include references or where
        context is requested
        '''
        # request for timing information
        if args and args[0] == '-t':
            key = tuple(args[1:])
            self.output = self.timing.get(key, '0') + '\n'
            return

        # all other queries
        key = tuple(args)

        if key in self.cache:
            self.output = self.cache[key]

        else:
            self._action(self.db, args)

            if self.output:
                self.output = '\n'.join(self.output) + '\n'
            else:
                self.output = ''

            self.maybe_cache(args)


class ApocryphaHandler(socketserver.BaseRequestHandler):
    '''
    The request handler class for our server.

    It is instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.
    '''

    def parse_arguments(self):

        # arguments are delimited by newlines, remove empty elements
        args = self.data.split('\n') if self.data else []
        args = list(filter(None, args))

        while args and args[0] in {'-c', '--context', '-s', '--strict'}:

            if args[0] == '-c':
                self.server.database.add_context = True

            if args[0] == '-s':
                self.server.database.strict = True

            args = args[1:]

        return args

    def handle(self):
        ''' none -> none

        self.request is the TCP socket connected to the client
        '''
        db = self.server.database
        milliseconds = 10 ** 5

        # get query, parse into arguments
        start_time = int(round(time.time() * milliseconds))
        self.data = ''
        while True:
            data = self.request.recv(1024).decode('utf-8')

            if not data:
                break
            else:
                self.data += data

        args = self.parse_arguments()

        result = ''
        try:
            db.action(args)
            result = db.output

        except apocrypha.ApocryphaError as error:
            # user, usage error
            result = str(error)

        # send reply to client
        self.request.sendall(result.encode('utf-8'))

        end_time = int(round(time.time() * milliseconds))
        query_duration = (end_time - start_time) / milliseconds

        # reset internal values, save changes if needed
        db.maybe_invalidate_cache()
        db.maybe_save_db()
        db.reset()

        if not self.server.quiet:
            print('{t:.5f} {c:2} {a}'
                  .format(t=query_duration, c=len(db.cache), a=str(args)[:70]))


class Server(socketserver.TCPServer):
    ''' none -> socketserver.TCPServer

    allow address reuse for faster restarts
    '''
    allow_reuse_address = True

    def __init__(self, server_address, RequestHandlerClass,
                 database, quiet=False):
        socketserver.TCPServer.__init__(
            self, server_address, RequestHandlerClass)
        self.database = database
        self.quiet = quiet


if __name__ == '__main__':

    # Create the tcp server
    host = '0.0.0.0'
    port = 9999
    db_path = os.path.expanduser('~') + '/.db.json'

    server = Server(
        (host, port),
        ApocryphaHandler,
        ApocryphaServer(db_path))

    try:
        print('starting')
        server.serve_forever()

    except KeyboardInterrupt:
        print('exiting')

    finally:
        server.shutdown()
        server.server_close()
