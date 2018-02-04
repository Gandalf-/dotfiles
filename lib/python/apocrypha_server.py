#!/usr/bin/env python3

import apocrypha
import os
import socketserver
import time

db_path = os.path.expanduser('~') + '/.db.json'


class ApocryphaServer(apocrypha.Apocrypha):

    def __init__(self, path):
        ''' filepath -> ApocryphaServer

        @path       full path to the database json file

        add_context=False is the default because context must be requested by
        the client

        headless=True means that results get saved to self.output, which we
        return to the client instead of printing to stdout
        '''
        apocrypha.Apocrypha.__init__(self, path, headless=True)

    def action(self, args):
        ''' list of string, maybe bool -> none

        @args       arguments from the user

        This overrides the default, which saves the database after giving the
        response to the user to only give the response

        caching is not allowed for queries that include references or where
        context is requested
        '''
        if args and args[-1] == '-t':
            key = tuple(args[:-1])
            self.output = [self.timing.get(key, '0')]
            return

        key = tuple(args)

        if key in self.cache:
            self.output = self.cache[key]

        else:
            self._action(self.db, args, create=True)

            if not (self.add_context or self.dereference_occurred):
                self.cache[key] = self.output


class Handler(socketserver.BaseRequestHandler):
    '''
    The request handler class for our server.

    It is instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.
    '''

    def handle(self):
        ''' none -> none

        self.request is the TCP socket connected to the client
        '''
        db = self.server.database

        # get query, parse into arguments
        start = int(round(time.time() * 100000))
        self.data = ''
        while True:
            data = self.request.recv(1024).decode("utf-8")

            if not data:
                break
            else:
                self.data += data

        # arguments are delimited by newlines
        args = self.data.split('\n') if self.data else []
        args = [arg for arg in args if arg]

        if len(args) > 0 and args[0] == '-c':
            args = args[1:]
            db.add_context = True

        result = ''
        try:
            # send the arguments to the Apocrypha instance. we handle saving
            # the database ourselves after sending the response to the client

            db.action(args)
            result = '\n'.join(db.output)

        except apocrypha.ApocryphaError as error:
            # user, usage error
            result = str(error)

        finally:
            if result:
                result += '\n'

        # send reply to client
        self.request.sendall(result.encode('utf-8'))
        end = int(round(time.time() * 100000))

        # reset internal values, save changes if needed
        db.maybe_invalidate_cache(args)
        db.maybe_save_db()
        db.reset()

        if not self.server.quiet:
            print('{t:.5f} {c:2} {a}'
                  .format(t=(end - start) / 100000,
                          c=len(db.cache),
                          a=str(args)[:70]))


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
    host, port = '0.0.0.0', 9999
    server = Server((host, port), Handler, ApocryphaServer(db_path))

    try:
        print('starting')
        server.serve_forever()

    except KeyboardInterrupt:
        print('exiting')

    finally:
        server.shutdown()
        server.server_close()
