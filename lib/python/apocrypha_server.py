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
        apocrypha.Apocrypha.__init__(
            self, path, add_context=False, headless=True)

    def action(self, args, read_only=False):
        ''' list of string, maybe bool -> none

        @args       arguments from the user
        @read_only  do not write changes to the database file

        This overrides the default, which saves the database after giving the
        response to the user to only give the response
        '''
        key = ''.join(args)

        if key in self.cache:
            self.output = self.cache[key]

        else:
            self._action(self.db, args, create=True)
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
            self.server.database.add_context = True

        result = ''
        try:
            # send the arguments to the Apocrypha instance, read_only=True
            # means that we don't write out the changes immediately. we handle
            # saving the database ourselves after sending the response to the
            # client

            self.server.database.action(args, read_only=True)
            result = '\n'.join(self.server.database.output)

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
        self.server.database.add_context = False
        self.server.database.output = []
        self.server.database.maybe_save_db()

        if not self.server.quiet:
            print('{t:.5f} {c:2} {a}'
                  .format(t=(end - start) / 100000,
                          c=len(self.server.database.cache),
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
