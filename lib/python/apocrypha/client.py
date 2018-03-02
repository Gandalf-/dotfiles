#!/usr/bin/env python3

import json
import select
import socket
import subprocess
import sys
import time

from apocrypha.core import ApocryphaError


class Client(object):

    def __init__(self, host='localhost', port=9999):
        self.host = host
        self.port = port

    def get(self, *keys, default=None, cast=None):
        ''' string ..., maybe any, maybe any -> string | list | dict | none

        retrieve a given key, if the key is not found `default` will be
        returned instead

            values = db.get('devbot', 'events', default={})
            root   = db.get()
        '''
        keys = list(keys) if keys else ['']

        result = query(keys, host=self.host, port=self.port, raw=True)

        if not result:
            return default

        elif cast:
            try:
                if isinstance(result, str):
                    result = [result]
                return cast(result)

            except ValueError:
                raise ApocryphaError(
                    'error: unable to case to ' + str(cast))

        else:
            return result

    def keys(self, *keys):
        ''' string ... -> list of string | none

            keys = db.keys('devbot', 'events')
            root = db.keys()
        '''
        keys = list(keys) if keys else ['']
        return query(keys + ['--keys'], host=self.host, port=self.port)

    def delete(self, *keys):
        ''' string ... -> none
        '''
        keys = list(keys) if keys else ['']
        query(keys + ['--del'], host=self.host, port=self.port)

    def append(self, *keys, value):
        ''' string ..., str | list of str -> none
        '''
        keys = list(keys) if keys else ['']

        if isinstance(value, str):
            value = [value]

        try:
            query(keys + ['+'] + value, host=self.host, port=self.port)

        except (TypeError, ValueError):
            raise ApocryphaError('error: {v} is not a str or list')

    def remove(self, *keys, value):
        ''' string ..., str | list of str -> none
        '''
        keys = list(keys) if keys else ['']

        if isinstance(value, str):
            value = [value]

        if type(value) not in [str, list]:
            raise ApocryphaError('error: {v} is not a str or list')

        query(keys + ['-'] + value, host=self.host, port=self.port)

    def set(self, *keys, value):
        ''' string ..., string | list | dict | none -> none

        set a value for a given key, creating if necessary. can be used to
        delete keys if value={}

            events = {'key': 'value'}
            db.set('devbot', 'events', value=events)
            db.set('devbot', 'events', value={})
        '''
        keys = list(keys) if keys else ['']

        try:
            value = json.dumps(value)

            query(keys + ['--set', value],
                  host=self.host, port=self.port)

        except (TypeError, ValueError):
            raise ApocryphaError(
                'error: cannot set values that are not JSON serializable')


def query(args, host='localhost', port=9999, raw=False):
    ''' list of string -> string | dict | list

    send a query to an Apocrypha server, either returning a list of strings or
    the result of json.loads() on the result '''

    remote = (host, port)
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(remote)
    args = list(args)

    if raw and args and args[-1] not in ['-e', '--edit']:
        args += ['--edit']

    message = '\n'.join(args) + '\n'
    sock.sendall(message.encode('utf-8'))
    sock.shutdown(socket.SHUT_WR)

    result = ''
    try:
        while True:
            data = sock.recv(1024)
            if data:
                result += data.decode('utf-8')
            else:
                break

    except UnicodeDecodeError:
        result = 'error: unable to decode query result'

    finally:
        sock.close()

    result = list(filter(None, result.split('\n')))
    if result and 'error:' in result[0]:
        raise ApocryphaError(result[0])

    if raw:
        result = json.loads(''.join(result)) if result else None

    return result


def _edit_temp_file(temp_file):
    ''' string -> string

    open up the result of the query in a temporary file for manual editing.
    '''
    subprocess.call(['vim', temp_file])

    with open(temp_file, 'r') as fd:
        output = fd.read()

    try:
        output = json.dumps(json.loads(output))

    except ValueError:
        print('error: file has JSON formatting errors')
        time.sleep(1)
        _edit_temp_file(temp_file)

    else:
        return output


def main(args):
    ''' list of string -> IO

    standard query
        ... server

    interactive edit
        ... server --edit

    remote server
        ... -h remote.host.net server '''

    edit_mode = False
    host = 'localhost'

    # check for data in stdin
    if select.select([sys.stdin], [], [], 0.0)[0]:
        args += [sys.stdin.read()]

    # using a non local server
    if args and args[0] in ['-h', '--host'] and args[1]:
        host = args[1]
        args = args[2:]

    # check for edit mode before we make the query
    if args and args[-1] in ['-e', '--edit']:
        edit_mode = True
        temp_file = '/tmp/apocrypha-' + '-'.join(args[:-1]) + '.json'

    try:
        result = query(args, host=host)
        result = '\n'.join(result)

    except ConnectionRefusedError:
        print('error: could not connect to server')
        sys.exit(1)

    # interactive edit
    if edit_mode:
        with open(temp_file, 'w+') as fd:
            fd.write(result)

        output = _edit_temp_file(temp_file)
        query(args[:-1] + ['--set', output], host=host)

    # result to console
    else:
        print(result)


if __name__ == '__main__':
    main(sys.argv[1:])
