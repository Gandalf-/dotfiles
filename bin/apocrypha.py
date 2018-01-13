#!/usr/bin/env python3

import json
import os
import pprint
import subprocess
import sys


class ApocryphaError(Exception):
    pass


class Apocrypha(object):

    type_error = 'error: cannot index into value. {a} -> {b}, {b} :: value'

    def __init__(self, path, context=False, test=False):
        ''' filepath, bool -> Apocrypha
        '''
        self.flush = False
        self.add_context = context
        self.path = path
        self.test = test
        self.output = []

        with open(path, 'r') as fd:
            self.db = json.load(fd)

    def action(self, args, read_only=False):
        ''' list of string -> None
        '''
        self._action(self.db, self.db, args, create=True)

        if not read_only and self.flush:
            self.normalize(self.db)

            # write the updated values back out
            with open(self.path, 'w') as fd:
                json.dump(self.db, fd)

    def _action(self, db, base, keys, create=False):
        ''' dict, dict, list of string -> bool

        move through the input arguments to
            - index further into the database
            - delete keys
            - assign values to keys
        '''
        last_base = {}

        for i, key in enumerate(keys):

            # assignement
            if key == '=':
                left = keys[i - 1]
                right = keys[i + 1:]

                # single = str, multi = list
                right = right[0] if len(right) == 1 else right

                last_base[left] = right
                self.flush = True
                return

            # append a value or values to a list
            elif key == '+':
                left = keys[i - 1]
                right = keys[i + 1:]

                # convert to list
                if not isinstance(last_base[left], list):
                    last_base[left] = [last_base[left]]

                # add the new element
                last_base[left] += right
                self.flush = True
                return

            # open up this level in Vim for modification
            elif key == 'edit':
                left = keys[i - 1]
                tmp = '/tmp/apocrypha-' + '-'.join(keys[:i]) + '.json'

                with open(tmp, 'w+') as f:
                    json.dump(base, f, indent=4, sort_keys=True)

                subprocess.call(['vim', tmp])

                try:
                    with open(tmp, 'r') as f:
                        right = json.load(f)

                    if right:
                        last_base[left] = right
                        self.flush = True

                finally:
                    os.remove(tmp)
                return

            # remove a value from a list
            elif key == '-':
                left = keys[i - 1]
                right = keys[i + 1]

                self.list_remove(left, right, last_base)

                if len(last_base[left]) == 1:
                    last_base[left] = last_base[left][0]

                self.flush = True
                return

            # wildcard, value -> key
            elif key == '@':
                self.search(db, keys[i + 1], keys[:i])
                return

            # remove
            elif key == 'del':
                del(last_base[keys[i - 1]])

                self.flush = True
                return

            # index
            else:

                # keep track of the level before so we can modify this level
                last_base = base
                reference = False

                if key[0] == '!':
                    # dereference result
                    key = key[1:]
                    reference = True

                try:
                    base = base[key]
                    if reference:
                        self.dereference(keys, i, db, base, True)
                        return

                except KeyError:
                    if not create:
                        self.error(key + ' not found')

                    # create a new key
                    base[key] = {}
                    base = base[key]

                except TypeError:
                    self.error(Apocrypha.type_error.format(a=base, b=key))

        context = keys[-1] if len(keys) > 0 else None
        self.display(base, context)

    def dereference(self, keys, i, db, base, create):
        ''' list of string, int, dict, dict, bool -> None

        dereferences always start at the top level of the database, hence the
            action(db, db, ...)
        '''

        # the value is a pointer to a key somewhere else
        deref_args = keys[i + 1:]

        # current value is a string
        if isinstance(base, str):
            self._action(db, db, [base] + deref_args, create=create)

        # current value is iterable
        else:
            for reference in base:
                self._action(db, db, [reference] + deref_args, create=create)

    def display(self, value, context=None):
        ''' any -> IO

        figure out what a value is, and try to print it correctly
        '''
        if not value:
            return

        if self.test:
            if self.add_context and context:
                self.output += [context + ' = ' + value]
            else:
                self.output += [value]
            return

        if context and self.add_context:
            print(context, end=' = ')

        # string
        if isinstance(value, str):
            print(value)

        # list
        elif isinstance(value, list):
            for elem in value:
                print(elem)

        # dict
        else:
            pprint.pprint(value)

    def search(self, base, key, context):
        ''' dict, string, list of string -> None

        recursively search through the base dictionary, print out all the keys
        that have the given value '''

        if isinstance(base, list):
            for e in [_ for _ in base if _ == key]:
                if e == key:
                    self.display(
                        context[-1], context=' = '.join(context[:-1]))
            return

        for k, v in base.items():
            if v == key:
                self.display(k, context=' = '.join(context))

            elif isinstance(v, dict) or isinstance(v, list):
                self.search(v, key, context + [k])

    def error(self, string):
        ''' string -> IO
        '''
        if self.test:
            self.output += [string]
            raise ApocryphaError
        else:
            print(string)
            sys.exit(1)

    def list_remove(self, left, right, base):
        ''' string, string, dict -> None
        '''
        fuzzy = False
        if right[0] == '`':
            fuzzy = True
            right = right[1:]

        if not isinstance(base[left], list):
            self.error('error: cannot subtract from non list value')

        try:
            if fuzzy:
                right = [_ for _ in base[left] if right in _].pop()

            base[left].remove(right)

        except (IndexError, ValueError):
            self.error('error: {a} not in {b}'.format(a=right, b=left))

    def normalize(self, db):
        ''' dict -> None

        finds lists of a single element and converts them into singletons
        '''
        for k, v in db.items():
            if isinstance(v, list) and len(v) == 1:
                db[k] = v[0]

            if isinstance(v, dict):
                self.normalize(v)
