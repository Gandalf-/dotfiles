#!/usr/bin/env python3

import json
import os
import pprint
import subprocess
import sys

flush = False
add_context = False


def error(string):
    ''' string -> IO
    '''
    print(string)
    sys.exit(1)


class Apocrypha(object):

    def __init__(self, path, context=False):
        ''' filepath, bool -> Apocrypha
        '''
        self.flush = False
        self.add_context = context
        self.path = path

        with open(path, 'r') as fd:
            self.db = json.load(fd)

    def action(self, args):
        ''' dict, list of string -> None
        '''
        self._action(self.db, self.db, args, create=True)

        if self.flush:
            # normalize
            normalize(self.db)

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
                    last_base[left] = list(last_base[left])

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

                fuzzy = False
                if right[0] == '`':
                    fuzzy = True
                    right = right[1:]

                if not isinstance(last_base[left], list):
                    error(
                        'error: cannot subtract from non list value')

                try:
                    if fuzzy:
                        right = \
                            [_ for _ in last_base[left] if right in _].pop()

                    last_base[left].remove(right)

                except (IndexError, ValueError):
                    error('error: {a} not in {b}'.format(a=right, b=left))
                else:
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
                    if not reference:
                        continue

                    self.dereference(keys, i, db, base, True)
                    return

                except KeyError:
                    if not create:
                        print(key + ' not found')
                        return

                    # create a new key
                    base[key] = {}
                    base = base[key]

                except TypeError:
                    error(
                        'error: cannot index into value. {a} -> {b}, {b} :: value'
                        .format(a=base, b=key))

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
        if value:
            if context and self.add_context:
                print(context, end=' = ')

            if isinstance(value, str):
                print(value)

            elif isinstance(value, list):
                for elem in value:
                    print(elem)

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


def normalize(db):
    ''' dict -> None

    finds lists of a single element and converts them into singletons
    '''
    for k, v in db.items():
        if isinstance(v, list) and len(v) == 1:
            db[k] = v[0]

        if isinstance(v, dict):
            normalize(v)


def main():
    ''' string, ... -> IO

    db apple colors = blue red
    db red = 123
    db blue = 456

    db apple colors
    db apple !colors

    db apple colors + green yellow
    db apple colors - `een

    db -c @ 456
    '''

    # load the database
    path = os.path.expanduser('~') + '/.db.json'
    args = sys.argv[1:]
    context = False

    if next(iter(args), '') == '-c':
        context = True
        args = args[1:]

    apocrypha = Apocrypha(path, context=context)
    apocrypha.action(args)


if __name__ == '__main__':
    main()
