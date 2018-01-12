#!/usr/bin/env python3

import json
import pprint
import sys
from os.path import expanduser

flush = False
add_context = False


def error(string):
    ''' string -> IO
    '''
    print(string)
    sys.exit(1)


def include_context():
    ''' none -> none

    signal to db_print() that we want key = value context added
    '''
    # pylint: disable=global-statement
    global add_context
    add_context = True


def flush_needed():
    ''' none -> none

    signal to main() that the database needs to be written out
    '''
    # pylint: disable=global-statement
    global flush
    flush = True


def db_print(value, context=None):
    ''' any -> IO

    figure out what a value is, and try to print it correctly
    '''
    if value:
        if context and add_context:
            print(context, end=' = ')

        if isinstance(value, str):
            print(value)

        elif isinstance(value, list):
            for elem in value:
                print(elem)

        else:
            pprint.pprint(value)


def db_dereference(keys, i, db, base, create):
    ''' list of string, int, dict, dict, bool -> None

    dereferences always start at the top level of the database, hence the
        db_action(db, db, ...)
    '''

    # the value is a pointer to a key somewhere else
    deref_args = keys[i + 1:]

    # current value is a string
    if isinstance(base, str):
        db_action(db, db, [base] + deref_args, create=create)

    # current value is iterable
    else:
        for reference in base:
            db_action(db, db, [reference] + deref_args, create=create)


def db_normalize(db):
    ''' dict -> None

    finds lists of a single element and converts them into singletons
    '''
    for k, v in db.items():
        if isinstance(v, list) and len(v) == 1:
            db[k] = v[0]

        if isinstance(v, dict):
            db_normalize(v)


def db_action(db, base, keys, create=False):
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
            flush_needed()
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
            flush_needed()
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
                error('error: cannot subtract from value that is not a list')

            try:
                if fuzzy:
                    right = [_ for _ in last_base[left] if right in _].pop()

                last_base[left].remove(right)

            except (IndexError, ValueError):
                error('error: {a} not in {b}'.format(a=right, b=left))
            else:
                flush_needed()
                return

        # wildcard, value -> key
        elif key == '@':
            for k, v in base.items():
                if v == keys[i + 1]:
                    if add_context:
                        db_print(v, context=k)
                    else:
                        db_print(k)
            return

        # remove
        elif key == 'del':
            del(last_base[keys[i - 1]])

            flush_needed()
            return

        # index
        else:

            # keep track of the level before so we can modify this level
            last_base = base
            dereference = False

            if key[0] == '!':
                # dereference result
                key = key[1:]
                dereference = True

            try:
                base = base[key]
                if not dereference:
                    continue

                db_dereference(keys, i, db, base, True)
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
    db_print(base, context)


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
    db_path = expanduser('~') + '/.db.json'
    with open(db_path, 'r') as fd:
        db = json.load(fd)

    keys = sys.argv[1:]

    if next(iter(keys), '') == '-c':
        include_context()
        keys = keys[1:]

    db_action(db, db, keys, create=True)

    if flush:
        # normalize
        db_normalize(db)

        # write the updated values back out
        with open(db_path, 'w') as fd:
            json.dump(db, fd)


if __name__ == '__main__':
    main()
