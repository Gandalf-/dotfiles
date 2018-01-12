#!/usr/bin/env python3

import json
import pprint
import sys
from os.path import expanduser

flush = False
add_context = False


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
                print(elem, end=' ')
            print('')

        else:
            pprint.pprint(value)


def db_dereference(args, i, db, base, create):
    ''' list of string, int, dict, dict, bool -> None

    dereferences always start at the top level of the database, hence the
        db_action(db, db, ...)
    '''

    # the value is a pointer to a key somewhere else
    deref_args = args[i + 1:]

    # current value is a string
    if isinstance(base, str):
        db_action(db, db, [base] + deref_args, create=create)

    # current value is iterable
    else:
        for reference in base:
            db_action(db, db, [reference] + deref_args, create=create)


def db_action(db, base, args, create=False):
    ''' dict, dict, list of string -> bool

    move through the input arguments to
        - index further into the database
        - delete keys
        - assign values to keys
    '''
    assignee = False
    last_base = {}

    for i, arg in enumerate(args):

        # assignement
        if arg == '=':
            assignee = args[i - 1]

            remaining_args = args[i + 1:]

            if len(remaining_args) == 1:
                # single element
                last_base[assignee] = args[i + 1]
            else:
                # list assignement
                last_base[assignee] = remaining_args

            flush_needed()
            return

        # wildcard, value -> key
        elif arg == '@':
            for k, v in base.items():
                if v == args[i + 1]:
                    if add_context:
                        db_print(v, context=k)
                    else:
                        db_print(k)
            return

        # remove
        elif arg == 'del':
            del(last_base[args[i - 1]])

            flush_needed()
            return

        # index
        else:

            # keep track of the level before so we can modify this level
            last_base = base

            dereference = False
            key = arg

            if arg[0] == '!':
                # dereference result
                key = arg[1:]
                dereference = True

            try:
                base = base[key]
                if not dereference:
                    continue

                db_dereference(args, i, db, base, True)
                return

            except KeyError:
                if not create:
                    print(key + ' not found')
                    return

                # create a new key
                base[arg] = {}
                base = base[arg]

            except TypeError:
                print(
                    'error: cannot index into value. {a} -> {b}, {b} :: value'
                    .format(a=base, b=arg))
                return

    db_print(base, args[-1])


def main():
    ''' string, ... -> IO

    db apple colors = blue red
    db red = 123
    db blue = 456

    db apple colors
    db apple !colors

    db -c @ 456
    '''

    # load the database
    db_path = expanduser('~') + '/.db.json'
    with open(db_path, 'r') as fd:
        db = json.load(fd)

    args = sys.argv[1:]

    if args[0] == '-c':
        # pylint: disable=global-statement
        global add_context
        add_context = True
        args = args[1:]

    db_action(db, db, args, create=True)

    if flush:
        # write the updated values back out
        with open(db_path, 'w') as fd:
            json.dump(db, fd)


if __name__ == '__main__':
    main()
