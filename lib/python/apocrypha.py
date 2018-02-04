#!/usr/bin/env python3

import json
import pprint
import sys
import time


class ApocryphaError(Exception):
    '''
    used by Apocrypha.error()
    '''
    pass


class Apocrypha(object):
    '''
    A flexible, json based database that supports
    - strings, lists, dictionaries
    - references to other keys
    - arbitrary depth indexing and assignment
    - symbolic links to other keys at any level
    '''

    def __init__(self, path, headless=True):
        ''' string, maybe bool -> Apocrypha

        @path           full path to the database json file
        @headless       don't write to stdout, save in self.output
        '''
        self.add_context = False
        self.dereference_occurred = False
        self.write_needed = False
        self.headless = headless
        self.path = path

        self.output = []    # list of string
        self.cache = {}     # dict of tuple of string
        self.timing = {}    # dict of tuple of string

        try:
            with open(path, 'r+') as fd:
                self.db = json.load(fd)

        except FileNotFoundError:
            self.db = {}

        except ValueError:
            self._error('could not parse database on disk')

    def action(self, args):
        ''' list of string -> none

        may be overridden for custom behavior such as utilizing self.cache or
        self.timing
        '''
        self._action(self.db, args, create=True)

    def reset(self):
        ''' none -> none

        restore internal values to defaults, used after action()
        '''
        self.add_context = False
        self.dereference_occurred = False
        self.write_needed = False

        self.output = []

    def maybe_save_db(self):
        ''' none -> none

        Normalize and write out the database, but only if self.write_needed is
        True also clear the cache, because things may have changed
        '''

        self.normalize(self.db)

        if self.write_needed:

            # write the updated values back out
            with open(self.path, 'w') as fd:
                json.dump(self.db, fd, sort_keys=True)

    def maybe_invalidate_cache(self, args):
        ''' list of string -> none

        If a write has occurred, we may need to invalidate some entries in
        self.cache; we can invalidate only the necessary entries by inspecting
        the arguments to the query

        we also record the current time, so last modification time can be
        queried with -t

        given:
            args = devbot events update when = 0

        remove from the cache:
            devbot events update when = 0
            devbot events update when =
            ..
            devbot
            []
        '''

        if not self.write_needed:
            return

        while args:

            # also need to clear any requests for keys at each level
            for args_list in [args, args + ['-k'], args + ['--keys']]:
                args_tuple = tuple(args_list)

                self.timing[args_tuple] = str(int(time.time()))

                if args_tuple in self.cache:
                    del(self.cache[args_tuple])

            args = args[:-1]

        # always have to clear the root level, which isn't represented in the
        # input arguments list
        for args_list in [[], ['-k'], ['--keys']]:
            args_tuple = tuple(args_list)

            self.timing[args_tuple] = str(int(time.time()))

            if args_tuple in self.cache:
                del(self.cache[args_tuple])

    def normalize(self, db):
        ''' dict -> bool

        @db     level of the database to normalize

        Finds lists of a single element and converts them into singletons,

        deletes key that don't have values, returns true when a child was
        deleted so the parent knows to recheck itself

        this allows deeply nested dictionarys not ending in a value to be
        removed in one call to normalize() on the root of the database

            { a : { b : { c : {} } } } -> None
        '''

        child_removed = False

        for child, leaf in list(db.items()):

            # remove children without leaves
            if not leaf:
                del(db[child])
                child_removed = True

            tleaf = type(leaf)

            # convert lists of a single element to singletons
            if tleaf == list and len(leaf) == 1:
                db[child] = leaf[0]

            # recurse
            elif tleaf == dict:
                child_removed_child = self.normalize(leaf)

                # if our child removed a child, they may now need to be removed
                # if that was their only child; so we check ourselves again
                if child_removed_child:
                    return self.normalize(db)

        return child_removed

    def _error(self, message):
        ''' string -> none | IO

        @message    description of the error that occurred
        #impure     self.output

        Send an error to the user and stop execution. In headless mode, errors
        are appended to the class.output list
        '''
        message = 'error: ' + message

        if self.headless:
            self.output += [message]
            raise ApocryphaError(message)

        print(message)
        sys.exit(1)

    def _action(self, base, keys, create=False):
        ''' dict, list of string, maybe bool -> none

        @base   current level of the database
        @keys   keys or arguments to apply
        @create whether to allow new elements to be added

        Move through the input arguments to
            - index further into the database
            - delete keys
            - assign values to keys
        '''
        last_base = {}

        for i, key in enumerate(keys):
            left = keys[i - 1]      # string
            right = keys[i + 1:]    # list of string

            if key == '=':
                self._assign(last_base, left, right)
                return

            elif key == '+':
                self._append(last_base, left, right)
                return

            elif key == '-':
                self._remove(last_base, left, right)
                return

            elif key == '@':
                self._search(self.db, keys[i + 1], keys[:i])
                return

            elif key in ['-k', '--keys']:
                self._keys(base, left)
                return

            elif key in ['-e', '--edit']:
                self.output = [json.dumps(base, indent=4, sort_keys=True)]
                return

            elif key in ['-s', '--set']:
                self._set(last_base, left, right[0])
                return

            elif key in ['-d', '--del']:
                del(last_base[left])
                self.write_needed = True
                return

            # indexing

            # keep track of the level before so we can modify this level
            last_base = base
            key_is_reference = False
            base_is_reference = False

            if key and key[0] == '!':
                key = key[1:]
                key_is_reference = True

            if isinstance(base, str) and base[0] == '!':
                base = base[1:]
                base_is_reference = True

            try:
                if base_is_reference:
                    # we're rebasing ourselves on the dereferenced value of
                    # our current base. we keep all arguments
                    #
                    # this means that we're trying to index through a
                    # reference
                    self._dereference(base, keys[i:], create=True)
                    return

                base = base[key]

                if key_is_reference:
                    # this means we're trying to get the value of a reference
                    self._dereference(base, keys[i + 1:], True)
                    return

            except KeyError:
                if not create:
                    self._error(key + ' not found')

                # create a new key
                base[key] = {}
                base = base[key]

            except TypeError:
                self._error(
                    'cannot index through non-dict.'
                    ' {a} -> {b} -> ?, {a} :: {t}'
                    .format(a=left, b=key, t=type(base).__name__))

        self._display(base, context=' = '.join(keys[:-1]))

    def _dereference(self, base, args, create):
        ''' dict, list of string, bool -> none

        @base   current object that we're working with, corresponds to a
                "level" in the database
        @args   list of database keys to check
        @create whether or not to add new indexes to the database, if false
                will throw an error if an index that doesn't exist is accessed

        Dereferences always start at the top level of the database, hence the
            action(db, db, ...)

            $ d pointer = value
            $ d !pointer
            value

        Spaces are significant in value being treated as references. A space
        denotes a new level of indexing from the top level

            $ d pointer = 'one two'
            $ d one two = value
            $ d !pointer
            value
        '''
        self.dereference_occurred = True

        # current value is a string
        if isinstance(base, str):
            if base in self.db:
                target = [base]
            else:
                target = base.split(' ')

            self._action(
                self.db, target + args, create=create)

        # current value is iterable
        else:
            for reference in base:

                if reference in self.db:
                    target = [reference]
                else:
                    target = reference.split(' ')

                self._action(
                    self.db, target + args, create=create)

    def _display(self, value, context=None):
        ''' any, maybe string -> none

        @value      string, list or dict to add to output
        @context    additional information to include in output

        Figure out what a value is, and print it correctly, dereferences
        symlinks automatically
        '''
        if not value:
            return

        result, base = [], ''

        if context and self.add_context:
            base = context + ' = '

        # string
        if value and isinstance(value, str):
            if value[0] == '!':
                value = value[1:]
                self._dereference(value, [], False)
            else:
                result += [base + str(value)]

        # list
        elif value and isinstance(value, list):
            for elem in value:
                if elem and isinstance(elem, str) and elem[0] == '!':
                    elem = elem[1:]
                    self._dereference(elem, [], False)
                else:
                    result += [base + str(elem)]

        # dict
        else:
            result += [base + pprint.pformat(value)]

        self.output += result

    def _search(self, base, key, context):
        ''' any, string, list of string -> none

        @base       the object to search through
        @key        value to find
        @context    additional information to pass onto _display()

        Recursively search through the base dictionary, print out all the keys
        that have the given value '''

        # list
        if isinstance(base, list):
            for e in [_ for _ in base if _ == key]:
                if e == key:
                    self._display(
                        context[-1],
                        context=' = '.join(context[:-1]))
            return

        # dict
        for k, v in base.items():
            if v == key:
                self._display(
                        k,
                        context=' = '.join(context))

            elif isinstance(v, dict) or isinstance(v, list):
                self._search(v, key, context + [k])

    def _assign(self, base, left, right):
        ''' dict of any, string, list of string -> none

        direct assignment, right side may be a list or string
        '''
        # single = string, multi = list
        right = right[0] if len(right) == 1 else right

        base[left] = right
        self.write_needed = True

    def _append(self, base, left, right):
        ''' dict of any, string, list of string -> none

        append a value or values to a list or string
        may create a new value
        '''
        ltype = type(base[left])

        # creation of a new value
        if not base[left]:
            base[left] = right[0] if len(right) == 1 else right

        # value exists but is a str, create list and add
        elif ltype == str:
            base[left] = [base[left]] + right

        # left and right are lists
        elif ltype == list:
            base[left] += right

        # attempt to append to dictionary, error
        else:
            self._error('cannot append to a dictionary')

        self.write_needed = True

    def _keys(self, base, left):
        ''' any -> none

        print the keys defined at this level
        '''
        if not isinstance(base, dict):
            self._error(
                'cannot retrieve keys non-dict. {a} :: {t}'
                .format(a=left, t=type(base).__name__))

        for base_key in sorted(base.keys()):
            self._display(base_key)

    def _set(self, base, left, right):
        ''' dict of any, string, JSON string

        @base  current level of the database
        @left  index to modify
        @right new value of the index, as a JSON string

        set the entire sub tree for this value with JSON
        '''
        try:
            right = json.loads(right)

        except ValueError:
            self._error('malformed json')

        if right:

            if base:
                base[left] = right
            else:
                # global overwrite
                self.db = right

            self.write_needed = True

    def _remove(self, base, left, right):
        ''' dict of any, string, list of string

        @base  current level of the database
        @left  index to modify
        @right elements to remove from left

        remove all elements in the right from the left
        '''
        if not isinstance(base[left], list):
            self._error(
                'cannot subtract from non-list. {a} - {b}, {a} :: {t}'
                .format(a=base[left],
                        b=right,
                        t=type(base[left]).__name__))

        try:
            for r in right:
                base[left].remove(r)

        except (IndexError, ValueError):
            self._error('{a} not in {b}.'.format(a=right, b=left))

        if len(base[left]) == 1:
            base[left] = base[left][0]

        self.write_needed = True
