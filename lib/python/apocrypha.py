#!/usr/bin/env python3

import json
import pprint
import sys


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

    def __init__(self, path, add_context=False, headless=False):
        ''' string, maybe bool, maybe bool -> Apocrypha

        @path           full path to the database json file
        @add_context    add context to output
        @headless       don't write to stdout, save in self.output
        '''
        self.add_context = add_context
        self.cache = {}
        self.flush = False
        self.headless = headless
        self.output = []
        self.path = path

        try:
            with open(path, 'r+') as fd:
                self.db = json.load(fd)

        except FileNotFoundError:
            self.db = {}

        except ValueError:
            self.error('could not parse database on disk')

    def action(self, args, read_only=False):
        ''' list of string, maybe bool -> none

        @args       arguments from the user
        @read_only  do not write changes to the database file

        Perform the database actions required from the arguments, handle
        output and save changes if required
        '''
        self._action(self.db, args, create=True)

        if not self.headless:
            print('\n'.join(self.output))

        if not read_only:
            self.maybe_save_db()

    def maybe_save_db(self):
        ''' none -> none

        Normalize and write out the database, but only if self.flush is True
        also clear the cache, because things may have changed
        '''

        self.normalize(self.db)

        if self.flush:

            # write the updated values back out
            with open(self.path, 'w') as fd:
                json.dump(self.db, fd, sort_keys=True)

            self.cache = {}
            self.flush = False

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
                self.assign(last_base, left, right)
                return

            elif key == '+':
                self.append(last_base, left, right)
                return

            elif key == '-':
                self.remove(last_base, left, right)
                return

            elif key == '@':
                self.search(self.db, keys[i + 1], keys[:i])
                return

            elif key in ['-k', '--keys']:
                self.keys(base, left)
                return

            elif key in ['-e', '--edit']:
                self.output = [json.dumps(base, indent=4, sort_keys=True)]
                return

            elif key in ['-s', '--set']:
                self.set(last_base, left, right[0])
                return

            elif key in ['-d', '--del']:
                del(last_base[left])
                self.flush = True
                return

            # indexing

            # keep track of the level before so we can modify this level
            last_base = base
            key_is_reference = False
            base_is_reference = False

            if key[0] == '!':
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
                    self.dereference(base, keys[i:], create=True)
                    return

                base = base[key]

                if key_is_reference:
                    # this means we're trying to get the value of a reference
                    self.dereference(base, keys[i + 1:], True)
                    return

            except KeyError:
                if not create:
                    self.error(key + ' not found')

                # create a new key
                base[key] = {}
                base = base[key]

            except TypeError:
                self.error(
                    'cannot index through non-dict.'
                    ' {a} -> {b} -> ?, {a} :: {t}'
                    .format(a=left, b=key, t=type(base).__name__))

        self.display(base, ' = '.join(keys[:-1]))

    def dereference(self, base, args, create):
        ''' list of string, int, dict, dict, bool -> none

        @keys   list of database keys to check
        @i      position in the input keys
        @base   current object that we're working with, corresponds to a
                "level" in the database
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

    def display(self, value, context=None):
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
                self.dereference(value, [], False)
            else:
                result += [base + str(value)]

        # list
        elif value and isinstance(value, list):
            for elem in value:
                if elem and isinstance(elem, str) and elem[0] == '!':
                    elem = elem[1:]
                    self.dereference(elem, [], False)
                else:
                    result += [base + str(elem)]

        # dict
        else:
            result += [base + pprint.pformat(value)]

        self.output += result

    def search(self, base, key, context):
        ''' any, string, list of string -> none

        @base       the object to search through
        @key        value to find
        @context    additional information to pass onto display()

        Recursively search through the base dictionary, print out all the keys
        that have the given value '''

        # list
        if isinstance(base, list):
            for e in [_ for _ in base if _ == key]:
                if e == key:
                    self.display(
                        context[-1], context=' = '.join(context[:-1]))
            return

        # dict
        for k, v in base.items():
            if v == key:
                self.display(k, context=' = '.join(context))

            elif isinstance(v, dict) or isinstance(v, list):
                self.search(v, key, context + [k])

    def error(self, message):
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

        leaf_removed = False

        for key, value in list(db.items()):
            if not value:
                del(db[key])
                leaf_removed = True
                self.cache = {}

            type_key = type(value)

            if type_key == list and len(value) == 1:
                db[key] = value[0]

            elif type_key == dict:
                child_removed_value = self.normalize(value)

                if child_removed_value:
                    return self.normalize(db)

        return leaf_removed

    def assign(self, base, left, right):
        ''' dict of any, string, list of string -> none

        assignment
        '''
        # single = string, multi = list
        right = right[0] if len(right) == 1 else right

        base[left] = right
        self.flush = True

    def append(self, base, left, right):
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
            self.error('cannot append to a dictionary')

        self.flush = True

    def keys(self, base, left):
        ''' any -> none

        print the keys defined at this level
        '''
        if not isinstance(base, dict):
            self.error(
                'cannot retrieve keys non-dict. {a} :: {t}'
                .format(a=left, t=type(base).__name__))

        for base_key in sorted(base.keys()):
            self.display(base_key)

    def set(self, base, left, right):
        ''' dict of any, string, JSON string

        @base  current level of the database
        @left  index to modify
        @right new value of the index, as a JSON string

        set the entire sub tree for this value with JSON
        '''
        try:
            right = json.loads(right)

        except ValueError:
            self.error('malformed json')

        if right:

            if base:
                base[left] = right
            else:
                # global overwrite
                self.db = right

            self.flush = True

    def remove(self, base, left, right):
        ''' dict of any, string, list of string

        @base  current level of the database
        @left  index to modify
        @right elements to remove from left

        remove all elements in the right from the left
        '''
        if not isinstance(base[left], list):
            self.error(
                'cannot subtract from non-list. {a} - {b}, {a} :: {t}'
                .format(a=base[left],
                        b=right,
                        t=type(base[left]).__name__))

        try:
            for r in right:
                base[left].remove(r)

        except (IndexError, ValueError):
            self.error('{a} not in {b}.'.format(a=right, b=left))

        if len(base[left]) == 1:
            base[left] = base[left][0]

        self.flush = True
