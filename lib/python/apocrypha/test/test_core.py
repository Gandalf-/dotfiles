#!/usr/bin/env python3

import unittest
from apocrypha.core import Apocrypha, ApocryphaError


testdb = 'test/test-db.json'


def run(commands, add_context=False):
    ''' list of list of string -> Apocrypha

    runs the provided commands on a new Apocrypha instance, and then returns
    the instance for inspection
    '''
    a = Apocrypha(testdb)
    a.add_context = add_context

    for c in commands:
        a.action(c)

    return a


class TestApocrypha(unittest.TestCase):

    def test_basics(self):
        '''
        we can open the database and nothing explodes
        '''
        Apocrypha(testdb)

    def test_no_db(self):
        a = Apocrypha('file-that-does-not-exist')
        self.assertEqual(a.db, {})

    def test_bad_db(self):
        with self.assertRaises(ApocryphaError):
            Apocrypha('test/test_core.py')

    def test_index(self):
        '''
        $ d a
        123
        '''
        a = Apocrypha(testdb)
        a.action(['a', '=', '123'])
        a.action(['a'])
        self.assertEqual(a.output, ['123'])

    def test_sub_index(self):
        '''
        $ d sub apple
        red
        '''
        a = Apocrypha(testdb)
        a.action(['sub', 'apple'])
        self.assertEqual(a.output, ['red'])

    def test_dereference(self):
        '''
        $ d one = two
        $ d two = a b c
        $ d !two
        a b c
        '''
        a = Apocrypha(testdb)
        a.action(['!colors'])
        self.assertEqual(a.output, ['nice'])

    def test_context(self):
        a = run([
            ['unique', 'two', 'three', '=', '2'],
            ['unique', 'three', 'four' '=', '2'],
            ['@', '2'],
        ], add_context=True)

        self.assertEqual(a.output, ['unique = two = three'])


class TestApocryphaAssignDelete(unittest.TestCase):

    def test_assign(self):
        '''
        $ d one = two
        $ d one
        two
        '''
        a = Apocrypha(testdb)
        a.action(['assign', '=', '123'])
        self.assertEqual(a.db['assign'], '123')

    def test_delete(self):
        '''
        $ d one = two
        $ d one --del
        $ d one
        '''
        a = Apocrypha(testdb)
        a.action(['removable', '--del'])
        self.assertFalse('removable' in a.db)

    def test_assign_through_reference(self):
        '''
        $ d one = two three
        $ d !one = four
        $ d two
        four
        $ d three
        four
        '''
        a = Apocrypha(testdb)

        args = ['!colors', '=', 'hello']
        a.action(args)

        self.assertEqual(a.db['green'], 'hello')
        self.assertEqual(a.db['blue'], 'hello')
        self.assertEqual(a.db['yellow'], 'hello')

    def test_dereference_list(self):

        a = run([
            ['one', '=', 'two', 'three'],
            ['two', '=', 'hello'],
            ['three', '=', 'there'],
            ['!one'],
        ])

        self.assertEqual(a.output, ['hello', 'there'])

    def test_deep_dereference(self):

        a = run([
            ['one', '=', 'two three'],
            ['two', 'three', '=', 'four'],
            ['!one'],
        ])

        self.assertEqual(a.output, ['four'])

    def test_deep_dereference_list(self):

        a = run([
            ['one', '=', 'two three', 'four five'],
            ['two', 'three', '=', 'apple'],
            ['four', 'five', '=', 'pumpkin'],
            ['!one'],
        ])

        self.assertEqual(a.output, ['apple', 'pumpkin'])

    def test_delete_through_reference(self):

        a = run([
            ['!animals', 'legs', '--del']
        ])

        self.assertEqual(a.db['wolf'], {})
        self.assertEqual(a.db['octopus'], {})
        self.assertEqual(a.db['bird'], {})


class TestApocryphaSymlink(unittest.TestCase):

    def test_symlink(self):

        a = run([
            ['one', '=', '!two'],
            ['two', '=', 'three'],
            ['one']
        ])

        self.assertEqual(a.output, ['three'])

    def test_symlink_list(self):
        a = run([
            ['one', '=', '!two', '!three'],
            ['two', '=', 'apple'],
            ['three', '=', 'pumpkin'],
            ['one']
        ])

        self.assertEqual(a.output, ['apple', 'pumpkin'])

    def test_error_symlink_assign(self):
        '''
        cannot assign through symlinks
        '''
        with self.assertRaises(ApocryphaError):
            run([
                ['one', '=', '!two', '!three'],
                ['one', 'four', '=', 'apple'],
            ])

    def test_symlink_list_index(self):
        '''
        index through a list of symlink
        '''
        with self.assertRaises(ApocryphaError):
            run([
                ['one', '=', '!two', '!three'],
                ['two', 'sub', '=', 'apple'],
                ['three', 'sub', '=', 'pumpkin'],
                ['one', 'sub']
            ])

    def test_symlink_index(self):
        '''
        index through a symlink
        '''
        a = run([
            ['one', '=', '!two'],
            ['two', 'sub', '=', 'apple'],
            ['one', 'sub']
        ])

        self.assertEqual(a.output, ['apple'])

    def test_symlink_dict(self):
        pass

    def test_symlink_recursion(self):
        a = run([
            ['a', '--del'],
            ['a', 'b', '=', '!b'],
        ])

        self.assertEqual(a.output, [])


class TestApocryphaAppend(unittest.TestCase):

    def test_append_create(self):
        ''' appending to a key that doesn't exist yet
        '''
        a = run([
            ['unique', '+', 'hello'],
            ['unique']
        ])

        self.assertEqual(a.output, ['hello'])
        self.assertTrue(isinstance(a.db['unique'], str))

    def test_append_create_with_space(self):
        ''' appending to a key that doesn't exist yet
        '''
        a = run([
            ['unique', '+', 'hello there'],
            ['unique']
        ])

        self.assertEqual(a.output, ['hello there'])
        self.assertTrue(isinstance(a.db['unique'], str))

    def test_append_to_string(self):
        '''
        appending to a string (singleton value) should convert it to a list
        '''
        a = run([
            ['unique', '=', 'hello there'],
            ['unique', '+', 'apple sauce'],
            ['unique']
        ])

        self.assertEqual(a.output, ['hello there', 'apple sauce'])
        self.assertTrue(isinstance(a.db['unique'], list))

    def test_append_to_list(self):
        a = run([
            ['unique', '=', 'a'],
            ['unique', '+', 'b'],
            ['unique', '+', 'c'],
            ['unique']
        ])

        self.assertEqual(a.output, ['a', 'b', 'c'])
        self.assertTrue(isinstance(a.db['unique'], list))

    def test_append_to_dict(self):
        with self.assertRaises(ApocryphaError):
            run([
                ['dict', 'a', '=', '1'],
                ['dict', 'b', '=', '1'],
                ['dict', '+', 'hello']
            ])


class TestApocryphaRemove(unittest.TestCase):

    def test_remove_string(self):
        '''
        list of any -> list to any
        '''
        a = run([
            ['list', '=', 'a', 'b', 'c'],
            ['list', '-', 'a'],
        ])

        self.assertEqual(a.db['list'], ['b', 'c'])

    def test_remove_string_to_string(self):
        '''
        '''
        a = run([
            ['list', '=', 'a', 'b', 'c'],
            ['list', '-', 'a'],
            ['list', '-', 'b'],
        ])

        self.assertEqual(a.db['list'], 'c')

    def test_remove_from_dict(self):
        '''
        '''
        with self.assertRaises(ApocryphaError):
            run([
                ['list', 'a', '=', 'a', 'b', 'c'],
                ['list', '-', 'a'],
            ])

    def test_remove_from_string(self):
        '''
        '''
        with self.assertRaises(ApocryphaError):
            run([
                ['list', '=', 'c'],
                ['list', '-', 'a'],
            ])

    def test_remove_non_existant(self):
        '''
        '''
        with self.assertRaises(ApocryphaError):
            run([
                ['new list', 'a', '=', 'a', 'b', 'c'],
                ['new list', 'a', '-', 'd'],
            ])

    def test_remove_multi(self):
        '''
        '''
        a = run([
            ['list', '=', 'a', 'b', 'c'],
            ['list', '-', 'a', 'b'],
        ])

        self.assertEqual(a.db['list'], 'c')


class TestApocryphaKeys(unittest.TestCase):

    def test_keys_on_dict(self):
        '''
        list all the keys under this index
        '''
        a = run([
            ['iron mountain', 'a', '=', '1'],
            ['iron mountain', 'b', '=', '1'],
            ['iron mountain', 'c', '=', '1'],
            ['iron mountain', '--keys']
        ])

        self.assertEqual(
            a.output, ['a', 'b', 'c'])

    def test_keys_on_list(self):
        '''
        error to request keys on a list
        '''
        with self.assertRaises(ApocryphaError):
            run([
                ['list', '=', 'a', 'b'],
                ['list', '--keys'],
            ])

    def test_keys_on_value(self):
        '''
        error to request keys on a value
        '''
        with self.assertRaises(ApocryphaError):
            run([
                ['value', '=', 'b'],
                ['value', '--keys'],
            ])


class TestApocryphaEdit(unittest.TestCase):
    '''
    d apple --edit

    without an intelligent client, this just dumps the json value of the index
    '''

    def test_edit_list(self):
        a = run([
            ['list', '=', 'a b c d'],
            ['list', '--edit']
        ])

        self.assertEqual(
            a.output, ['"a b c d"'])

    def test_edit_dict(self):
        a = run([
            ['dict', 'a', '=', '1'],
            ['dict', 'b', '=', '2'],
            ['dict', 'c', '=', '3'],
            ['dict', '--edit'],
        ])

        self.assertEqual(
            a.output, ['{\n    "a": "1",\n    "b": "2",\n    "c": "3"\n}'])

    def test_edit_singleton(self):
        a = run([
            ['single', '=', '1'],
            ['single', '--edit'],
        ])

        self.assertEqual(
            a.output, ['"1"'])

    def test_edit_none(self):
        a = run([
            ['unique key', '--edit'],
        ])

        self.assertEqual(
            a.output, ['{}'])


class TestApocryphaSet(unittest.TestCase):
    '''
    d apple --set '["a", "b", "c"]'
    '''

    def test_set_global(self):
        a = run([
            ['--set', '["a", "b", "c"]'],
            []
        ])

        self.assertEqual(
            a.output, ['a', 'b', 'c'])

    def test_set_list(self):
        a = run([
            ['list', '=', 'a b c d'],
            ['list', '--set', '["a", "b", "c"]'],
            ['list']
        ])

        self.assertEqual(
            a.output, ['a', 'b', 'c'])

    def test_set_list_of_dict(self):
        a = run([
            ['unique', '--set', '[{"a": "1"}, {"b": "2"}]'],
            ['unique']
        ])

        self.assertEqual(
            a.output, ["{'a': '1'}", "{'b': '2'}"])

    def test_set_dict(self):
        a = run([
            ['dict', '--set', '{"a":"1","b":"2"}'],
            ['dict', 'a'],
            ['dict', 'b']
        ])

        self.assertEqual(
            a.output, ["1", "2"])

    def test_set_singleton(self):
        a = run([
            ['single', '--set', '"hello"'],
            ['single']
        ])

        self.assertEqual(
            a.output, ['hello'])

    def test_set_json_error(self):
        a = Apocrypha(testdb)

        args = ['broken', '--set', 'gobbeldy gook']

        with self.assertRaises(ApocryphaError):
            a.action(args)


class TestApocryphaSearch(unittest.TestCase):

    def test_search_list(self):
        a = run([
            ['list', '=', 'haystack', 'haystack', 'needle'],
            ['other', '=', 'haystack', 'haystack'],
            ['@', 'needle'],
        ])

        self.assertEqual(
            a.output, ['list'])

    def test_search_dict(self):
        a = run([
            ['blue', 'berry', '=', 'octopus'],
            ['blue', 'cobbler', '=', 'squid'],
            ['@', 'squid'],
        ])

        self.assertEqual(
            a.output, ['cobbler'])

    def test_error_search_singleton(self):
        a = run([
            ['value', '=', 'needle'],
            ['@', 'needle'],
        ])

        self.assertEqual(
            a.output, ['value'])


class TestApocryphaErrors(unittest.TestCase):

    def test_list_subtract_error(self):
        a = Apocrypha(testdb)

        args = ['list', '-', 'applesauce']

        with self.assertRaises(ApocryphaError):
            a.action(args)

    def test_list_subtract_non_list(self):
        a = Apocrypha(testdb)

        args = ['green', '-', 'applesauce']

        with self.assertRaises(ApocryphaError):
            a.action(args)

    def test_index_into_value(self):

        with self.assertRaises(ApocryphaError):
            run([
                ['green', 'nice', 'failure']
            ])


class TestCache(unittest.TestCase):

    def test_add(self):
        a = Apocrypha(testdb)

        a.output = 'value'
        a.maybe_cache(['key'])

        self.assertEqual(
            a.cache, {('key',): 'value'})

    def test_add_deep(self):
        a = Apocrypha(testdb)

        a.output = 'value'
        a.maybe_cache(['apple', 'blue', 'berry'])

        self.assertEqual(
            a.cache, {('apple', 'blue', 'berry'): 'value'})

    def test_add_read_op(self):
        a = Apocrypha(testdb)

        a.output = 'value'
        a.maybe_cache(['key'])

        for op in Apocrypha.read_ops:
            a.maybe_cache(['apple', op])

        output = {
            ('apple', '-e'): 'value', ('apple', '--keys'): 'value',
            ('apple', '-k'): 'value', ('apple', '--edit'): 'value',
            ('key',): 'value'}

        self.assertEqual(a.cache, output)

    def test_write_op_not_added(self):
        a = Apocrypha(testdb)

        for op in Apocrypha.write_ops:
            a.maybe_cache(['apple', op, 'value'])
            self.assertEqual(a.cache, {})

    def test_invalidate_with_overwrite(self):
        a = Apocrypha(testdb)

        a.output = 'value'
        a.maybe_cache(['key'])
        self.assertEqual(a.cache, {('key',): 'value'})

        a.write_needed = True
        a.maybe_invalidate_cache(['key', '=', 'new value'])
        self.assertEqual(a.cache, {})

    def test_invalidate_with_delete(self):
        a = Apocrypha(testdb)

        a.output = 'value'
        a.maybe_cache(['key'])
        self.assertEqual(a.cache, {('key',): 'value'})

        a.write_needed = True
        a.maybe_invalidate_cache(['key', '-d'])
        self.assertEqual(a.cache, {})

    def test_invalidate_with_set(self):
        a = Apocrypha(testdb)

        a.output = 'value'
        a.maybe_cache(['key'])
        self.assertEqual(a.cache, {('key',): 'value'})

        a.write_needed = True
        a.maybe_invalidate_cache(['key', '--set', 'new value'])
        self.assertEqual(a.cache, {})

    def test_invalidate_children(self):
        a = Apocrypha(testdb)

        a.output = 'value'
        a.maybe_cache(['one', 'two three', 'four'])
        a.maybe_cache(['one', 'two three'])
        self.assertEqual(
            a.cache,
            {('one', 'two three', 'four'): 'value',
             ('one', 'two three'): 'value'})

        a.write_needed = True
        a.maybe_invalidate_cache(['one', '-d'])
        self.assertEqual(a.cache, {})

    def test_invalidate_root_child(self):
        '''
        both children of a non root key are invalidated when the parent changed
        '''
        a = Apocrypha(testdb)

        a.output = 'value'
        a.maybe_cache(['one', 'two three', 'four'])
        a.maybe_cache(['two', 'two three', 'five'])
        a.maybe_cache(['three'])

        self.assertEqual(
            a.cache,
            {('one', 'two three', 'four'): 'value',
             ('two', 'two three', 'five'): 'value',
             ('three',): 'value'})

        a.write_needed = True
        a.maybe_invalidate_cache(['one', '-d'])
        self.assertEqual(
            a.cache,
            {('two', 'two three', 'five'): 'value',
             ('three',): 'value'})

    def test_invalidate_children_non_root(self):
        '''
        both children of a non root key are invalidated when the parent changed
        '''
        a = Apocrypha(testdb)

        a.output = 'value'
        a.maybe_cache(['one', 'two three', 'four'])
        a.maybe_cache(['one', 'two three', 'five'])
        a.maybe_cache(['one'])
        self.assertEqual(
            a.cache,
            {('one', 'two three', 'four'): 'value',
             ('one', 'two three', 'five'): 'value',
             ('one',): 'value'})

        a.write_needed = True
        a.maybe_invalidate_cache(['one', 'two three', '-d'])
        self.assertEqual(a.cache, {})

    def test_invalidate_children_non_root_read_op(self):
        '''
        both children of a non root key are invalidated when the parent changed
        '''
        a = Apocrypha(testdb)

        a.output = 'value'
        a.maybe_cache(['one', 'two three', 'four'])
        a.maybe_cache(['one', '--keys'])
        a.maybe_cache(['one'])
        a.maybe_cache(['two'])

        a.write_needed = True
        a.maybe_invalidate_cache(['one', 'two three', '-d'])
        self.assertEqual(a.cache, {('two',): 'value'})

    def test_invalidate_siblings_not_effected(self):
        '''
        invalidating one child doesn't affect the other children
        '''
        a = Apocrypha(testdb)

        a.output = 'value'
        a.maybe_cache(['one', 'two three', 'four'])
        a.maybe_cache(['one', 'apple', 'sauce'])
        self.assertEqual(
            a.cache,
            {('one', 'two three', 'four'): 'value',
             ('one', 'apple', 'sauce'): 'value'})

        a.write_needed = True
        a.maybe_invalidate_cache(['one', 'two three', '-d'])
        self.assertEqual(
            a.cache, {('one', 'apple', 'sauce'): 'value'})


class TestNormalize(unittest.TestCase):

    def test_remove_empty_dict(self):
        d = {'a': {'b': {'c': {}}}}

        a = Apocrypha(testdb)

        a.normalize(d)
        self.assertEqual(d, {})

    def test_list_to_singleton(self):
        lis = {'a': ['1']}

        a = Apocrypha(testdb)

        a.normalize(lis)
        self.assertEqual(lis, {'a': '1'})


class TestApocryphaExtensive(unittest.TestCase):

    def test_workflow_one(self):
        run([
            ['a', '--del'],
            ['a', 'b', 'c', 'd', 'e', 'f', 'g', '=', '!b'],
            ['a']
        ])

        # we survived
        self.assertTrue(1 == 1)


if __name__ == '__main__':
    unittest.main()
