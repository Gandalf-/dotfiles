#!/usr/bin/env python3

import unittest
from apocrypha import Apocrypha, ApocryphaError


class TestApocrypha(unittest.TestCase):

    def test_basics(self):
        '''
        we can open the database and nothing explodes
        '''
        Apocrypha('tests/test-db.json', headless=True)

    def test_index(self):
        '''
        $ d a
        123
        '''
        a = Apocrypha('tests/test-db.json', headless=True)
        a.action(['a'])
        self.assertEqual(a.output, ['123'])

    def test_sub_index(self):
        '''
        $ d sub apple
        red
        '''
        a = Apocrypha('tests/test-db.json', headless=True)
        a.action(['sub', 'apple'])
        self.assertEqual(a.output, ['red'])

    def test_dereference(self):
        '''
        $ d one = two
        $ d two = a b c
        $ d !two
        a b c
        '''
        a = Apocrypha('tests/test-db.json', headless=True)
        a.action(['!colors'])
        self.assertEqual(a.output, ['nice'])


class TestApocryphaAssignDelete(unittest.TestCase):

    def test_assign(self):
        '''
        $ d one = two
        $ d one
        two
        '''
        a = Apocrypha('tests/test-db.json', headless=True)
        a.action(['assign', '=', '123'], read_only=True)
        self.assertEqual(a.db['assign'], '123')

    def test_delete(self):
        '''
        $ d one = two
        $ d one del
        $ d one
        '''
        a = Apocrypha('tests/test-db.json', headless=True)
        a.action(['removable', 'del'], read_only=True)
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
        a = Apocrypha('tests/test-db.json', headless=True)

        args = ['!colors', '=', 'hello']
        a.action(args, read_only=True)

        self.assertEqual(a.db['green'], 'hello')
        self.assertEqual(a.db['blue'], 'hello')
        self.assertEqual(a.db['yellow'], 'hello')

    def test_dereference_list(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        commands = [
            ['one', '=', 'two', 'three'],
            ['two', '=', 'hello'],
            ['three', '=', 'there'],
            ['!one'],
        ]

        for c in commands:
            a.action(c, read_only=True)

        self.assertEqual(a.output, ['hello', 'there'])

    def test_deep_dereference(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        commands = [
            ['one', '=', 'two three'],
            ['two', 'three', '=', 'four'],
            ['!one'],
        ]

        for c in commands:
            a.action(c, read_only=True)

        self.assertEqual(a.output, ['four'])

    def test_deep_dereference_list(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        commands = [
            ['one', '=', 'two three', 'four five'],
            ['two', 'three', '=', 'apple'],
            ['four', 'five', '=', 'pumpkin'],
            ['!one'],
        ]

        for c in commands:
            a.action(c, read_only=True)

        self.assertEqual(a.output, ['apple', 'pumpkin'])

    def test_delete_through_reference(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        args = ['!animals', 'legs', 'del']
        a.action(args, read_only=True)

        self.assertEqual(a.db['wolf'], {})
        self.assertEqual(a.db['octopus'], {})
        self.assertEqual(a.db['bird'], {})


class TestApocryphaSymlink(unittest.TestCase):

    def test_symlink(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        commands = [
            ['one', '=', '!two'],
            ['two', '=', 'three'],
            ['one']
        ]

        for c in commands:
            a.action(c, read_only=True)

        self.assertEqual(a.output, ['three'])

    def test_symlink_list(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        commands = [
            ['one', '=', '!two', '!three'],
            ['two', '=', 'apple'],
            ['three', '=', 'pumpkin'],
            ['one']
        ]

        for c in commands:
            a.action(c, read_only=True)

        self.assertEqual(a.output, ['apple', 'pumpkin'])

    def test_error_symlink_assign(self):
        '''
        cannot assign through symlinks
        '''
        a = Apocrypha('tests/test-db.json', headless=True)

        commands = [
            ['one', '=', '!two', '!three'],
            ['one', 'four', '=', 'apple'],
        ]

        with self.assertRaises(ApocryphaError):
            for c in commands:
                a.action(c, read_only=True)

    @unittest.skip
    def test_symlink_list_index(self):
        '''
        index through a list of symlink
        '''
        a = Apocrypha('tests/test-db.json', headless=True)

        commands = [
            ['one', '=', '!two', '!three'],
            ['two', 'sub', '=', 'apple'],
            ['three', 'sub', '=', 'pumpkin'],
            ['one', 'sub']
        ]

        for c in commands:
            a.action(c, read_only=True)

        self.assertEqual(a.output, ['apple', 'pumpkin'])

    def test_symlink_index(self):
        '''
        index through a symlink
        '''
        a = Apocrypha('tests/test-db.json', headless=True)

        commands = [
            ['one', '=', '!two'],
            ['two', 'sub', '=', 'apple'],
            ['one', 'sub']
        ]

        for c in commands:
            a.action(c, read_only=True)

        self.assertEqual(a.output, ['apple'])

    def test_symlink_dict(self):
        pass

    def test_symlink_recursion(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        commands = [
            ['a', 'del'],
            ['a', 'b', '=', '!b'],
        ]

        for c in commands:
            a.action(c, read_only=True)

        self.assertEqual(a.output, [])


class TestApocryphaLists(unittest.TestCase):

    def test_list_append_create(self):
        ''' appending to a key that doesn't exist yet
        '''
        a = Apocrypha('tests/test-db.json', headless=True)

        commands = [
            ['unique', '+', 'hello'],
            ['unique']
        ]

        for c in commands:
            a.action(c, read_only=True)

        self.assertEqual(a.output, ['hello'])
        self.assertTrue(isinstance(a.db['unique'], str))

    def test_list_append_create_with_space(self):
        ''' appending to a key that doesn't exist yet
        '''
        a = Apocrypha('tests/test-db.json', headless=True)

        commands = [
            ['unique', '+', 'hello there'],
            ['unique']
        ]

        for c in commands:
            a.action(c, read_only=True)

        self.assertEqual(a.output, ['hello there'])
        self.assertTrue(isinstance(a.db['unique'], str))

    def test_list_append_to_string(self):
        '''
        appending to a string (singleton value) should convert it to a list
        '''
        a = Apocrypha('tests/test-db.json', headless=True)

        commands = [
            ['unique', '=', 'hello there'],
            ['unique', '+', 'apple sauce'],
            ['unique']
        ]

        for c in commands:
            a.action(c, read_only=True)

        self.assertEqual(a.output, ['hello there', 'apple sauce'])
        self.assertTrue(isinstance(a.db['unique'], list))

    def test_list_append_to_list(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        commands = [
            ['unique', '=', 'a'],
            ['unique', '+', 'b'],
            ['unique', '+', 'c'],
            ['unique']
        ]

        for c in commands:
            a.action(c, read_only=True)

        self.assertEqual(a.output, ['a', 'b', 'c'])
        self.assertTrue(isinstance(a.db['unique'], list))

    def test_list_append_to_dict(self):
        pass

    def test_list_subtract(self):
        '''
        list of any -> list to any
        '''
        a = Apocrypha('tests/test-db.json', headless=True)

        args = ['animals', '-', 'bird']
        a.action(args, read_only=True)
        self.assertEqual(
            a.db['animals'], ['wolf', 'octopus'])

    def test_list_subtract_to_string(self):
        '''
        list of any -> string
            where len(list of any) == 2
        '''
        a = Apocrypha('tests/test-db.json', headless=True)

        args = ['list', '-', '2']
        a.action(args, read_only=True)

        self.assertEqual(a.db['list'], "1")


class TestApocryphaKeys(unittest.TestCase):

    def test_keys_on_dict(self):
        pass

    def test_keys_on_list(self):
        pass

    def test_keys_on_value(self):
        pass


class TestApocryphaSearch(unittest.TestCase):

    def test_search_list(self):
        pass

    def test_search_dict(self):
        pass

    def test_error_search_singleton(self):
        pass


class TestApocryphaErrors(unittest.TestCase):

    def test_list_subtract_error(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        args = ['list', '-', 'applesauce']

        with self.assertRaises(ApocryphaError):
            a.action(args, read_only=True)

    def test_list_subtract_non_list(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        args = ['green', '-', 'applesauce']

        with self.assertRaises(ApocryphaError):
            a.action(args, read_only=True)

    def test_index_into_value(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        args = ['green', 'nice', 'failure']

        with self.assertRaises(ApocryphaError):
            a.action(args, read_only=True)


class TestApocryphaExtensive(unittest.TestCase):

    def test_workflow_one(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        commands = [
            ['a', 'del'],
            ['a', 'b', 'c', 'd', 'e', 'f', 'g', '=', '!b'],
            ['a']
        ]

        for c in commands:
            a.action(c, read_only=True)

        # we survived
        self.assertTrue(1 == 1)


if __name__ == '__main__':
    unittest.main()
