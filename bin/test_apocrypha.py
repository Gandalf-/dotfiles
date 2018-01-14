#!/usr/bin/env python3

import unittest
from apocrypha import Apocrypha, ApocryphaError


class TestApocrypha(unittest.TestCase):

    def test_basics(self):
        Apocrypha('tests/test-db.json', headless=True)

    def test_index(self):
        a = Apocrypha('tests/test-db.json', headless=True)
        a.action(['a'])
        self.assertEqual(a.output, ['123'])

    def test_sub_index(self):
        a = Apocrypha('tests/test-db.json', headless=True)
        a.action(['sub', 'apple'])
        self.assertEqual(a.output, ['red'])

    def test_dereference(self):
        a = Apocrypha('tests/test-db.json', headless=True)
        a.action(['!colors'])
        self.assertEqual(a.output, ['nice', {}, {}])


class TestApocryphaAssignDelete(unittest.TestCase):

    def test_delete(self):
        a = Apocrypha('tests/test-db.json', headless=True)
        a.action(['removable', 'del'], read_only=True)
        self.assertFalse('removable' in a.db)

    def test_assign(self):
        a = Apocrypha('tests/test-db.json', headless=True)
        a.action(['assign', '=', '123'], read_only=True)
        self.assertEqual(a.db['assign'], '123')

    def test_assign_through_reference(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        args = ['!colors', '=', 'hello']
        a.action(args, read_only=True)

        self.assertEqual(a.db['green'], 'hello')
        self.assertEqual(a.db['blue'], 'hello')
        self.assertEqual(a.db['yellow'], 'hello')

    def test_dereference_list(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        args = ['!animals', 'legs']
        a.action(args, read_only=True)
        self.assertEqual(a.output, [4, 8, 2])

    def test_delete_through_reference(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        args = ['!animals', 'legs', 'del']
        a.action(args, read_only=True)

        self.assertEqual(a.db['wolf'], {})
        self.assertEqual(a.db['octopus'], {})
        self.assertEqual(a.db['bird'], {})


class TestApocryphaLists(unittest.TestCase):

    def test_list_append_create(self):
        ''' appending to a key that doesn't exist yet
        '''
        pass

    def test_list_append_to_string(self):
        '''
        appending to a string (singleton value) should convert it to a list
        '''
        a = Apocrypha('tests/test-db.json', headless=True)

        args = ['green', '+', 'foresty']
        a.action(args, read_only=True)
        self.assertEqual(a.db['green'], ['nice', 'foresty'])

    def test_list_append_to_list(self):
        a = Apocrypha('tests/test-db.json', headless=True)

        args = ['colors', '+', 'foresty']
        a.action(args, read_only=True)
        self.assertEqual(
            a.db['colors'], ['green', 'blue', 'yellow', 'foresty'])

    def test_list_append_to_hash(self):
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


if __name__ == '__main__':
    unittest.main()
