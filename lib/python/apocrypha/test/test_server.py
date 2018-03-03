#!/usr/bin/env python3

import apocrypha.client
import threading
import unittest

from apocrypha.core import ApocryphaError
from apocrypha.server import ApocryphaServer, ApocryphaHandler, Server

db = apocrypha.client.Client()


def query(args, raw=False):
    ''' list of string -> string
    '''
    return apocrypha.client.query(
        args, port=49999, raw=raw)


class TestServer(unittest.TestCase):

    database = None
    server = None
    server_thread = None

    @classmethod
    def setUpClass(cls):
        '''
        create an Apocrypha instance and server to handle connections
        run the server in a thread so test cases may run
        '''
        # create the ApocryphaServer instance, which inherits from Apocrypha
        TestServer.database = ApocryphaServer(
                'test/test-db.json')

        # Create the tcp server
        host, port = '0.0.0.0', 49999
        TestServer.server = Server(
            (host, port), ApocryphaHandler,
            TestServer.database, quiet=True)

        # start the server
        TestServer.server_thread = threading.Thread(
                target=TestServer.server.serve_forever)

        TestServer.server_thread.daemon = True
        TestServer.server_thread.start()

        TestServer.db = apocrypha.client.Client(port=49999)

    @classmethod
    def tearDownClass(cls):
        '''
        shutdown the server
        '''
        TestServer.server.shutdown()
        TestServer.server.server_close()

    # server tests
    #   caching
    def test_cache_hit(self):

        # write operations don't update the cache
        query(['pizza', '=', 'sauce'])
        self.assertNotIn(('pizza',), TestServer.database.cache)

        # get operations do
        query(['pizza'])

        self.assertIn(('pizza',), TestServer.database.cache)
        result = query(['pizza'])

        self.assertEqual(result, ['sauce'])
        self.assertIn(('pizza',), TestServer.database.cache)

    def test_cache_deep_hit(self):
        query(['a', '-d'])
        query(['a', 'b', 'c', 'd', 'e', '=', 'f'])
        query(['a', 'b', 'c', 'd', 'e'])

        self.assertIn(
                ('a', 'b', 'c', 'd', 'e'),
                TestServer.database.cache)

    def test_cache_invalidate(self):
        query(['pizza', '=', 'sauce'])

        query(['pizza'])
        query([])
        self.assertIn(('pizza',), TestServer.database.cache)
        self.assertIn((), TestServer.database.cache)

        query(['pizza', '-d'])
        self.assertNotIn(('pizza',), TestServer.database.cache)
        self.assertNotIn((), TestServer.database.cache)

    def test_cache_invalidate_parent(self):
        '''
        changing a child key invalidates all of it's parents
        '''
        query(['one layer', 'two layer', '=', 'cake'])

        query(['one layer', 'two layer'])
        self.assertIn(('one layer', 'two layer'), TestServer.database.cache)

        query(['one layer'])
        self.assertIn(('one layer',), TestServer.database.cache)

        # both parent and child are in cache, now change the child and make
        # sure the parent is also invalidated

        query(['one layer', 'two layer', '=', 'goop'])

        self.assertNotIn(('one layer', 'two layer'), TestServer.database.cache)
        self.assertNotIn(('one layer',), TestServer.database.cache)

    def test_cache_invalidate_child(self):
        '''
        changing a parent key invalidates all of it's direct children
        '''
        query(['one layer', 'two layer', '=', 'cake'])

        query(['one layer', 'two layer'])
        self.assertIn(('one layer', 'two layer'), TestServer.database.cache)

        query(['one layer'])
        self.assertIn(('one layer',), TestServer.database.cache)

        # both parent and child are in cache, now change the parent and make
        # sure the child is also invalidated

        query(['one layer', '-d'])

        self.assertNotIn(('one layer', 'two layer'), TestServer.database.cache)
        self.assertNotIn(('one layer',), TestServer.database.cache)

    @unittest.skip('unknown issue')
    def test_cache_doesnt_effect_sibling(self):
        db.delete('one layer')

        db.set('one layer', 'two layer', value='cake')
        db.set('one layer', 'apple layer', value='sauce')
        print(TestServer.database.db)

        self.assertEqual(
            db.get('one layer', 'two layer'), 'cake')
        self.assertEqual(
            db.get('one layer', 'apple layer'), 'sauce')
        self.assertEqual(
            db.get('one layer'), {'two layer': 'cake', 'apple layer': 'sauce'})

        print(TestServer.database.cache)
        self.assertIn(('one layer',), TestServer.database.cache)
        self.assertIn(('one layer', 'two layer',), TestServer.database.cache)
        self.assertIn(('one layer', 'apple layer',), TestServer.database.cache)

    def test_cache_top_level_read_operators(self):
        '''
        make sure --keys, --edit on root are invalidated correctly
        '''
        pass

    def test_cache_top_level_write_operators(self):
        '''
        writing to root clears the entire cache
        '''
        pass

    def test_cache_write_ops_not_cached(self):
        pass

    def test_cache_read_ops_are_cached(self):
        query(['pizza', '=', 'sauce'])
        value = query(['pizza', '--edit'])

        self.assertIn(('pizza', '--edit',), TestServer.database.cache)
        self.assertEqual(value, ['"sauce"'])

    #  timing
    def test_timing(self):
        result = query(['-t', 'wolf', 'legs'])
        self.assertEqual(result, ['0'])

        query(['wolf', 'legs', '=', '4'])

        result = query(['-t', 'wolf', 'legs'])
        self.assertNotEqual(result, ['0'])

    # client tests - query
    def test_assign(self):
        query(['apple', '=', 'sauce'])
        result = query(['apple'])

        self.assertEqual(result, ['sauce'])

    def test_strict(self):
        with self.assertRaises(ApocryphaError):
            query(['-s', 'gadzooks'])

    def test_context(self):
        result = query(['-c', '@', 'red'])
        self.assertEqual(result, ['sub = apple'])

    def test_query_json_dict(self):
        result = query(['octopus'], raw=True)
        self.assertEqual(result, {'legs': 8})
        self.assertTrue(type(result) == dict)

    def test_query_json_list(self):
        result = query(['colors'], raw=True)
        self.assertTrue(type(result) == list)

    def test_query_json_string(self):
        result = query(['apple'], raw=True)
        self.assertTrue(type(result) == str)

    # client tests - Client
    def test_get_string(self):
        self.assertEqual(
            TestServer.db.get('green'), 'nice')

        self.assertEqual(
            TestServer.db.get('octopus', 'legs'), 8)

    # get
    def test_get_list(self):
        self.assertEqual(
            TestServer.db.get('animals'),
            ['wolf', 'octopus', 'bird'])

    def test_get_dict(self):
        self.assertEqual(
            TestServer.db.get('octopus'),
            {'legs': 8})

    def test_get_non_existant(self):
        self.assertEqual(
            TestServer.db.get('yahoo', 'foobar'),
            None)

    def test_get_default(self):
        '''
        when a key doesn't exist, default=<something> determines what to
        respond with
        '''
        self.assertEqual(
            TestServer.db.get('yahoo', 'foobar', default={}),
            {})

        self.assertEqual(
            TestServer.db.get('yahoo', 'foobar', default=[]),
            [])

        self.assertEqual(
            TestServer.db.get('yahoo', 'foobar', default='abc'),
            'abc')

    def test_get_error(self):
        with self.assertRaises(ApocryphaError):
            TestServer.db.get('animals', 'octopus')

    def test_get_cast_to_list(self):
        self.assertEqual(
            TestServer.db.get('green', cast=list),
            ['nice'])

    def test_get_cast_to_str(self):
        self.assertEqual(
            TestServer.db.get('animals', cast=str),
            "['wolf', 'octopus', 'bird']")

    def test_get_cast_to_set(self):
        self.assertEqual(
            TestServer.db.get('animals', cast=set),
            {'wolf', 'octopus', 'bird'})

    def test_get_cast_to_error(self):
        with self.assertRaises(ApocryphaError):
            TestServer.db.get('animals', cast=dict)

    # keys
    def test_keys(self):
        self.assertEqual(
            TestServer.db.keys('octopus'), ['legs'])

    def test_keys_non_existant(self):
        self.assertEqual(
            TestServer.db.keys('yahoo', 'foobar'), [])

    def test_keys_error(self):
        with self.assertRaises(ApocryphaError):
            TestServer.db.keys('animals', 'octopus')

    # remove
    def test_remove(self):
        TestServer.db.set('test list', value=['a', 'b', 'c'])
        TestServer.db.remove('test list', value='a')
        self.assertEqual(
            TestServer.db.get('test list'),
            ['b', 'c'])

    def test_remove_list(self):
        TestServer.db.set('test list', value=['a', 'b', 'c'])
        TestServer.db.remove('test list', value=['a', 'b'])
        self.assertEqual(
            TestServer.db.get('test list'),
            'c')

    def test_remove_error(self):
        with self.assertRaises(ApocryphaError):
            TestServer.db.remove('octopus', value='sandwich')

    # append
    def test_append(self):
        TestServer.db.delete('test list')

        TestServer.db.append('test list', value='apple')
        self.assertEqual(
            TestServer.db.get('test list'),
            'apple')

        TestServer.db.append('test list', value='blue')
        self.assertEqual(
            TestServer.db.get('test list'),
            ['apple', 'blue'])

    def test_append_list(self):
        TestServer.db.delete('test list')

        TestServer.db.append('test list', value=['a', 'b'])
        self.assertEqual(
            TestServer.db.get('test list'),
            ['a', 'b'])

        TestServer.db.append('test list', value=['c', 'd'])
        self.assertEqual(
            TestServer.db.get('test list'),
            ['a', 'b', 'c', 'd'])

    def test_append_non_existant(self):
        TestServer.db.delete('test list')

        TestServer.db.append('test list', value=['a', 'b'])
        self.assertEqual(
            TestServer.db.get('test list'),
            ['a', 'b'])

    def test_append_error(self):
        with self.assertRaises(ApocryphaError):
            TestServer.db.append('octopus', value='sandwich')

    def test_append_type_error(self):
        with self.assertRaises(ApocryphaError):
            TestServer.db.append('octopus', value={'a': 1})

    # set
    def test_set(self):

        TestServer.db.set('test item', value='hello')
        value = TestServer.db.get('test item')
        self.assertEqual(value, 'hello')

    def test_set_list(self):
        TestServer.db.set('test list', value=['hello', 'there'])
        self.assertEqual(
            TestServer.db.get('test list'),
            ['hello', 'there'])

    def test_set_error(self):
        with self.assertRaises(ApocryphaError):
            TestServer.db.set('hello', value=set())

    # delete
    def test_delete(self):
        TestServer.db.set('test item', value='hello')
        self.assertEqual(
            TestServer.db.get('test item'),
            'hello')
        TestServer.db.delete('test item')
        self.assertEqual(
            TestServer.db.get('test item'),
            None)


if __name__ == '__main__':
    unittest.main()
