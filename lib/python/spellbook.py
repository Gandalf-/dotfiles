#!/usr/bin/env python3

import math

_ackerman_values = {}


def prime(n):
    ''' int -> bool

    tell if a number if prime, good for small primes
    '''
    if n % 2 == 0 and n > 2:
        return False
    return all(n % i for i in range(3, int(math.sqrt(n)) + 1, 2))


def perfect(n):
    ''' int -> bool

    tell if a number is perfect, x * x == n, x in Integers
    '''
    return math.sqrt(n) % 1 == 0


def palindrome(xs):
    ''' list -> bool
    '''
    length = len(xs)

    if length == 0 or length == 1:
        return True
    else:
        rest = xs[1:-1]
        return xs[0] == xs[-1] and palindrome(rest)


def ackerman(m, n):

    t = tuple([m, n])

    if t in _ackerman_values:
        return _ackerman_values[t]

    if m == 0:
        value = n + 1
        _ackerman_values[t] = value
        return value

    if m > 0 and n == 0:
        value = ackerman(m - 1, 1)
        _ackerman_values[t] = value
        return value

    if m > 0 and n > 0:
        left = _ackerman_values.get(
                tuple([m, n - 1]), ackerman(m, n - 1))

        value = _ackerman_values.get(
                tuple([m - 1, left]), ackerman(m - 1, left))
        _ackerman_values[t] = value
        return value


class BST(object):
    ''' binary search tree
    '''

    def __init__(self, value=None):
        self.value = value
        self.left = None
        self.right = None

    def __str__(self):
        output = []
        if self.left:
            output += [str(self.left)]

        if self.value:
            output += [str(self.value)]

        if self.right:
            output += [str(self.right)]

        return ' '.join(output)

    def insert(self, value):
        if not self.value or value == self.value:
            self.value = value

        elif value < self.value:

            if not self.left:
                self.left = BST()

            self.left.insert(value)

        else:

            if not self.right:
                self.right = BST()

            self.right.insert(value)
