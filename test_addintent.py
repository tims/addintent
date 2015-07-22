import unittest

from addintent import *


class TestGetMaximalGenerator(unittest.TestCase):
    def test_bottom(self):
        bottom = Concept([], ['a'])
        lattice = Lattice([])
        self.assertEquals(get_maximal_generator(set(), bottom, lattice), bottom)
        self.assertEquals(get_maximal_generator(set('a'), bottom, lattice), bottom)
        self.assertEquals(get_maximal_generator(set('ab'), bottom, lattice), bottom)
        self.assertEquals(get_maximal_generator(set('b'), bottom, lattice), bottom)

    def test_top_bottom(self):
        top = Concept([1], 'a')
        bottom = Concept([], 'ab')
        lattice = Lattice([
            (top, bottom)
        ])
        self.assertEquals(get_maximal_generator(set(), bottom, lattice), top)
        self.assertEquals(get_maximal_generator(set('a'), bottom, lattice), top)
        self.assertEquals(get_maximal_generator(set('ab'), bottom, lattice), bottom)
        self.assertEquals(get_maximal_generator(set('b'), bottom, lattice), bottom)
        self.assertEquals(get_maximal_generator(set('c'), bottom, lattice), bottom)

    def test_diamond(self):
        top = Concept([1, 2], [])
        left = Concept([1], 'a')
        right = Concept([2], 'b')
        bottom = Concept([], 'ab')
        lattice = Lattice([
            (top, left),
            (top, right),
            (left, bottom),
            (right, bottom)
        ])

        self.assertEquals(get_maximal_generator(set(), bottom, lattice), top)
        self.assertEquals(get_maximal_generator(set('a'), bottom, lattice), left)
        self.assertEquals(get_maximal_generator(set('ac'), bottom, lattice), bottom)
        self.assertEquals(get_maximal_generator(set('b'), bottom, lattice), right)
        self.assertEquals(get_maximal_generator(set('bc'), bottom, lattice), bottom)
        self.assertEquals(get_maximal_generator(set('ab'), bottom, lattice), bottom)
        self.assertEquals(get_maximal_generator(set('c'), bottom, lattice), bottom)

class TestLattice(unittest.TestCase):
    def test_parents(self):
        top = Concept([1, 2], [])
        left = Concept([1], 'a')
        right = Concept([2], 'b')
        bottom = Concept([], 'ab')
        lattice = Lattice([
            (top, left),
            (top, right),
            (left, bottom),
            (right, bottom)
        ])
        self.assertEquals(lattice.parents(top), set())
        self.assertEquals(lattice.parents(left), set([top]))
        self.assertEquals(lattice.parents(right), set([top]))
        self.assertEquals(lattice.parents(bottom), set([left, right]))




if __name__ == '__main__':
    unittest.main()
