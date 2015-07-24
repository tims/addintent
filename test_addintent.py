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


class TestCreateLattice(unittest.TestCase):
    def test_trivial_lattice(self):
        rel = Relation([])
        l = create_lattice_incrementally([], ['a'], rel)
        self.assertEquals(l.concepts, set([Concept([], 'a')]))


class TestAddIntent(unittest.TestCase):
    def test_insert_into_middle(self):
        top = Concept([1], [])
        bottom = Concept([], ['a', 'b'])
        lattice = Lattice([
            (top, bottom)
        ])

        add_intent(2, set('b'), bottom, lattice)
        self.assertEquals(len(lattice._relation.images), 2)
        self.assertEquals(lattice.parents(bottom), set([Concept([2], ['b'])]))
        self.assertEquals(lattice.children(top), set([Concept([2], ['b'])]))

    def test_insert_into_one_side(self):
        left = Concept([1], 'ab')
        right = Concept([2], 'cd')
        bottom = Concept([], 'abcded')
        lattice = Lattice([
            (left, bottom),
            (right, bottom)
        ])
        middleright = Concept([4], 'cde')

        add_intent(4, middleright.intent, bottom, lattice)
        self.assertEquals(len(lattice._relation.images), 4)
        self.assertEquals(lattice.parents(middleright), set([right]))
        self.assertEquals(lattice.parents(bottom), set([left, middleright]))

    def test_insert_into_one_side(self):
        left = Concept([1], 'ac')
        right = Concept([2], 'bc')
        bottom = Concept([], 'abcd')
        lattice = Lattice([
            (left, bottom),
            (right, bottom)
        ])
        middleright = Concept([4], 'bcd')

        add_intent(4, middleright.intent, bottom, lattice)
        self.assertEquals(len(lattice._relation.images), 4)
        self.assertEquals(lattice.parents(middleright), set([right]))
        self.assertEquals(lattice.parents(bottom), set([left, middleright]))

        # print 'BANANAS', lattice
        # self.assertEquals(lattice.parents(bottom), set([Concept([2], ['b'])]))
        # self.assertEquals(lattice.children(top), set([Concept([2], ['b'])]))



if __name__ == '__main__':
    unittest.main()
