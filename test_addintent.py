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

    def test_insert_into_lower_right_side(self):
        left = Concept([1], 'ab')
        right = Concept([2], 'cd')
        bottom = Concept([], 'abcded')
        lattice = Lattice([
            (left, bottom),
            (right, bottom)
        ])
        lowerright = Concept([4], 'cde')

        add_intent(4, lowerright.intent, bottom, lattice)
        self.assertEquals(len(lattice._relation.images), 4)
        self.assertEquals(lattice.parents(lowerright), set([right]))
        self.assertEquals(lattice.parents(bottom), set([left, lowerright]))

    def test_insert_into_upper_left_and_lower_right_side(self):
        bottom = Concept([], 'abcd')
        lattice = Lattice([])
        lattice.add_concept(bottom)

        add_intent(1, set(['a', 'b']), bottom, lattice)
        add_intent(2, set(['c', 'd']), bottom, lattice)

        self.assertEquals(len(lattice.concepts), 4)
        self.assertEquals(lattice.children(lattice.top()), set([Concept([1], 'ab'), Concept([2], 'cd')]))
        self.assertEquals(lattice.parents(lattice.bottom()), set([Concept([1], 'ab'), Concept([2], 'cd')]))

        add_intent(3, set('acd'), bottom, lattice)

        self.assertEquals(len(lattice.concepts), 6)
        self.assertEquals(lattice.children(lattice.top()), set([Concept([1, 3], 'a'), Concept([2], 'cd')]))
        self.assertEquals(lattice.parents(lattice.bottom()), set([Concept([1], 'ab'), Concept([3], 'acd')]))
        self.assertEquals(lattice.children(Concept([1, 3], 'a')), set([Concept([1], 'ab'), Concept([3], 'acd')]))
        self.assertEquals(lattice.parents(Concept([3], 'acd')), set([Concept([1, 3], 'a'), Concept([2], 'cd')]))

    def test_modify_existing_concept_with_same_intent(self):
        bottom = Concept([], 'ab')
        lattice = Lattice([])
        lattice.add_concept(bottom)

        add_intent(1, set(['a']), bottom, lattice)

        self.assertEquals(len(lattice.concepts), 2)
        self.assertEquals(lattice.children(lattice.top()), set([lattice.bottom()]))
        self.assertEquals(lattice.parents(lattice.bottom()), set([lattice.top()]))
        self.assertEquals(lattice.top(), Concept([1], 'a'))
        self.assertEquals(lattice.bottom(), Concept([], 'ab'))

        add_intent(2, set(['a']), bottom, lattice)

        self.assertEquals(len(lattice.concepts), 2)
        self.assertEquals(lattice.children(lattice.top()), set([lattice.bottom()]))
        self.assertEquals(lattice.parents(lattice.bottom()), set([lattice.top()]))
        self.assertEquals(lattice.top(), Concept([1, 2], 'a'))
        self.assertEquals(lattice.bottom(), Concept([], 'ab'))

    def test_make_3_branches(self):
        bottom = Concept([], 'abc')
        lattice = Lattice([])
        lattice.add_concept(bottom)

        add_intent(1, set(['a']), bottom, lattice)
        add_intent(2, set(['b']), bottom, lattice)
        add_intent(3, set(['c']), bottom, lattice)

        print lattice._relation.images
        print lattice._relation.preimages
        print lattice.concepts
        self.assertEquals(len(lattice.concepts), 5)

        self.assertEquals(lattice.top(), Concept([1, 2, 3], ''))
        self.assertEquals(lattice.bottom(), Concept([], 'abc'))


if __name__ == '__main__':
    unittest.main()
