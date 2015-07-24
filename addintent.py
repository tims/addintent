from collections import defaultdict
import sys


class Relation(object):
    def __init__(self, pairs):
        self.images = defaultdict(set)
        self.preimages = defaultdict(set)
        for x, y in pairs:
            self.add(x, y)

    def add(self, x, y):
        self.images[x].add(y)
        self.preimages[y].add(x)

    def remove(self, x, y):
        try:
            self.images[x].remove(y)
            self.preimages[y].remove(x)
        except:
            pass

    def image(self, x):
        return self.images[x]

    def preimage(self, y):
        return self.preimages[y]


class Lattice(object):
    def __init__(self, links):
        self.concepts = set()
        self._relation = Relation([])
        self._bottom = None
        for a, b in links:
            self.add_concept(a)
            self.add_concept(b)
            self.add_link(a, b)

    def parents(self, concept):
        return self._relation.preimage(concept)

    def children(self, concept):
        return self._relation.image(concept)

    def add_concept(self, concept):
        if self._bottom is None:
            self._bottom = concept
        self.concepts.add(concept)

    def bottom(self):
        return self._bottom

    def add_link(self, parent, child):
        if parent == self._bottom:
            self._bottom = child
        self._relation.add(parent, child)

    def remove_link(self, parent, child):
        if child == self._bottom:
            self._bottom = parent
        self._relation.remove(parent, child)

    def __repr__(self):
        return repr(self._relation.images)


class Concept(object):
    def __init__(self, extent, intent):
        self.extent = frozenset(extent)
        self.intent = frozenset(intent)

    def __hash__(self):
        return hash((self.extent, self.intent))

    def __eq__(self, other):
        return self.__hash__() == other.__hash__()

    def __repr__(self):
        return repr((".".join(map(str, self.extent)), ".".join(map(str, self.intent))))


def get_maximal_generator(intent, generator_concept, lattice):
    parent_is_maximal = True
    while parent_is_maximal:
        parent_is_maximal = False
        parents = lattice.parents(generator_concept)
        for parent in parents:
            print intent, parent.intent
            if intent <= parent.intent:
                generator_concept = parent  # go up a level
                parent_is_maximal = True  # continue loop and check new generator's parents
    return generator_concept

def make_new_parents(parents, candidate):
    new_parents = set(parents)
    add_concept = True
    for concept in parents:
        if candidate.intent <= concept.intent:
            add_concept = False
            break
        elif concept.intent <= candidate.intent:
            new_parents.remove(concept)
    if add_concept:
        new_parents.add(candidate)
    return new_parents

def add_intent(obj, intent, generator_concept, lattice):
    print 'adding obj', obj, 'intent', intent, 'for generator concept', generator_concept
    generator_concept = get_maximal_generator(intent, generator_concept, lattice)
    if generator_concept.intent == intent:
        return generator_concept

    parents = lattice.parents(generator_concept)
    new_parents = set()
    for candidate in parents:
        if not candidate.intent < intent:
            print 'found a candidate parent', candidate, 'above', generator_concept
            print 'new intent', candidate.intent.intersection(intent)
            candidate = add_intent(obj, candidate.intent.intersection(intent), candidate, lattice)
        new_parents = make_new_parents(new_parents, candidate)

    new_concept = Concept(generator_concept.extent.union(set([obj])), intent)
    print 'new concept', new_concept
    lattice.add_concept(new_concept)
    for parent in new_parents:
        lattice.remove_link(parent, generator_concept)
        lattice.add_link(parent, new_concept)
    lattice.add_link(new_concept, generator_concept)
    print 'LATTICE:', lattice
    print 'DONE added intent', intent
    return new_concept


def create_lattice_incrementally(g, m, i):
    bottom = Concept(set(), set(m))
    lattice = Lattice([])
    lattice.add_concept(bottom)

    for obj in g:
        intent = i.image(obj)
        # should this replace the bottom concept or something? how does that work...
        add_intent(obj, intent, bottom, lattice)
        # then add obj to extent of concept and all concepts above it.
        # concept.extent.add(obj)
    return lattice


if __name__ == '__main__':
    rel = Relation([
        ('a', 'x'),
        ('a', 'y'),
        ('b', 'y'),
        ('b', 'z'),
    ])
    l = create_lattice_incrementally(['a', 'b'], ['x', 'y','z'], rel)
    print 'FINAL LATTICE:'
    print l
