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
        self.images[x].remove(y)
        self.preimages[y].remove(x)

    def image(self, x):
        return self.images[x]

    def preimage(self, y):
        return self.preimages[y]


class Lattice(object):
    def __init__(self, links):
        self.concepts = set()
        self._relation = Relation([])
        for a, b in links:
            self.add_concept(a)
            self.add_concept(b)
            self.add_link(a, b)

    def parents(self, concept):
        return self._relation.preimage(concept)

    def add_concept(self, concept):
        self.concepts.add(concept)

    def add_link(self, parent, child):
        self._relation.add(parent, child)

    def remove_link(self, parent, child):
        self._relation.remove(parent, child)

    def __repr__(self):
        return repr(self._relation.images)


class Concept(object):
    def __init__(self, extent, intent):
        self.extent = set(extent)
        self.intent = set(intent)

    def __repr__(self):
        return repr(("".join(map(str, self.extent)), "".join(map(str, self.intent))))


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


def add_intent(intent, generator_concept, lattice):
    print 'adding intent', intent, 'for generator concept', generator_concept
    generator_concept = get_maximal_generator(intent, generator_concept, lattice)
    if generator_concept.intent == intent:
        return generator_concept

    parents = lattice.parents(generator_concept)
    new_parents = set()
    for candidate in parents:
        if not candidate.intent < intent:
            print 'found a candidate parent', candidate, 'above', generator_concept
            print 'new intent', candidate.intent.intersection(intent)
            candidate = add_intent(candidate.intent.intersection(intent), candidate, lattice)

        add_parent = True
        for parent in new_parents:
            if candidate.intent <= parent.intent:
                add_parent = False
            elif parent.intent <= candidate.intent:
                new_parents.remove(parent)

        if add_parent:
            new_parents.add(candidate)

    new_concept = Concept(generator_concept.extent, intent)

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
    top = Concept(set(g), set())
    lattice = Lattice([])
    lattice.add_concept(bottom)
    lattice.add_concept(top)
    lattice.add_link(top, bottom)

    for obj in g:
        intent = i.preimage(obj)
        # should this replace the bottom concept or something? how does that work...
        concept = add_intent(intent, bottom, lattice)

        # then add obj to extent of concept and all concepts above it.
    return lattice


if __name__ == '__main__':
    rel = Relation([
        (1, 1),
        (1, 2),
        (2, 2),
        (2, 3),
        (3, 3)
    ])
    create_lattice_incrementally([1, 2, 3], [1, 2, 3], rel)
