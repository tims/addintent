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
        for a, b in links:
            self.add_concept(a)
            self.add_concept(b)
            self.add_link(a, b)

    def parents(self, concept):
        return self._relation.preimage(concept)

    def children(self, concept):
        return self._relation.image(concept)

    def add_concept(self, concept):
        self.concepts.add(concept)

    def top(self):
        concept = next(iter(self.concepts))
        parents = self.parents(concept)
        while len(parents) > 0:
            concept = next(iter(parents))
            parents = self.parents(concept)
        return concept

    def bottom(self):
        concept = next(iter(self.concepts))
        children = self.children(concept)
        while len(children) > 0:
            concept = next(iter(children))
            children = self.children(concept)
        return concept

    def add_link(self, parent, child):
        self._relation.add(parent, child)

    def remove_link(self, parent, child):
        self._relation.remove(parent, child)

    def replace_concept(self, concept, new_concept):
        parents = self.parents(concept)
        children = self.children(concept)
        self.concepts.remove(concept)
        self.add_concept(new_concept)
        for parent in set(parents):
            self.remove_link(parent, concept)
            self.add_link(parent, new_concept)
        for child in set(children):
            self.remove_link(concept, child)
            self.add_link(new_concept, child)



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
        return repr((".".join(sorted(map(str, self.extent))), ".".join(sorted(map(str, self.intent)))))


def get_maximal_generator(intent, generator_concept, lattice):
    parent_is_maximal = True
    while parent_is_maximal:
        parent_is_maximal = False
        parents = lattice.parents(generator_concept)
        for parent in parents:
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


def add_object_to_parents(obj, concept, lattice):
    new_concept = Concept(concept.extent.union(set([obj])), concept.intent)
    lattice.replace_concept(concept, new_concept)
    parents = lattice.parents(concept)
    for parent in parents:
        add_object_to_parents(obj, parent, lattice)


def add_intent(obj, intent, generator_concept, lattice):
    generator_concept = get_maximal_generator(intent, generator_concept, lattice)
    if generator_concept.intent == intent:
        new_concept = Concept(generator_concept.extent.union(set([obj])), intent)

        # TODO update parents
        lattice.replace_concept(generator_concept, new_concept)
        return new_concept

    parents = lattice.parents(generator_concept)
    new_parents = set()
    for candidate in parents:
        if not candidate.intent < intent:
            candidate = add_intent(obj, candidate.intent.intersection(intent), candidate, lattice)
        new_parents = make_new_parents(new_parents, candidate)

    new_concept = Concept(generator_concept.extent.union(set([obj])), intent)
    lattice.add_concept(new_concept)
    for parent in new_parents:
        lattice.remove_link(parent, generator_concept)
        lattice.add_link(parent, new_concept)
    lattice.add_link(new_concept, generator_concept)
    add_object_to_parents(obj, new_concept, lattice)

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
    l = create_lattice_incrementally(['a', 'b'], ['x', 'y', 'z'], rel)
    print 'FINAL LATTICE:'
    print l
