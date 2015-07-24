from addintent import *
from flask import *
import json
from flask_cors import CORS

app = Flask(__name__)
cors = CORS(app)


def init_lattice():
    bottom = Concept(set(), set(range(100)))
    l = Lattice([])
    l.add_concept(bottom)
    return l


lattice = init_lattice()
# add_intent('1', set('x'), lattice.bottom(), lattice)
# add_intent('2', set('yfe'), lattice.bottom(), lattice)
# add_intent('3', set('af'), lattice.bottom(), lattice)
# add_intent('4', set('c'), lattice.bottom(), lattice)


# This the last line in this setup causes, RuntimeError: Set changed size during iteration
# add_intent(1, set([1]), lattice.bottom(), lattice)
# add_intent(2, set([2, 6, 5]), lattice.bottom(), lattice)
# add_intent(3, set([3, 6]), lattice.bottom(), lattice)
# add_intent(4, set([4, 5]), lattice.bottom(), lattice)
# add_intent(5, set([1, 2, 5]), lattice.bottom(), lattice)

objects = {}
attributes = {}


def object_id(label):
    if objects.get(label, False):
        return objects[label]
    objects[label] = len(objects) + 1
    return objects[label]


def attribute_id(label):
    if attributes.get(label, False):
        return attributes[label]
    attributes[label] = len(attributes) + 1
    return attributes[label]


def objects_from_ids(ids):
    object_lookup = {v: k for k, v in objects.items()}
    return list(map(lambda o: object_lookup.get(o, o), ids))


def attributes_from_ids(ids):
    attributes_lookup = {v: k for k, v in attributes.items()}
    return list(map(lambda a: attributes_lookup.get(a, a), ids))


@app.route("/")
def show():
    nodes = []
    links = []
    i = 0
    node_lookup = {}
    for parent in sorted(lattice.concepts, lambda x, y: sorted(x.extent) < sorted(y.extent)):
        node_lookup[parent.__hash__()] = i
        nodes.append({
            'extent': objects_from_ids(parent.extent),
            'intent': attributes_from_ids(parent.intent)
        })
        i += 1

    for parent in lattice.concepts:
        for child in lattice._relation.images[parent]:
            links.append({
                'source': node_lookup[parent.__hash__()],
                'target': node_lookup[child.__hash__()]
            })

    objs = set()
    attr = set()
    for c in lattice.concepts:
        if c != lattice.bottom():
            for a in attributes_from_ids(c.intent):
                attr.add(a)

        for o in objects_from_ids(c.extent):
            objs.add(o)

    data = json.dumps({
        'nodes': nodes,
        'links': links,
        'attributes': list(attr),
        'objects': list(objs)
    })
    resp = Response(response=data,
                    status=200,
                    mimetype="application/json")
    return resp


@app.route("/add_intent", methods=['GET', 'POST'])
def add():
    data = request.json

    g = object_id(data['object'])
    intent = map(attribute_id, data['attribute'].split())

    add_intent(g, set(intent), lattice.bottom(), lattice)

    return ""


if __name__ == "__main__":
    app.run(debug=True)
