from addintent import *
from flask import *
import json
from flask_cors import CORS

app = Flask(__name__)
cors = CORS(app)

def init_lattice():
    bottom = Concept(set(), set('abcdefghijklmnopqrstuvwxyz'))
    l = Lattice([])
    l.add_concept(bottom)
    return l


lattice = init_lattice()
add_intent('1', set('x'), lattice.bottom(), lattice)
add_intent('2', set('yfe'), lattice.bottom(), lattice)
add_intent('3', set('af'), lattice.bottom(), lattice)
add_intent('4', set('c'), lattice.bottom(), lattice)


# This the last line in this setup causes, RuntimeError: Set changed size during iteration
# add_intent('1', set('x'), lattice.bottom(), lattice)
# add_intent('2', set('yfe'), lattice.bottom(), lattice)
# add_intent('3', set('af'), lattice.bottom(), lattice)
# add_intent('4', set('ce'), lattice.bottom(), lattice)




@app.route("/")
def show():
    nodes = []
    links = []
    i = 0
    node_lookup = {}
    for parent in lattice.concepts:
        node_lookup[parent.__hash__()] = i
        nodes.append({
            'extent': list(parent.extent),
            'intent': list(parent.intent)
        })
        i += 1

    for parent in lattice.concepts:
        for child in lattice._relation.images[parent]:
            links.append({
                'source': node_lookup[parent.__hash__()],
                'target': node_lookup[child.__hash__()]
            })

    data = json.dumps({
        'nodes': nodes,
        'links': links
    })
    resp = Response(response=data,
                    status=200,
                    mimetype="application/json")
    return (resp)



@app.route("/add", methods=['GET', 'POST'])
def add():

    return True


if __name__ == "__main__":
    app.run(debug=True)
