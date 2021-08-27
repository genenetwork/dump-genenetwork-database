import os.path
from pathlib import Path
from rdflib import Graph

graph = Graph()
graph.parse(location=os.path.join(Path.home(), 'data/dump/dump.ttl'),
            format='text/turtle')

query = """
PREFIX gn: <https://genenetwork.org/>

SELECT ?name ?binomial
WHERE {
  ?species rdf:type gn:species .
  ?strain rdf:type gn:strain .
  ?strain gn:name "JN9" .
  ?strain gn:strainOfSpecies ?species .

  ?species gn:name ?name .
  ?species gn:binomialName ?binomial .
}
"""

for result in graph.query(query):
    print(result)
