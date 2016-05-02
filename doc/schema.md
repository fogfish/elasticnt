# Index schema design consideration

## Namespace prefixes

A namespace name is a uniform resource identifier, see [wiki article](https://en.wikipedia.org/wiki/XML_namespace). The library uses namespaces and they prefixes to transform NT-triples:
```
<http://dbpedia.org/resource/Albert_Einstein> <http://xmlns.com/foaf/0.1/name> "Albert Einstein"@en .

_:dbr:Albert_Einstein _:foaf:name "Albert Einstein"@en .
```   

The library defines a collection of built-in prefixes at [namespace.nt](../priv/namespace.nt)
```
_:xmlns:dbr _:rdfs:domain <http://dbpedia.org/resource/> .
```

