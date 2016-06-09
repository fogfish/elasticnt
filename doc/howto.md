# HowTo `elasticnt`


## Intake dbpedia snapshots

[DBpedia](http://wiki.dbpedia.org) provides large multi-domain, localized ontologies. Its derived from from Wikipedia. This chapter provides step-by-step instructions how to store knowledge snapshots into Elastic Search. 

### download snapshot

You should download either `ttl` or `nt` formatted snapshots (e.g. category labels)   
```
curl -O http://downloads.dbpedia.org/2015-10/core-i18n/en/category_labels_en.ttl.bz2
```

### prepare snapshot

The `bz2` compression format is not supported by `elasticnt` library. It is required to re-compress into `gz`.
```
bunzip2 -c category_labels_en.ttl.bz2 | gzip > category_labels_en.ttl.gz
``` 

### start ElasticSearch
```
docker run -it -p 9200:9200 fogfish/elasticsearch
```

### create index

Build library and run the development console
```
make
make run
```

Let's create an index and store the N-triple
```erlang
%%
%% start application
elasticnt:start().

%%
%% deploy index schema
{ok, Schema} = esio:socket("http://192.168.99.100:9200/dbpedia").
elasticnt:schema(Schema, "", []).


%%
%% create ElasticSearch socket dedicated for bulk intake
{ok, Sock} = esio:socket("http://192.168.99.100:9200/dbpedia", [bulk, {n, 1000}]).

%%
%% create data stream
Stream = semantic:nt("category_labels_en.ttl.gz").

%%
%% intake data stream to cluster 
elasticnt:in(Sock, Stream).
```




   
