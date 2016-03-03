-module(elasticnt_dbpedia).

-export([
   nt/1
]).

%%
%% dbpedia name space schema
-define(DBPEDIA, [
   {rdf,          <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>}
  ,{rdfs,         <<"http://www.w3.org/2000/01/rdf-schema#">>}
  ,{dc,           <<"http://purl.org/dc/elements/1.1/">>}
  ,{foaf,         <<"http://xmlns.com/foaf/0.1/">>}
  ,{things,       <<"http://dbpedia.org/resource/">>}
  ,{dbpedia,      <<"http://dbpedia.org/ontology/">>}
  ,{wikimedia,    <<"http://commons.wikimedia.org/wiki/">>}
]).


nt(Stream) ->
   elasticnt:nt(?DBPEDIA, Stream).

