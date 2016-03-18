-module(elasticnt_freebase).

-export([
   nt/1
]).

%%
%% dbpedia name space schema
-define(FREEBASE, [
   {rdf,          <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>}
  ,{rdfs,         <<"http://www.w3.org/2000/01/rdf-schema#">>}
  ,{owl,          <<"http://www.w3.org/2002/07/owl#">>}
  ,{fbase,        <<"http://rdf.freebase.com/ns/">>}
  ,{fbkey,        <<"http://rdf.freebase.com/key/">>}
]).


nt(Stream) ->
   elasticnt:nt(?FREEBASE, Stream).

