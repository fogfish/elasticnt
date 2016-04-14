%%
%%   Copyright 2016 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%

%% 
%% default urn schema for statement identity (used by elastic I/O)
-define(URN, {urn, <<"es">>, <<>>}).


%%
%% built-in ontologies and they knowledge name-spaces
-define(KNS, [
   %%
   %% dbpedia
   {rdf,          <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>}
  ,{rdfs,         <<"http://www.w3.org/2000/01/rdf-schema#">>}
  ,{dc,           <<"http://purl.org/dc/elements/1.1/">>}
  ,{foaf,         <<"http://xmlns.com/foaf/0.1/">>}
  ,{things,       <<"http://dbpedia.org/resource/">>}
  ,{dbpedia,      <<"http://dbpedia.org/ontology/">>}
  ,{wikimedia,    <<"http://commons.wikimedia.org/wiki/">>}

   %%
   %% free-base
  ,{owl,          <<"http://www.w3.org/2002/07/owl#">>}
  ,{fbase,        <<"http://rdf.freebase.com/ns/">>}
  ,{fbkey,        <<"http://rdf.freebase.com/key/">>}

   %%
   %% geo rss
  ,{georss,       <<"http://www.georss.org/georss/">>}
]).
