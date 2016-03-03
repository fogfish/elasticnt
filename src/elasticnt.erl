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
%% @doc
%%   N-triple intake to elastic search
-module(elasticnt).

%% elastic search i/o
-export([
   schema/3,
   declare/4,
   in/2
]).
%% data processing
-export([
   nt/2,
   dbpedia/1
]).

%% 
%%
-define(URN, {urn, <<"es">>, <<>>}).


%%
%% defines schema for attributes
%%  Options
%%    n - number of replica (default 1)
%%    q - number of shards (default 8)
schema(Sock, Schema, Opts) ->
   esio:put(Sock, uri:segments([Schema], ?URN),
      #{
         settings => #{
            number_of_shards   => opts:val(q, 8, Opts), 
            number_of_replicas => opts:val(n, 1, Opts)
         },
         mappings => #{
            '_default_' => #{
               properties => #{
                  s => #{type => string, index => not_analyzed},
                  o => #{type => string} 
               }
            }
         }
      }
   ).


%%
%% declare predicate to existed schema
%%  Type(s)
%%    string - 
%%    number -
%%    double -
declare(Sock, Schema, Attr, Type) ->
   esio:put(Sock, uri:segments([Schema, <<"_mappings">>, Attr], ?URN),
      #{
         properties => #{
            s => #{type => string, index => not_analyzed},
            o => #{type => type(Type)} 
         }
      }
   ).

type(string) ->
   string;
type(number) ->
   long;
type(double) ->
   double.


%%
%% intake stream of atomic statements 
in(Sock, Stream) ->
   stream:foreach(
      fun(#{s:= Key, p := Type, o := Val}) ->
         Uid = base64:encode( uid:encode(uid:g()) ),
         Urn = uri:segments([Type, Uid], ?URN),
         esio:put(Sock, Urn, #{s => Key,  o => jsval(Val)}, infinity)
      end,
      Stream
   ).

jsval({_, _, _} = X) ->
   scalar:s(tempus:encode(X));
jsval(X) ->
   X.


%%
%% produces stream of triples, read from n-triple file
-spec nt(atom() | list(), datum:stream()) -> datum:stream().

nt(Prefixes, Stream) ->
   stream:map(
      fun
      ({{url, S}, {url, P}, {url, O}}) ->
         #{s => uri:urn(S, Prefixes), p => uri:urn(P, Prefixes), o => uri:urn(O, Prefixes)};

      ({{url, S}, {url, P}, {url, O}, _}) ->
         #{s => uri:urn(S, Prefixes), p => uri:urn(P, Prefixes), o => uri:urn(O, Prefixes)};

      ({{url, S}, {url, P}, O}) ->
         #{s => uri:urn(S, Prefixes), p => uri:urn(P, Prefixes), o => O};

      ({{url, S}, {url, P}, O, _}) ->
         #{s => uri:urn(S, Prefixes), p => uri:urn(P, Prefixes), o => O}
      end,
      nt:stream(Stream)
   ).


%%
%% takes stream of N-triples and converts them to 
%% knowledge statements, using dbpedia ontology to
%% build identifiers.
-spec dbpedia(string() | datum:stream()) -> datum:stream().

dbpedia({s, _, _} = Stream) ->
   elasticnt_dbpedia:nt(Stream);
dbpedia(File) ->
   dbpedia(stdio:file(File)).





