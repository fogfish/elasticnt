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
-include("elasticnt.hrl").

%% elastic search i/o
-export([
   schema/2,
   schema/3,
   put/2,
   put/3,
   in/2,
   nt/1,
   nt/2
]).

%%
%% create new knowledge statement schema
%%  Options
%%    n - number of replica (default 1)
%%    q - number of shards (default 8)
-spec schema(pid(), atom(), [{_, _}]) -> ok.

schema(Sock, Opts) ->
   elasticnt:schema(Sock, "", Opts).

schema(Sock, Schema, Opts) ->
   esio:put(Sock, uri:segments([Schema], ?URN), elasticnt_schema:new(Opts)).


%%
%% put the statement into cluster
-spec put(pid(), #{s => _, p => _, o => _}) -> ok.
-spec put(pid(), #{s => _, p => _, o => _}, timeout()) -> ok.

put(Sock, Fact) ->
   elasticnt:put(Sock, Fact, 5000).

put(Sock, Fact, Timeout) ->
   {Urn, Stmt} = elasticnt_schema:encode(Fact),
   esio:put(Sock, Urn, Stmt, Timeout).

%%
%% intake stream of atomic statements 
-spec in(pid(), datum:stream()) -> ok.

in(Sock, Stream) ->
   stream:foreach(
      fun(Fact) ->
         elasticnt:put(Sock, Fact, infinity)
      end,
      Stream
   ).

%%
%% takes stream of N-triples and converts them to 
%% knowledge statements, using built-in ontologies.
-spec nt(datum:stream()) -> datum:stream().
-spec nt(list(), datum:stream()) -> datum:stream().

nt(Stream) ->
   nt(?KNS, Stream).

nt(Prefixes, {s, _, _} = Stream) ->
   stream:map(
      fun(X) -> fact(Prefixes, X) end,
      nt:stream(Stream)
   );
nt(Prefixes, File) ->
   case filename:extension(File) of
      ".nt" -> nt(Prefixes, stdio:file(File));
      ".gz" -> nt(Prefixes, gz:stream(stdio:file(File)))
   end.


%%
%%
fact(Prefixes, {{url, S}, {url, P}, {url, O}}) ->
   #{
      s => uri:urn(S, Prefixes), 
      p => uri:urn(P, Prefixes), 
      o => uri:urn(O, Prefixes)
   };
fact(Prefixes, {{url, S}, {url, P}, {url, O}, Lang})
 when is_binary(Lang) ->
   #{
      s => uri:urn(S, Prefixes), 
      p => uri:urn(P, Prefixes), 
      o => uri:urn(O, Prefixes), 
      lang => Lang
   };
fact(Prefixes, {{url, S}, {url, P}, O}) ->
   #{
      s => uri:urn(S, Prefixes), 
      p => uri:urn(P, Prefixes), 
      o => O
   };
fact(Prefixes, {{url, S}, {url, P}, O, Lang})
 when is_binary(Lang) ->
   #{
      s => uri:urn(S, Prefixes), 
      p => uri:urn(P, Prefixes), 
      o => O,
      lang => Lang
   }.
