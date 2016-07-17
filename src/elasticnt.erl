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

-export([start/0]).
-export([
   schema/2,
   schema/3,
   define/2,
   put/2,
   put/3,
   remove/2,
   remove/3
]).

%%
%% RnD start application
start() ->
   applib:boot(?MODULE, []).

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
%% define predicate meta-data
-spec define(binary(), _) -> [semantic:spo()].

define(Predicate, Type) ->
   lists:map(fun(X) -> X#{type => schema} end, semantic:define(Predicate, Type)).


%%
%% put the statement into cluster
-spec put(pid(), semantic:spo() | [semantic:spo()] | datum:stream()) -> ok.
-spec put(pid(), semantic:spo(), timeout()) -> ok.

put(Sock, {s, _, _} = Stream) ->
   stream:foreach(
      fun(Fact) ->
         elasticnt:put(Sock, Fact)
      end,
      Stream
   );

put(Sock, [_ | _] = List) ->
   lists:foreach(
      fun(Fact) ->
         elasticnt:put(Sock, Fact)
      end,
      List
   );

put(Sock, Fact) ->
   elasticnt:put(Sock, Fact, 5000).

put(Sock, Fact, Timeout) ->
   {Urn, Stmt} = elasticnt_schema:encode(Fact),
   esio:put(Sock, Urn, Stmt, Timeout).

%%
%% remove the statement from cluster
-spec remove(pid(), semantic:spo() | [semantic:spo()] | datum:stream()) -> ok.
-spec remove(pid(), semantic:spo(), timeout()) -> ok.

remove(Sock, {s, _, _} = Stream) ->
   stream:foreach(
      fun(Fact) ->
         elasticnt:remove(Sock, Fact)
      end,
      Stream
   );

remove(Sock, [_ | _] = List) ->
   lists:foreach(
      fun(Fact) ->
         elasticnt:remove(Sock, Fact)
      end,
      List
   );

remove(Sock, Fact) ->
   elasticnt:remove(Sock, Fact, 5000).

remove(Sock, Fact, Timeout) ->
   {Urn, _Stmt} = elasticnt_schema:encode(Fact),
   esio:remove(Sock, Urn, Timeout).


