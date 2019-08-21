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
%% data types
-type fact() :: semantic:spo() | [semantic:spo()] | datum:stream().
-type opts() :: [{_, _} | _].

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
   elasticnt:schema(Sock, undefined, Opts).

schema(Sock, Schema, Opts) ->
   % esio:put(Sock, uri:segments([Schema], ?URN), elasticnt_schema:new(Opts)).
   esio:put(Sock, <<"/nt">>, elasticnt_schema:new(Opts)).

%%
%% define predicate meta-data
-spec define(binary(), _) -> [semantic:spo()].

define(Predicate, Type) ->
   lists:map(fun(X) -> X#{type => schema} end, semantic:define(Predicate, Type)).


%%
%% put the statement into cluster
%%  Options
%%   * t - timeout to accomplish operation
%%   * unique - identity of unique flag sp | spo
-spec put(pid(), fact()) -> ok.
-spec put(pid(), fact(), opts()) -> ok.

put(Sock, Fact) ->
   elasticnt:put(Sock, Fact, [{t, 5000}]).

put(Sock, #{s := _, p := _, o := _} = Fact, Opts) ->
   put_into(Sock, Fact, Opts);   

put(Sock, {s, _, _} = Stream, Opts) ->
   stream:foreach(
      fun(Fact) ->
         put_into(Sock, Fact, Opts)
      end,
      Stream
   );

put(Sock, [_ | _] = List, Opts) ->
   lists:foreach(
      fun(Fact) ->
         put_into(Sock, Fact, Opts)
      end,
      List
   ).

put_into(Sock, Fact, Opts) ->
   {Urn, Stmt} = elasticnt_schema:encode(Fact, opts:val(unique, ?UNIQUE, Opts)),
   esio:put(Sock, Urn, Stmt, opts:val(t, ?TIMEOUT, Opts)).

%%
%% remove the statement from cluster
-spec remove(pid(), fact()) -> ok.
-spec remove(pid(), fact(), opts()) -> ok.

remove(Sock, Fact) ->
   elasticnt:remove(Sock, Fact, [{t, 5000}]).

remove(Sock, {s, _, _} = Stream, Opts) ->
   stream:foreach(
      fun(Fact) ->
         remove_from(Sock, Fact, Opts)
      end,
      Stream
   );

remove(Sock, [_ | _] = List, Opts) ->
   lists:foreach(
      fun(Fact) ->
         remove_from(Sock, Fact, Opts)
      end,
      List
   ).

remove_from(Sock, Fact, Opts) ->
   {Urn, _Stmt} = elasticnt_schema:encode(Fact, opts:val(unique, ?UNIQUE, Opts)),
   esio:remove(Sock, Urn, opts:val(t, ?TIMEOUT, Opts)).


