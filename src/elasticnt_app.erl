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
-module(elasticnt_app).
-behaviour(application).

-include("elasticnt.hrl").

-export([start/2, stop/1]).
-export([ns_encode/3, ns_decode/3]).


start(_Type, _Args) -> 
   ns_build(),
   {ok, self()}.

stop(_State) ->
   ok.


%%
%% compile ns
ns_build() ->
   Kns = ns_load(), 
   ns_build_encode(Kns),
   ns_build_decode(Kns).

%%
%%
ns_load() ->
   stream:list(
      stream:map(fun nt2kns/1,
         nt:stream(
            stdio:file( filename:join([code:priv_dir(elasticnt), "namespace.nt"]) )
         )
      )
   ) ++ [{undefined, <<>>}].

nt2kns({{urn, <<"urn:xmlns:", Ns/binary>>}, {urn,<<"urn:rdfs:domain">>}, {uri, Uri}}) ->
   {Ns, Uri}.

%%
%% compile prefix encoder
ns_build_encode(Kns) ->
   hornlog:c(elasticnt_ns_encode, [ns_encode(X) || X <- Kns]).

ns_encode({Ns, Uri}) ->
   hornlog:rule(hornlog:like(fun elasticnt_app:ns_encode/2, [Ns]), scalar:s(Uri)).

ns_encode(undefined, _X, Suffix) ->
   Suffix;
ns_encode(Ns, _X, Suffix) ->
   <<"urn:", Ns/binary, $:, Suffix/binary>>.


%%
%% compile prefix decoder
ns_build_decode(Kns) ->
   hornlog:c(elasticnt_ns_decode, [ns_decode(X) || X <- Kns]).

ns_decode({undefined, <<>>}) ->
   hornlog:rule(hornlog:like(fun elasticnt_app:ns_decode/2, [undefined]), <<>>);

ns_decode({Ns, Uri}) ->
   hornlog:rule(hornlog:like(fun elasticnt_app:ns_decode/2, [scalar:s(Uri)]), <<"urn:", Ns/binary, $:>>).

ns_decode(undefined, _X, Suffix) ->
   Suffix;
ns_decode(Uri, _X, Suffix) ->
   <<Uri/binary, Suffix/binary>>.


