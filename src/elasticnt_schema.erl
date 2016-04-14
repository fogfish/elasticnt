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
%%   defines serialization schema for knowledge statements into elastic search
-module(elasticnt_schema).
-include("elasticnt.hrl").

-export([
   new/1
  ,encode/1
]).


%%
%% define new schema
%%    n - number of replica (default 1)
%%    q - number of shards (default 8)
new(Opts) ->
   #{
      settings => #{
         number_of_shards   => opts:val(q, 8, Opts), 
         number_of_replicas => opts:val(n, 1, Opts)
      },
      mappings => #{
         '_default_' => #{properties => properties(string)},
         string      => #{properties => properties(string)},
         long        => #{properties => properties(long)},
         double      => #{properties => properties(double)},
         datetime    => #{properties => properties(string)},
         geohash     => #{properties => properties(geohash)}
      }
   }.


properties(geohash) ->
   #{
      s => #{type => string, index => not_analyzed},
      p => #{type => string, index => not_analyzed},
      o => #{type => geo_point, index => not_analyzed, geohash_prefix => true},
      k => #{type => string, index => not_analyzed}
   };
properties(Type) ->
   #{
      s => #{type => string, index => not_analyzed},
      p => #{type => string, index => not_analyzed},
      o => #{type => Type},
      k => #{type => string, index => not_analyzed}
   }.


%%
%% encode fact to JSON format
encode(#{s := S, p := P, o := O} = Fact) ->
   Uid  = base64:encode( uid:encode(uid:g()) ),
   JsO  = jsonify(O),
   Key  = unique_id(S, P, JsO),
   Type = typeof(Fact),
   Urn  = uri:segments([Type, Key], ?URN),
   {Urn, #{s => S, p => P, o => JsO, k => Uid}}.

%%
%% jsonify fact value
jsonify({geo, Lat, Lng}) ->
   hash:geo(Lat, Lng);

jsonify({_, _, _} = X) ->
   scalar:s(tempus:encode(X));

jsonify(X) ->
   X.

%%
%% unique fact identity (content address)
unique_id(S, P, O) ->
   bits:btoh(
      crypto:hash(md5, [scalar:s(S), scalar:s(P), scalar:s(O)])
   ).

%%
%% fact type
typeof(#{lang := Lang}) ->
   Lang;

typeof(#{o := O})
 when is_binary(O) ->
   string;

typeof(#{o := O})
 when is_integer(O) ->
   long;

typeof(#{o := O})
 when is_float(O) ->
   double;

typeof(#{o := {geo, _, _}}) ->
   geohash;

typeof(#{o := {_, _, _}}) ->
   datetime.

