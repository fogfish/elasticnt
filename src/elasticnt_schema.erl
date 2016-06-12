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
         %% expected mappings: schema, rel, long, double, boolean, datetime, geohash, string
         '_default_' => #{
            properties => #{
               s        => #{type => string, index => not_analyzed},
               p        => #{type => string, index => not_analyzed},
               k        => #{type => string, index => not_analyzed},
               string   => #{type => string},
               long     => #{type => long},
               double   => #{type => double},
               boolean  => #{type => boolean},
               datetime => #{type => date, format => basic_date_time_no_millis, index => not_analyzed},
               geohash  => #{type => geo_point, index => not_analyzed, geohash_prefix => true}
            }
         }
      }
   }.


%%
%% encode fact to ElasticSearch JSON format
encode(#{s := {uri, S}, p := {uri, P}, o := O} = Fact) ->
   Uid  = base64:encode( uid:encode(uid:g()) ),
   JsO  = val(O),
   Key  = key(S, P, JsO),
   Type = semantic:typeof(Fact),
   Urn  = uri:segments([Type, Key], ?URN),
   {Urn, #{s => S, p => P, Type => JsO, k => Uid}}.

%%
%% jsonify fact value
val({_, _, _} = X) ->
   scalar:s(tempus:encode(X));
val({uri, X}) ->
   X;
val(X) ->
   X.

%%
%% unique fact identity (content address)
key(S, P, O) ->
   bits:btoh(
      crypto:hash(md5, [scalar:s(S), scalar:s(P), scalar:s(O)])
   ).
