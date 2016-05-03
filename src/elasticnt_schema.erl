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
  ,namespace/1
  ,ontology/2
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
         prefix      => #{properties => properties(string)},
         schema      => #{properties => properties(string)},
         string      => #{properties => properties(string)},
         long        => #{properties => properties(long)},
         double      => #{properties => properties(double)},
         datetime    => #{properties => properties(datetime)},
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
properties(datetime) ->
   #{
      s => #{type => string, index => not_analyzed},
      p => #{type => string, index => not_analyzed},
      o => #{type => date, format => basic_date_time_no_millis, index => not_analyzed},
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
%% provides built-in namespace
namespace(_) ->
   stream:list(
      stream:map(fun nt2json/1,
         nt:stream(
            stdio:file( filename:join([code:priv_dir(elasticnt), "namespace.nt"]) )
         )
      )
   ).   

nt2json({{uri, S}, {uri, P}, {uri, O}}) ->
   jsonify(#{s => S, p => P, o => O, type => prefix}).   

%%
%%
ontology(P, Type) ->
   jsonify(#{s => elasticnt_ns_encode(P), p => <<"urn:rdf:type">>, o => typeof(Type), type => schema}).


%%
%% encode fact to JSON format
encode(Fact) ->
   jsonify( typed(Fact) ).

%%
%% map nt-triple to typed structure
typed(#{s := S, p := P, lang := _} = Fact) ->
   Fact#{
      s => elasticnt_ns_encode(S),
      p => elasticnt_ns_encode(P)
   };

typed(#{s := S, p := P, o := O, type := <<"http://www.w3.org/2001/XMLSchema#date">>} = Fact) ->
   Fact#{
      s => elasticnt_ns_encode(S),
      p => elasticnt_ns_encode(P),
      o => tempus:iso8601(O),
      type => datetime
   };

typed(#{s := S, p := P, o := O, type := <<"http://www.w3.org/2001/XMLSchema#dateTime">>} = Fact) ->
   Fact#{
      s => elasticnt_ns_encode(S),
      p => elasticnt_ns_encode(P),
      o => tempus:iso8601(O),
      type => datetime
   };

typed(#{s := S, p := P, o := O, type := <<"http://www.w3.org/2001/XMLSchema#gYearMonth">>} = Fact) ->
   Fact#{
      s => elasticnt_ns_encode(S),
      p => elasticnt_ns_encode(P),
      o => tempus:iso8601(O),
      type => datetime
   };

typed(#{s := S, p := P, o := O, type := <<"http://www.w3.org/2001/XMLSchema#gYear">>} = Fact) ->
   Fact#{
      s => elasticnt_ns_encode(S),
      p => elasticnt_ns_encode(P),
      o => tempus:iso8601(O),
      type => datetime
   };

typed(#{s := S, p := P, o := O, type := <<"http://www.w3.org/2001/XMLSchema#gMonth">>} = Fact) ->
   Fact#{
      s => elasticnt_ns_encode(S),
      p => elasticnt_ns_encode(P),
      o => scalar:i(O),
      type => long
   };

typed(#{s := S, p := P, o := O, type := <<"http://www.w3.org/2001/XMLSchema#gDay">>} = Fact) ->
   Fact#{
      s => elasticnt_ns_encode(S),
      p => elasticnt_ns_encode(P),
      o => scalar:i(O),
      type => long
   };

typed(#{s := S, p := P, o := O, type := <<"http://www.w3.org/2001/XMLSchema#integer">>} = Fact) ->
   Fact#{
      s => elasticnt_ns_encode(S),
      p => elasticnt_ns_encode(P),
      o => scalar:i(O),
      type => long
   };

typed(#{s := S, p := P, o := O, type := <<"http://www.w3.org/2001/XMLSchema#string">>} = Fact) ->
   Fact#{
      s => elasticnt_ns_encode(S),
      p => elasticnt_ns_encode(P),
      o => scalar:i(O),
      type => string
   };

typed(#{s := S, p := P, o := O} = Fact) ->
   Fact#{
      s => elasticnt_ns_encode(S),
      p => elasticnt_ns_encode(P),
      o => elasticnt_ns_encode(O)
   }.

elasticnt_ns_encode(X) ->
   elasticnt_ns_encode:q(undefined, X).

%%
%% map typed fact to json object
jsonify(#{s := S, p := P, o := O} = Fact) ->
   Uid  = base64:encode( uid:encode(uid:g()) ),
   JsO  = value(O),
   Key  = unique_id(S, P, JsO),
   Type = typeof(Fact),
   Urn  = uri:segments([Type, Key], ?URN),
   {Urn, #{s => S, p => P, o => JsO, k => Uid}}.

%%
%% jsonify fact value
value({_, _, _} = X) ->
   scalar:s(tempus:encode(X));
value(X) ->
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

typeof(#{type := Type}) ->
   Type;

typeof(#{o := O})
 when is_binary(O) ->
   string;

typeof(<<"http://www.w3.org/2001/XMLSchema#date">>) ->
   datetime;

typeof(<<"http://www.w3.org/2001/XMLSchema#dateTime">>) ->
   datetime;

typeof(<<"http://www.w3.org/2001/XMLSchema#gYearMonth">>) ->
   datetime;

typeof(<<"http://www.w3.org/2001/XMLSchema#gYear">>) ->
   datetime;

typeof(<<"http://www.w3.org/2001/XMLSchema#gMonth">>) ->
   long;

typeof(<<"http://www.w3.org/2001/XMLSchema#gDay">>) ->
   long;

typeof(<<"http://www.w3.org/2001/XMLSchema#integer">>) ->
   long;

typeof(<<"http://www.w3.org/2001/XMLSchema#string">>) ->
   string;

typeof(<<"string">>) ->
   string;

typeof(<<"long">>) ->
   long;

typeof(<<"double">>) ->
   double;

typeof(<<"datetime">>) ->
   datetime;

typeof(<<"geohash">>) ->
   geohash.



