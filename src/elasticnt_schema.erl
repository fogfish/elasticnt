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
  ,encode/2
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
               c        => #{type => float,  index => not_analyzed},
               k        => #{type => string, index => not_analyzed},
               binary   => #{type => string},
               integer  => #{type => long},
               float    => #{type => double},
               boolean  => #{type => boolean},
               datetime => #{type => date, format => basic_date_time_no_millis, index => not_analyzed},
               geohash  => #{type => geo_point, index => not_analyzed, geohash_prefix => true},
               rel      => #{type => string, index => not_analyzed}
            }
         }
      }
   }.

%%
%% encode fact to ElasticSearch JSON format
encode(#{s := S, p := P, o := O} = Fact, Unique) ->
   encode(urn(S), urn(P), val(O), Fact, Unique).

encode(S, P, O, #{c := C, k := K} = Fact, Unique) ->
   Key  = key(S, P, O, Unique),
   Uid  = bits:btoh(uid:encode(K)),
   Type = semantic:typeof(Fact),
   Urn  = uri:segments([Type, Key], ?URN),
   {Urn, #{s => S, p => P, Type => O, c => C, k => Uid}}.


%%
%% unique fact identity (content address)
key(S, P, O, spo) ->
   base64( crypto:hash(md5, [S, P, scalar:s(O)]) );

key(S, P, _, sp) ->
   base64( crypto:hash(md5, [S, P]) ).


%%
%% jsonify iri into urn
urn({iri, Prefix, Suffix}) ->
   <<"_:", Prefix/binary, $:, Suffix/binary>>;
urn({iri, Urn}) ->
   Urn.

%%
%% jsonify fact value
val({iri, _, _} = IRI) ->
   urn(IRI);
val({iri, _} = IRI) ->
   urn(IRI);
val({_, _, _} = X) ->
   scalar:s(tempus:encode(X));
val(X) ->
   X.

%%
%%
base64(Hash) ->
   << << (urlencode(D)) >> || <<D>> <= base64:encode(Hash), D =/= $= >>.

urlencode($/) -> $_;
urlencode($+) -> $-;
urlencode(D)  -> D.
