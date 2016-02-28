-module(elasticnt).

-export([
   nt/2,
   dbpedia/1,
   in/2
]).


dbpedia({s, _, _} = Stream) ->
   elasticnt_dbpedia:nt(Stream);
dbpedia(File) ->
   dbpedia(stdio:file(File)).

% xxx:nt(?DBPEDIA, gz:stream(stdio:file(File))).

%%
%% produces stream of triples, read from n-triple file
-spec(nt/2 :: (atom() | list(), datum:stream()) -> datum:stream()).

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
%%
in(Sock, Stream) ->
   stream:foreach(
      fun(#{s := S, p := P, o := O}) ->
         Urn = uri:segments([P, S], uri:new({urn, <<"es">>, <<>>})),
         esio:put(Sock, Urn, #{o => O}, infinity)
      end,
      Stream
   ).
   