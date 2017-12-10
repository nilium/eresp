%% Copyright 2017, Noel Cower <ncower@gmail.com>.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(eresp).

%% API exports
-export([encode/1, encode/2]).
-export([cmd/1, cmd/2]).
-export([resp/1]).

-type command() :: atom() | binary() | nonempty_string().
-type resp_scalar() :: number() | iolist() | string() | binary() | atom() | boolean() | 'nil'.
-type resp_term() :: resp_scalar() | list(resp_term()).
-type resp_terms() :: list(resp_term()).
-type options() :: #{
	mode => client | server | none()
       }.
-type error() :: {error, Reason :: term()}.

%%====================================================================
%% API functions
%%====================================================================

%% @equiv encode(Term, #{})
encode(Term) ->
   encode(Term, #{}).

-spec encode(Term :: resp_term(), Options :: options()) -> iolist() | error().
encode(Term, #{mode := client} = Options) ->
   try encode_client(Term, Options)
   catch error:Reason -> {error, Reason} end;
encode(Term, Options) ->
   try encode_server(Term, Options)
   catch error:Reason -> {error, Reason} end.

-spec resp(Term :: resp_term()) -> iolist() | error().
resp(Term) ->
   encode(Term, #{}).

%% @equiv cmd(Command, [])
-spec cmd(Command :: command()) -> iolist() | error().
cmd(Command) ->
   cmd(Command, []).

-spec cmd(Command :: command(), Args :: resp_terms()) -> iolist() | error().
cmd(Command, Args) when is_atom(Command), is_list(Args) ->
   cmd(atom_to_binary(Command, utf8), Args);
cmd(Command, Args) when is_binary(Command), is_list(Args);
			is_list(Command), is_list(Args) ->
   encode_list([upper(Command)|Args], #{mode => client}).


%%====================================================================
%% Internal functions
%%====================================================================

encode_client(List, _Options) when is_list(List) ->
   encode_bulk_string(List);
encode_client(Integer, _Options) when is_integer(Integer) ->
   encode_bulk_string(integer_to_binary(Integer, 10));
encode_client(Term, Options) ->
   encode_term(Term, Options).

encode_server(nil, _Options) ->
   encode_nil();
encode_server(true, _Options) ->
   encode_integer(1);
encode_server(false, _Options) ->
   encode_integer(0);
encode_server(ok, _Options) ->
   encode_simple_string(<<"OK">>);
encode_server(Term, Options) ->
   encode_term(Term, Options).

-define(CRLF, "\r\n").
-define(EMPTY_BULK_STRING, <<$$, "0", ?CRLF, ?CRLF>>).

encode_nil() ->
   [<<"$-1" ?CRLF>>].

encode_integer(Integer) when is_integer(Integer) ->
   [$:, integer_to_binary(Integer, 10), <<?CRLF>>].

encode_float(Float) when is_float(Float) ->
   encode_bulk_string(float_to_binary(Float, [{decimals, 14}, compact])).

encode_bulk_string(<<>>) ->
   [?EMPTY_BULK_STRING];
encode_bulk_string([]) ->
   [?EMPTY_BULK_STRING];
encode_bulk_string(String) when is_binary(String) ->
   [$$, integer_to_binary(byte_size(String), 10), <<?CRLF>>, String, <<?CRLF>>];
encode_bulk_string(IOList) when is_list(IOList) ->
   try [$$, integer_to_binary(iolist_size(IOList), 10), <<?CRLF>>, IOList, <<?CRLF>>]
   catch error:badarg -> error(bad_iolist) end.

-spec encode_list(resp_terms(), options()) -> iolist().
encode_list([], _Options) ->
   [<<$*, "0", ?CRLF>>];
encode_list(List, Options) when is_list(List) ->
   [$*, integer_to_binary(length(List)), <<?CRLF>>,
    [encode(Term, Options) || Term <- List], <<?CRLF>>].

-spec encode_simple_string(binary() | iolist()) -> iolist().
encode_simple_string(<<"OK">>) ->
   [<<$+, "OK" ?CRLF>>];
encode_simple_string(Str) when is_binary(Str); is_list(Str) ->
   [$+, Str, <<?CRLF>>].

-spec encode_atom(atom()) -> iolist().
encode_atom(Atom) when is_atom(Atom) ->
   encode_bulk_string(erlang:atom_to_binary(Atom, utf8)).

-spec encode_term(resp_term(), options()) -> iolist().
encode_term(Atom, _Options) when is_atom(Atom) ->
   encode_atom(Atom);
encode_term(Binary, _Options) when is_binary(Binary) ->
   encode_bulk_string(Binary);
encode_term(Integer, _Options) when is_integer(Integer) ->
   encode_integer(Integer);
encode_term(Float, _Options) when is_float(Float) ->
   encode_float(Float);
encode_term(List, Options) when is_list(List) ->
   encode_list(List, Options);
encode_term(_Term, _Options) ->
   error(badarg).

-spec upper(binary()) -> binary();
	   (string()) -> string().
upper(Bin) when is_binary(Bin) ->
   << <<(if C >= $a, C =< $z -> C - $a + $A; true -> C end)>>
      || <<C>> <= Bin >>;
upper(List) when is_list(List) ->
   [ (if C >= $a, C =< $z -> C - $a + $A; true -> C end)
     || C <- List ].
