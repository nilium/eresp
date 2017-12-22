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

%% @doc eresp is a package for encoding and decoding the Redis serialization
%% protocol (RESP).
-module(eresp).

%% API exports
-export([encode/1, encode/2]).
-export([cmd/1, cmd/2]).
-export([decode/1]).

-type command() :: atom() | binary() | nonempty_string().
-type resp_error() :: {error, string() | binary()}.
-type resp_scalar() :: number() | iolist() | string() | binary() | atom() | boolean() | 'nil'.
-type resp_term() :: resp_scalar() | resp_error() | resp_terms().
-type resp_terms() :: list(resp_term()).
-type options() :: #{
        mode => client | server | none()
       }.

-type error() :: {error, Reason :: term()}.
-type bad_resp_error() :: {error,
                           {bad_resp,
                            list(integer
                                 | simple_string
                                 | bulk_string
                                 | array
                                 | error
                                 | unknown)}}.
-type decoded_term() :: {ok, Term :: resp_term(), Rest :: binary()}.

%%====================================================================
%% API functions
%%====================================================================

%% Encoding

%% @equiv encode(Term, #{})
encode(Term) ->
  encode(Term, #{}).

%% @doc Encodes an Erlang term using the given options. If the Term cannot be
%% encoded, it fails with a `badarg' error.
%%
%% Options may include a `mode' key, which if set to `client' will encode all
%% terms passed as bulk strings. To produce a flat list suitable for client
%% encoding of a command, you can use the `cmd/2' function.
%%
%% By default, if no options are given (i.e., Options is an empty map), server
%% encoding is used. For common cases using server encoding, you should prefer
%% using `encode/1'.
%%
%% === Client Encoding ===
%%
%% The following types can be encoded as bulk strings in client encoding:
%%
%% <ul>
%% <li>IOLists</li>
%% <li>Binaries</li>
%% <li>Integers</li>
%% <li>Floats</li>
%% <li>Atoms</li>
%% </ul>
%%
%% === Server Encoding ===
%%
%% The following table details how different Erlang terms are encoded in RESP:
%%
%% <table border="1">
%%   <tr>
%%     <th width="20%">Erlang</th>
%%     <th>RESP</th>
%%   </tr>
%%   <tr>
%%     <td>12345 (Integer)</td>
%%     <td>Integers.</td>
%%   </tr>
%%   <tr>
%%     <td>123.45 (Float)</td>
%%     <td>
%%       A bulk string, encoded in compact form with up to 14 decimals. If you
%%       need more or fewer digits, convert your float to a string ahead of time
%%       to ensure it's encoded as desired.
%%     </td>
%%   </tr>
%%   <tr>
%%     <td>Binaries</td>
%%     <td>Bulk strings.</td>
%%   </tr>
%%   <tr>
%%     <td>Lists</td>
%%     <td>Encoded as arrays.</td>
%%   </tr>
%%   <tr>
%%     <td>`{bulk, IOList}'</td>
%%     <td>
%%       Bulk strings. This allows using an IOList for a bulk string when
%%       performing server encoding, as lists are encoded as arrays.
%%     </td>
%%   </tr>
%%   <tr>
%%     <td>
%%       `{error, Message}'<br/>
%%       `{error, {Class, Reason}}'
%%     </td>
%%     <td>
%%       Errors. If the second form is used, `Class' and `Reason' are encoded
%%       into a single error message of the form `CLASS Reason', and `Class' is
%%       uppercased, as in common Redis error messages.
%%
%%       `Class', `Reason', and `Message' may be an atom, string, or binary
%%       string.
%%     </td>
%%   </tr>
%%   <tr>
%%     <td>`` 'ok' ''</td>
%%     <td>The simple string `OK'.</td>
%%   </tr>
%%   <tr>
%%     <td>`` 'nil' ''</td>
%%     <td>
%%       A null bulk string. Although semantically equivalent to a null bulk
%%       array, the null array is never used in encoding in eresp.
%%     </td>
%%   </tr>
%%   <tr>
%%     <td>`` 'true' '', `` 'false' ''</td>
%%     <td>RESP integers 1 and 0, respectively.</td>
%%   </tr>
%% </table>
%%
%% @see cmd/2
-spec encode(Term :: resp_term(), Options :: options()) -> iolist().
encode(Term, #{mode := client} = Options) ->
  encode_client(Term, Options);
encode(Term, Options) ->
  encode_server(Term, Options).

%% @equiv cmd(Command, [])
-spec cmd(Command :: command()) -> iolist() | error().
cmd(Command) ->
  cmd(Command, []).

%% @doc Encodes a client command as a flat array of bulk strings, containing
%% Command as its command and Args as the arguments to that command.
%%
%% This is intended for use as the client encoding when sending commands to
%% a Redis or RESP-compatible server.
-spec cmd(Command :: command(), Args :: resp_terms()) -> iolist() | error().
cmd(Command, Args) when is_atom(Command), is_list(Args) ->
  cmd(atom_to_binary(Command, utf8), Args);
cmd(Command, Args) when is_binary(Command), is_list(Args);
                        is_list(Command), is_list(Args) ->
  encode_array([upper(Command)|Args], #{mode => client}).

%% Decoding

%% @doc Decodes a binary string as RESP, returning Erlang terms representing
%% the elements of the RESP provided.
%%
%% If decoding is successful, returns the tuple `{ok, Term, Rest}', where Term
%% is an Erlang term representing the decoded RESP, and Rest is the tail of Bin
%% that was not parsed.
%%
%% If there is an error decoding Bin (because the RESP is invalid), the result
%% is {error, {bad_resp, Stack}}, where Stack is a list of atoms showing the
%% parsing stack. Often, the last element is the most relevant -- the last is
%% the element decode thought it was parsing and failed to handle. In cases
%% where the binary makes no sense at all, the atom is `` 'unknown' ''.
%%
%% === Decoding Table ===
%%
%% <table border="1">
%%   <tr>
%%     <th>RESP Type</th>
%%     <th>Erlang Type</th>
%%   </tr>
%%   <tr>
%%     <td>Simple String "OK"</td>
%%     <td>`` 'ok' ''</td>
%%   </tr>
%%   <tr>
%%     <td>Simple String</td>
%%     <td>Binaries: `<<"Simple String">>'</td>
%%   </tr>
%%   <tr>
%%     <td>Bulk String</td>
%%     <td>Binaries: `<<"Bulk String">>'</td>
%%   </tr>
%%   <tr>
%%     <td>Array</td>
%%     <td>Lists</td>
%%   </tr>
%%   <tr>
%%     <td>Integer</td>
%%     <td>Integer</td>
%%   </tr>
%%   <tr>
%%     <td>Bulk Nil (Array / String)</td>
%%     <td>`` 'nil' ''</td>
%%   </tr>
%%   <tr>
%%     <td>Errors</td>
%%     <td>`{error, <<"ERROR description">>}'</td>
%%   </tr>
%% </table>
-spec decode(binary()) -> decoded_term() | bad_resp_error().
decode(Bin) when is_binary(Bin) ->
  try decode_binary(Bin) of
    {Term, Rest} ->
      {ok, Term, Rest}
  catch
    error:Reason ->
      {error, Reason}
  end.


%%====================================================================
%% Internal functions
%%====================================================================

%% Encoding

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
  encode_ok();
encode_server({bulk, String}, _Options) when is_binary(String); is_list(String) ->
  encode_bulk_string(String);
encode_server({bulk, _String}, _Options) ->
  error(bad_iolist);
encode_server({error, Reason}, _Options) ->
  encode_error(Reason);
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
  ?EMPTY_BULK_STRING;
encode_bulk_string([]) ->
  ?EMPTY_BULK_STRING;
encode_bulk_string(String) when is_binary(String) ->
  [$$, integer_to_binary(byte_size(String), 10), <<?CRLF>>, String, <<?CRLF>>];
encode_bulk_string(IOList) when is_list(IOList) ->
  try iolist_size(IOList) of
    Size ->
      [$$, integer_to_binary(Size, 10), <<?CRLF>>, IOList, <<?CRLF>>]
  catch
    error:badarg ->
      error(bad_iolist)
  end.

-spec encode_array(resp_terms(), options()) -> iolist().
encode_array([], _Options) ->
  [<<$*, "0", ?CRLF, ?CRLF>>];
encode_array(List, Options) when is_list(List) ->
  [$*, integer_to_binary(length(List)), <<?CRLF>>,
   [encode(Term, Options) || Term <- List], <<?CRLF>>].

-spec encode_ok() -> iolist().
encode_ok() ->
  [<<$+, "OK", ?CRLF>>].

encode_error(Str) when is_binary(Str); is_list(Str) ->
  [$-, simple_string(Str), <<?CRLF>>];
encode_error({Class, Reason})
  when is_atom(Class) orelse is_binary(Class) orelse is_list(Class);
       is_atom(Reason) orelse is_binary(Reason) orelse is_list(Reason) ->
  [$-, upper(simple_string(Class)), <<" ">>, simple_string(Reason), <<?CRLF>>].

not_crlf(Char) ->
  Char =/= $\r andalso Char =/= $\n.

simple_string(Str) when is_binary(Str) ->
  case binary:split(Str, [<<$\r>>, <<$\n>>]) of
    [Str] ->
      Str;
    _Error ->
      error(badarg)
  end;
simple_string(Str) when is_list(Str) ->
  case lists:splitwith(fun not_crlf/1, Str) of
    {Str, []} ->
      Str;
    _Error ->
      error(badarg)
  end;
simple_string(Str) when is_atom(Str) ->
  simple_string(atom_to_binary(Str, utf8)).

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
  encode_array(List, Options);
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

%% Decoding

-spec decode_binary(binary()) -> {resp_term(), binary()} | no_return().
decode_binary(<<$+, Rest/binary>> = _Bin) ->
  decode_simple_string(Rest);
decode_binary(<<$$, Rest/binary>> = _Bin) ->
  decode_bulk_string(Rest);
decode_binary(<<$*, Rest/binary>> = _Bin) ->
  decode_array(Rest);
decode_binary(<<$:, Rest/binary>> = _Bin) ->
  decode_integer(Rest);
decode_binary(<<$-, Rest/binary>> = _Bin) ->
  decode_error(Rest);
decode_binary(<<>>) ->
  error(eof);
decode_binary(_Bin) ->
  error({bad_resp, [unknown]}).

-spec decode_simple_string(binary()) -> {binary() | 'ok', binary()} | no_return().
decode_simple_string(Bin) ->
  read_simple_string(simple_string, Bin).

-spec decode_bulk_string(binary()) -> {binary() | 'nil', binary()} | no_return().
decode_bulk_string(<<"-1\r\n", Rest/binary>>) ->
  {'nil', Rest};
decode_bulk_string(Bin) ->
  try decode_integer(Bin) of
    {Length, Rest0} when Length >= 0 ->
      case Rest0 of
        <<Str:Length/binary, "\r\n", Rest/binary>> ->
          {Str, Rest};
        _Error ->
          error({bad_resp, [bulk_string]})
      end;
    _Error ->
      error({bad_resp, [bulk_string]})
  catch
    error:{bad_resp, [_Pred|Tail]} ->
      error({bad_resp, [bulk_string|Tail]})
  end.

-spec decode_array(binary()) -> {resp_terms() | 'nil', binary()} | no_return().
decode_array(<<"-1\r\n", Rest/binary>>) ->
  {'nil', Rest};
decode_array(Bin) ->
  try decode_integer(Bin) of
    {0, <<"\r\n", Rest0/binary>>} ->
      {[], Rest0};
    {Length, Rest0} when Length > 0 ->
      case  decode_array_n(Rest0, Length, []) of
        {List, <<"\r\n", Rest1/binary>>} ->
          {List, Rest1};
        _Error ->
          error({bad_resp, [array]})
      end;
    _Error ->
      error({bad_resp, [array]})
  catch
    error:{bad_resp, [_Pred|Tail]} ->
      error({bad_resp, [array|Tail]})
  end.

-spec decode_array_n(binary(), non_neg_integer(), resp_terms()) -> {resp_terms(), binary()} | no_return().
decode_array_n(Bin, 0, Accum) ->
  {lists:reverse(Accum), Bin};
decode_array_n(Bin, N, Accum) when N > 0 ->
  case decode(Bin) of
    {ok, Term, Rest} ->
      decode_array_n(Rest, N-1, [Term|Accum]);
    {error, {bad_resp, Tail}} ->
      error({bad_resp, [array|Tail]})
  end.

-spec decode_integer(binary()) -> {integer(), binary()} | no_return().
decode_integer(Bin) ->
  {Str, Rest} = read_simple_string(integer, Bin),
  try binary_to_integer(Str, 10) of
    Int ->
      {Int, Rest}
  catch
    error:badarg ->
      error({bad_resp, [integer]})
  end.

-spec decode_error(binary()) -> {resp_error(), binary()} | no_return().
decode_error(Bin) ->
  {Reason, Rest} = read_simple_string(error, Bin),
  {{error, Reason}, Rest}.

-spec read_simple_string(atom(), binary()) -> {ok | binary(), binary()} | no_return().
read_simple_string(simple_string, <<"OK\r\n", Rest/binary>>) ->
  {ok, Rest};
read_simple_string(Type, Bin) ->
  case binary:split(Bin, <<"\r">>) of
    [Message, <<"\n", Rest/binary>>] ->
      {Message, Rest};
    _Error ->
      error({bad_resp, [Type]})
  end.
