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
-export([encode_client/1, encode_server/1]).
-export([cmd/1, cmd/2]).
-export([decode/1]).

-export_type([command/0, resp_error/0, resp_scalar/0, resp_term/0]).

-type command() :: atom() | binary() | nonempty_string().
-type resp_error() :: {error, string() | binary()}.
-type resp_scalar() :: number() | iolist() | string() | binary() | atom() | boolean() | 'nil'.
-type resp_term() :: [resp_term()] | resp_scalar() | resp_error().


%%====================================================================
%% API functions
%%====================================================================

%% Encoding

%% @doc Encodes an Erlang term using client encoding (e.g., to send to a Redis
%% or similar server). This will encode Term as a bulk string. To encode an
%% array of terms, including a command string, use `cmd/2'.
%%
%% The following types can be encoded as bulk strings:
%%
%% <ul>
%% <li>IOLists</li>
%% <li>Binaries</li>
%% <li>Integers</li>
%% <li>Floats</li>
%% <li>Atoms</li>
%% </ul>
%%
%% @see cmd/2
-spec encode_client(Term)
-> {ok, iolist()} | {error, badarg}
     when Term :: atom() | iolist() | number() | binary().
encode_client(Term) ->
  try encode_client_unsafe(Term) of
    IOList when is_list(IOList) ->
      {ok, IOList}
  catch
    error:badarg ->
      {error, badarg}
  end.

%% @doc Encodes an Erlang term using server encoding (e.g., responses to
%% a client). If the Term cannot be encoded, it fails with a `badarg' error.
%%
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
-spec encode_server(Term)
-> {ok, iolist()} | {error, badarg}
     when Term ::
          [Term]
          | number()
          | atom()
          | binary()
          | {bulk, iolist()}
          | {error, Reason | {Class, Reason}},
          Reason :: atom() | binary() | nonempty_string(),
          Class :: Reason.
encode_server(Term) ->
  try encode_server_unsafe(Term) of
    IOList when is_list(IOList) ->
      {ok, IOList}
  catch
    error:Reason ->
      {error, Reason}
  end.

%% @equiv cmd(Command, [])
-spec cmd(Command :: command()) -> iolist() | {error, badarg}.
cmd(Command) ->
  cmd(Command, []).

%% @doc Encodes a client command as a flat array of bulk strings, containing
%% Command as its command and Args as the arguments to that command.
%%
%% This is intended for use as the client encoding when sending commands to
%% a Redis or RESP-compatible server.
-spec cmd(Command, Args)
-> iolist() | {error, badarg}
     when Command :: command(),
          Args :: [atom() | iolist() | number() | binary()].
cmd(Command, Args) when is_atom(Command), is_list(Args) ->
  cmd(atom_to_binary(Command, utf8), Args);
cmd(Command, Args) when is_binary(Command), is_list(Args);
                        is_list(Command), is_list(Args) ->
  try encode_array([upper(Command)|Args], fun encode_client_unsafe/1) of
    IOList when is_list(IOList) ->
      {ok, IOList}
  catch
    error:badarg ->
      {error, badarg}
  end.

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
-spec decode(binary())
-> Decoded | Error
     when Decoded :: {ok, Term, Rest :: binary()},
          Term :: [Term] | binary() | integer() | 'nil' | 'ok' | {error, binary()},
          Error :: {error,
                    eof
                    | {bad_resp, [integer
                                  | simple_string
                                  | bulk_string
                                  | array
                                  | error
                                  | unknown]}
                   }.
decode(Bin) when is_binary(Bin) ->
  try decode_binary(Bin) of
    {Term, Rest} ->
      {ok, Term, Rest}
  catch
    error:{bad_resp, _} = Reason ->
      {error, Reason};
    error:eof ->
      {error, eof}
  end.


%%====================================================================
%% Internal functions
%%====================================================================

encode_client_unsafe(List) when is_list(List) ->
  encode_bulk_string(List);
encode_client_unsafe(Integer) when is_integer(Integer) ->
  encode_bulk_string(integer_to_binary(Integer, 10));
encode_client_unsafe(Term) ->
  encode_term(Term).

encode_server_unsafe(nil) ->
  encode_nil();
encode_server_unsafe(true) ->
  encode_integer(1);
encode_server_unsafe(false) ->
  encode_integer(0);
encode_server_unsafe(ok) ->
  encode_ok();
encode_server_unsafe({bulk, String}) when is_binary(String); is_list(String) ->
  encode_bulk_string(String);
encode_server_unsafe({bulk, _String}) ->
  error(badarg);
encode_server_unsafe({error, Reason}) ->
  encode_error(Reason);
encode_server_unsafe(Term) ->
  encode_term(Term).


%% Encoding

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
  [$$, integer_to_binary(iolist_size(IOList), 10), <<?CRLF>>, IOList, <<?CRLF>>].

-spec encode_array([resp_term()], fun((resp_term()) -> iolist())) -> iolist().
encode_array([], _EncodeFun) ->
  [<<$*, "0", ?CRLF, ?CRLF>>];
encode_array(List, EncodeFun) when is_list(List) ->
  [$*, integer_to_binary(length(List)), <<?CRLF>>,
   [EncodeFun(Term) || Term <- List], <<?CRLF>>].

-spec encode_ok() -> iolist().
encode_ok() ->
  [<<$+, "OK", ?CRLF>>].

encode_error(Str) when is_binary(Str); is_list(Str) ->
  [$-, simple_string(Str), <<?CRLF>>];
encode_error({Class, Reason})
  when is_atom(Class) orelse is_binary(Class) orelse is_list(Class);
       is_atom(Reason) orelse is_binary(Reason) orelse is_list(Reason) ->
  [$-, upper(simple_string(Class)), <<" ">>, simple_string(Reason), <<?CRLF>>].

simple_string(Str) when is_binary(Str) ->
  case simple_binary_string_loop(Str) of
    true ->
      Str;
    _ ->
      error(badarg)
  end;
simple_string(Str) when is_list(Str) ->
  case simple_list_string_loop(Str) of
    true ->
      Str;
    _ ->
      error(badarg)
  end;
simple_string(Str) when is_atom(Str) ->
  simple_string(atom_to_binary(Str, utf8)).

simple_binary_string_loop(<<>>) -> true;
simple_binary_string_loop(<<$\r, _/binary>>) -> false;
simple_binary_string_loop(<<$\n, _/binary>>) -> false;
simple_binary_string_loop(<<_, Rest/binary>>) -> simple_binary_string_loop(Rest).

simple_list_string_loop([]) -> true;
simple_list_string_loop([$\r|_]) -> false;
simple_list_string_loop([$\n|_]) -> false;
simple_list_string_loop([_|Tail]) -> simple_list_string_loop(Tail).

-spec encode_atom(atom()) -> iolist().
encode_atom(Atom) when is_atom(Atom) ->
  encode_bulk_string(erlang:atom_to_binary(Atom, utf8)).

-spec encode_term(resp_term()) -> iolist().
encode_term(Atom) when is_atom(Atom) ->
  encode_atom(Atom);
encode_term(Binary) when is_binary(Binary) ->
  encode_bulk_string(Binary);
encode_term(Integer) when is_integer(Integer) ->
  encode_integer(Integer);
encode_term(Float) when is_float(Float) ->
  encode_float(Float);
encode_term(List) when is_list(List) ->
  encode_array(List, fun encode_server_unsafe/1);
encode_term(_Term) ->
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
decode_binary(<<$+, Rest/binary>>) ->
  decode_simple_string(Rest);
decode_binary(<<$$, Rest/binary>>) ->
  decode_bulk_string(Rest);
decode_binary(<<$*, Rest/binary>>) ->
  decode_array(Rest);
decode_binary(<<$:, Rest/binary>>) ->
  decode_integer(Rest);
decode_binary(<<$-, Rest/binary>>) ->
  decode_error(Rest);
decode_binary(<<>>) ->
  error(eof);
decode_binary(_Bin) ->
  error({bad_resp, [unknown]}).

-spec decode_simple_string(binary()) -> {binary() | 'ok', binary()} | no_return().
decode_simple_string(Bin) ->
  read_simple_string(Bin, simple_string).

-spec decode_bulk_string(binary()) -> {binary() | 'nil', binary()} | no_return().
decode_bulk_string(<<"-1\r\n", Rest/binary>>) ->
  {'nil', Rest};
decode_bulk_string(Bin) ->
  case decode_size(Bin, bulk_string) of
    {bad_resp, _} = Reason ->
      error(Reason);
    {Length, Rest0} when Length >= 0 ->
      case Rest0 of
        <<Str:Length/binary, "\r\n", Rest/binary>> ->
          {Str, Rest};
        _Error ->
          error({bad_resp, [bulk_string]})
      end;
    _Error ->
      error({bad_resp, [bulk_string]})
  end.

-spec decode_array(binary()) -> {[resp_term()] | 'nil', binary()} | no_return().
decode_array(<<"-1\r\n", Rest/binary>>) ->
  {'nil', Rest};
decode_array(Bin) ->
  case decode_size(Bin, array) of
    {bad_resp, _} = Reason ->
      error(Reason);
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
  end.

-spec decode_array_n(binary(), non_neg_integer(), [resp_term()]) -> {[resp_term()], binary()} | no_return().
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
  case decode_size(Bin, integer) of
    {bad_resp, _} = Reason -> error(Reason);
    Result -> Result
  end.

decode_size(<<$-, Rest/binary>>, Type) ->
  {Pos, Rest1} = decode_integer_loop(Rest, 0, 0, Type),
  {-Pos, Rest1};
decode_size(Bin, Type) ->
  decode_integer_loop(Bin, 0, 0, Type).

decode_integer_loop(<<N:8/integer, Rest/binary>>, Bytes, Acc, Type) when N >= $0, N =< $9 ->
  decode_integer_loop(Rest, Bytes+1, Acc*10 + (N - $0), Type);
decode_integer_loop(<<?CRLF, Rest/binary>>, Bytes, Acc, _Type) when Bytes > 0 ->
  {Acc, Rest};
decode_integer_loop(_Bin, _Bytes, _Acc, Type) ->
  {bad_resp, [Type]}.

-spec decode_error(binary()) -> {resp_error(), binary()} | no_return().
decode_error(Bin) ->
  {Reason, Rest} = read_simple_string(Bin, error),
  {{error, Reason}, Rest}.

-spec read_simple_string(binary(), error | integer | simple_string) -> {ok | binary(), binary()} | no_return().
read_simple_string(Bin, Type) ->
  {Msg, Rest0} = split_binary(Bin, read_simple_string_loop(Bin, Type, 0)),
  {_CRLF, Rest1} = split_binary(Rest0, 2),
  case Msg of
    <<"OK">> -> {ok, Rest1};
    _ -> {Msg, Rest1}
  end.

read_simple_string_loop(<<?CRLF, _/binary>>, _Type, Index) ->
  Index;
read_simple_string_loop(<<Byte:8/integer, _/binary>>, Type, _Index)
  when Byte =:= $\r;
       Byte =:= $\n ->
  error({bad_resp, [Type]});
read_simple_string_loop(<<>>, Type, _Index) ->
  error({bad_resp, [Type]});
read_simple_string_loop(<<_Byte:8/integer, Rest/binary>>, Type, Index) ->
  read_simple_string_loop(Rest, Type, Index+1).
