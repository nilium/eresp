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

-module(eresp_coding_test).

-include_lib("eunit/include/eunit.hrl").

encode_test_gen(Fn, {In, Out, Options}) ->
  fun() ->
      Got = try iolist_to_binary(eresp:Fn(In, Options))
            catch error:Reason -> {error, Reason} end,
      ?assertEqual(Out, Got)
  end;
encode_test_gen(Fn, {In, Out}) ->
  fun() ->
      Got = try iolist_to_binary(eresp:Fn(In))
            catch error:Reason -> {error, Reason} end,
      ?assertEqual(Out, Got)
  end.

encode_server_test_() ->
  {inparallel,

   [[{Desc ++ " (encode/2)", encode_test_gen(encode, {In, Out, #{}})},
     {Desc ++ " (encode/1)", encode_test_gen(encode, {In, Out})}]
    || {Desc, In, Out}
       <- [ %% { Desc, In, Out } = ServerTest

           % Booleans -- these don't follow the convention used by redis's EVAL, but
           % instead encode true/false as integers 1 and 0, since this disambiguates nil
           % and false.
           {"Encode true as the integer 1",
            true,
            <<":1\r\n">>},
           {"Encode false as the integer 0",
            false,
            <<":0\r\n">>},

           {"Encode {error, binary()} as an error",
            {error, <<"ERROR bad message">>},
            <<"-ERROR bad message\r\n">>},

           {"Encode {error, string()} as an error",
            {error, "ERROR bad message"},
            <<"-ERROR bad message\r\n">>},

           {"Encode {error, {Class, Reason}} as an error",
            {error, {error, "bad message"}},
            <<"-ERROR bad message\r\n">>},

           {"Encode nil as a nil string",
            nil,
            <<"$-1\r\n">>},

           % ok is a special case and is sent as a simple string -- this is preferable
           % to converting it to a binary since clients often understand this as
           % a special case.
           {"Encode 'ok' as the simple string OK",
            ok,
            <<"+OK\r\n">>},

           {"Encode binaries as bulk strings",
            <<"foobar">>,
            <<"$6\r\nfoobar\r\n">>},

           {"Encode {bulk, iolist()} tuples as bulk strings",
            {bulk, [<<"foo">>, [$b] | "ar"]},
            <<"$6\r\nfoobar\r\n">>},

           {"Encode empty lists as empty arrays",
            [],
            <<"*0\r\n\r\n">>},

           {"Encode lists as arrays",
            [<<"foo">>, $b, "ar"],
            <<"*3\r\n",
              "$3\r\nfoo\r\n",
              ":98\r\n",
              "*2\r\n",
              ":97\r\n"
              ":114\r\n"
              "\r\n"
              "\r\n"
            >>},

           % Floats
           {"Encoding floats as bulk strings",
            [123.45, -123.45],
            <<"*2\r\n"
              "$6\r\n123.45\r\n"
              "$7\r\n-123.45\r\n"
              "\r\n">>},

           % Integers
           {"Encode integers as RESP integers",
            123456,
            <<":123456\r\n">>},
           {"Encode negative integers as RESP integers",
            -123456,
            <<":-123456\r\n">>},

           %% Invalid

           {"Do not encode invalid iolists as bulk strings",
            {bulk, [<<"foo">>, an_atom, [$b] | "ar"]},
            {error, bad_iolist}},

           {"Do not encode non-iolists as bulk strings",
            {bulk, 123.45},
            {error, bad_iolist}},

           {"Do not encode errors with special characters (\\n; binary)",
            {error, <<"NOPE bad\nerror">>},
            {error, badarg}},

           {"Do not encode errors with special characters (\\r; binary)",
            {error, <<"NOPE bad\rerror">>},
            {error, badarg}},

           {"Do not encode errors with special characters (\\n; list)",
            {error, "NOPE bad\nerror"},
            {error, badarg}},

           {"Do not encode errors with special characters (\\r; list)",
            {error, "NOPE bad\rerror"},
            {error, badarg}},

           {"Do not encode pids",
            self(),
            {error, badarg}}

          ]]}.

cmd_test_gen({Cmd, Args}, Out) ->
  fun() -> ?assertEqual(Out, iolist_to_binary(eresp:cmd(Cmd, Args))) end;
cmd_test_gen(Cmd, Out) ->
  fun() -> ?assertEqual(Out, iolist_to_binary(eresp:cmd(Cmd))) end.

cmd_test_() ->
  {inparallel,

   [{Desc, cmd_test_gen(In, Out)}
    || {Desc, In, Out}
       <- [ %% { Desc, {Cmd, Args} | Cmd, Out } = ClientTest

           %% Use ping as a simple command example
           {"Ping (cmd/1) - atom",
            ping,
            <<"*1\r\n$4\r\nPING\r\n\r\n">>},

           {"Ping (cmd/2) - atom",
            {ping, []},
            <<"*1\r\n$4\r\nPING\r\n\r\n">>},

           {"Ping (cmd/1) - binary",
            <<"Ping">>,
            <<"*1\r\n$4\r\nPING\r\n\r\n">>},

           {"Ping (cmd/1) - list",
            "ping",
            <<"*1\r\n$4\r\nPING\r\n\r\n">>},

           {"Ping (cmd/2) - pong",
            {ping, ["pong"]},
            <<"*2\r\n$4\r\nPING\r\n$4\r\npong\r\n\r\n">>},

           {"Nested lists are iolists",
            {rpush, ["key", ["foo", <<"bar">>]]},
            <<"*3\r\n$5\r\nRPUSH\r\n$3\r\nkey\r\n$6\r\nfoobar\r\n\r\n">>},

           {"All types are bulk strings",
            {type, [-123, "list", <<"bin">>, 123.45, true, false, nil, [], <<>>, [<<>>]]},
            <<"*11\r\n"
              "$4\r\nTYPE\r\n"
              "$4\r\n-123\r\n"
              "$4\r\nlist\r\n"
              "$3\r\nbin\r\n"
              "$6\r\n123.45\r\n"
              "$4\r\ntrue\r\n"
              "$5\r\nfalse\r\n"
              "$3\r\nnil\r\n"
              "$0\r\n\r\n"
              "$0\r\n\r\n"
              "$0\r\n\r\n"
              "\r\n">>}

          ]]}.


decode_test_gen({<<$+, _/binary>> = In, {ok, Term, _} = Out}) when Term =/= ok ->
  % Special case -- only encoding 'ok' will produce a simple string
  fun() -> ?assertEqual(Out, eresp:decode(In)) end;
decode_test_gen({<<"*-1\r\n", _/binary>> = In, Out}) ->
  % Special case --re-encoding nil will only produce a nil bulk string
  fun() -> ?assertEqual(Out, eresp:decode(In)) end;
decode_test_gen({In, {error, _Reason} = Out}) ->
  % Special case -- bad input
  fun() -> ?assertEqual(Out, eresp:decode(In)) end;
decode_test_gen({In, {ok, WantedTerm, _} = Out}) ->
  fun() ->
      io:format("~w~n", [Out]),
      {ok, GotTerm, Rest} = Decoded = eresp:decode(In),
      ?assertEqual(Out, Decoded),
      Encoded = iolist_to_binary(eresp:encode(WantedTerm)),
      ?assertEqual(In, <<Encoded/binary, Rest/binary>>),
      Reencoded = iolist_to_binary(eresp:encode(GotTerm)),
      ?assertEqual(In, <<Reencoded/binary, Rest/binary>>)
  end.

decode_test_() ->
  {inparallel,

   [{Desc ++ " (decode/1)", decode_test_gen({In, Out})}
    || {Desc, In, Out}
       <- [ %% { Desc, In, Out } = DecodeTest

           %% Valid payloads

           {"Decode integers",
            <<":-1234567891011121314151617181920\r\n+Trailing\r\n">>,
            {ok, -1234567891011121314151617181920, <<"+Trailing\r\n">>}},

           {"Decode arrays as lists",
            <<"*12\r\n"
              "*0\r\n\r\n"
              "$5\r\nfalse\r\n"
              "$4\r\ntrue\r\n"
              "$2\r\nOK\r\n"
              "-PROTO protocol error\r\n"
              "+OK\r\n"
              "$-1\r\n"
              "*1\r\n" "$3\r\nfoo\r\n" "\r\n"
              ":-12345\r\n"
              ":12345\r\n"
              "$6\r\n123.45\r\n"
              ":0\r\n"
              "\r\n+Trailing\r\n">>,
            {ok,
             [[],
              <<"false">>,
              <<"true">>,
              <<"OK">>,
              {error, <<"PROTO protocol error">>},
              'ok',
              'nil',
              [<<"foo">>],
              -12345,
              12345,
              <<"123.45">>,
              0
             ],
             <<"+Trailing\r\n">>}
           },

           {"Decode errors as {error, Description} tuples",
            <<"-ERROR decoded error\r\n+Trailing\r\n">>,
            {ok, {error, <<"ERROR decoded error">>}, <<"+Trailing\r\n">>}},

           {"Decode bulk strings as binaries",
            <<"$15\r\nfoobar bazwub\r\n\r\n+Trailing\r\n">>,
            {ok, <<"foobar bazwub\r\n">>, <<"+Trailing\r\n">>}},

           {"Decode the simple string OK as 'ok'",
            <<"+OK\r\n+Trailing\r\n">>,
            {ok, 'ok', <<"+Trailing\r\n">>}},

           {"Decode other simple strings as binaries",
            <<"+PING\r\n+Trailing\r\n">>,
            {ok, <<"PING">>, <<"+Trailing\r\n">>}},

           {"Decode null arrays as 'nil'",
            <<"*-1\r\n+Trailing\r\n">>,
            {ok, 'nil', <<"+Trailing\r\n">>}},

           {"Decode null bulks as 'nil'",
            <<"$-1\r\n+Trailing\r\n">>,
            {ok, 'nil', <<"+Trailing\r\n">>}},

           %% Invalid payloads

           {"Decode an empty binary and return eof",
            <<>>,
            {error, eof}},

           {"Decode an invalid payload and return an error",
            <<"Foobar+Trailing\r\n">>,
            {error, {bad_resp, [unknown]}}},

           {"Decode an invalid integer and return an error",
            <<":123.45\r\n">>,
            {error, {bad_resp, [integer]}}},

           {"Decode an invalid integer (hex-like) and return an error",
            <<":ff\r\n">>,
            {error, {bad_resp, [integer]}}},

           {"Decode an invalid integer (0x-hex-like) and return an error",
            <<":0xff\r\n">>,
            {error, {bad_resp, [integer]}}},

           {"Decode an invalid integer (empty) and return an error",
            <<":\r\n">>,
            {error, {bad_resp, [integer]}}},

           {"Decode a short bulk string and return an error",
            <<"$12\r\nfoo\r\n$13\r\nfoobar bazwub\r\n">>,
            {error, {bad_resp, [bulk_string]}}},

           {"Decode a bulk string with no ending CRLF and return an error",
            <<"$0\r\n">>,
            {error, {bad_resp, [bulk_string]}}},

           {"Decode a long bulk string and return an error",
            <<"$12\r\nfoobar bazwub\r\n">>, % off by one
            {error, {bad_resp, [bulk_string]}}},

           {"Decode a bulk string with a length < -1 and return an error",
            <<"$-13\r\nfoobar bazwub\r\n">>,
            {error, {bad_resp, [bulk_string]}}},

           {"Decode a bulk string with a malformed length and return an error",
            <<"$12.5\r\nfoobar bazwub\r\n">>,
            {error, {bad_resp, [bulk_string]}}},

           {"Decode an unterminated OK simple string",
            <<"+OK">>,
            {error, {bad_resp, [simple_string]}}},

           {"Decode an unterminated simple string",
            <<"+foobar">>,
            {error, {bad_resp, [simple_string]}}},

           {"Decode an invalid simple string (\\n)",
            <<"+foobar\n\r\n">>,
            {error, {bad_resp, [simple_string]}}},

           {"Decode an invalid simple string (\\r)",
            <<"+foobar\rfoobar\r\n">>,
            {error, {bad_resp, [simple_string]}}},

           {"Decode an error without a CRLF and return an error",
            <<"-ERROR bad error">>,
            {error, {bad_resp, [error]}}},

           {"Decode an invalid error (\\n)",
            <<"-ERROR bad error\n\r\n">>,
            {error, {bad_resp, [error]}}},

           {"Decode an invalid error (\\r)",
            <<"-ERROR bad\rerror\r\n">>,
            {error, {bad_resp, [error]}}},

           {"Decode a malformed array and return an error",
            <<"*\r\n+1\r\n">>,
            {error, {bad_resp, [array]}}},

           {"Decode a short array and return an error",
            <<"*1\r\n\r\n">>,
            {error, {bad_resp, [array, unknown]}}},

           {"Decode an array with an invalid length and return an error",
            <<"*1\r\n:1\r\n:2\r\n\r\n">>,
            {error, {bad_resp, [array]}}},

           {"Decode an array with a length < -1 and return an error",
            <<"*-2\r\n:1\r\n:2\r\n\r\n">>,
            {error, {bad_resp, [array]}}},

           {"Decode an array with a malformed length and return an error",
            <<"*0x2\r\n:1\r\n:2\r\n\r\n">>,
            {error, {bad_resp, [array]}}},

           {"Decode an invalid array and return an error",
            <<"*1\r\nFoobar\r\n+Trailing\r\n">>,
            {error, {bad_resp, [array, unknown]}}}
          ]]}.
