%% Copyright 2017 Noel Cower
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(eresp_coding_test).

-include_lib("eunit/include/eunit.hrl").

%% Server Encoding

encode_test_gen(Fn, {In, Out}) ->
  fun() ->
      Got = case eresp:Fn(In) of
              {ok, IOList} -> {ok, iolist_to_binary(IOList)};
              {error, _} = Error -> Error
            end,
      ?assertEqual(Out, Got)
  end.

encode_server_test_() ->
  {inparallel,

   [{Desc ++ " (encode_server/1)", encode_test_gen(encode_server, {In, Out})}
    || {Desc, In, Out}
       <- [ %% { Desc, In, Out } = ServerTest

           % Booleans -- these don't follow the convention used by redis's EVAL, but
           % instead encode true/false as integers 1 and 0, since this disambiguates nil
           % and false.
           {"Encode true as the integer 1",
            true,
            {ok, <<":1\r\n">>}},
           {"Encode false as the integer 0",
            false,
            {ok, <<":0\r\n">>}},

           {"Encode {error, binary()} as an error",
            {error, <<"ERROR bad message">>},
            {ok, <<"-ERROR bad message\r\n">>}},

           {"Encode {error, string()} as an error",
            {error, "ERROR bad message"},
            {ok, <<"-ERROR bad message\r\n">>}},

           {"Encode {error, {Class, Reason}} as an error",
            {error, {error, "bad message"}},
            {ok, <<"-ERROR bad message\r\n">>}},

           {"Encode nil as a nil string",
            nil,
            {ok, <<"$-1\r\n">>}},

           % ok is a special case and is sent as a simple string -- this is preferable
           % to converting it to a binary since clients often understand this as
           % a special case.
           {"Encode 'ok' as the simple string OK",
            ok,
            {ok, <<"+OK\r\n">>}},

           {"Encode binaries as bulk strings",
            <<"foobar">>,
            {ok, <<"$6\r\nfoobar\r\n">>}},

           {"Encode {bulk, iolist()} tuples as bulk strings",
            {bulk, [<<"foo">>, [$b] | "ar"]},
            {ok, <<"$6\r\nfoobar\r\n">>}},

           {"Encode empty lists as empty arrays",
            [],
            {ok, <<"*0\r\n">>}},

           {"Encode lists as arrays",
            [<<"foo">>, $b, "ar"],
            {ok, <<"*3\r\n",
              "$3\r\nfoo\r\n",
              ":98\r\n",
              "*2\r\n",
              ":97\r\n"
              ":114\r\n"
            >>}},

           % Floats
           {"Encode floats as bulk strings",
            [123.45, -123.45],
            {ok, <<"*2\r\n"
              "$6\r\n123.45\r\n"
              "$7\r\n-123.45\r\n"
                 >>}},

           % Integers
           {"Encode integers as RESP integers",
            123456,
            {ok, <<":123456\r\n">>}},
           {"Encode negative integers as RESP integers",
            -123456,
            {ok, <<":-123456\r\n">>}},

           %% Invalid

           {"Do not encode invalid iolists as bulk strings",
            {bulk, [<<"foo">>, an_atom, [$b] | "ar"]},
            {error, badarg}},

           {"Do not encode non-iolists as bulk strings",
            {bulk, 123.45},
            {error, badarg}},

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


%% Client Encoding

encode_client_test_() ->
  {inparallel,

   [{Desc ++ " (encode_client/1)", encode_test_gen(encode_client, {In, Out})}
    || {Desc, In, Out}
       <- [ %% { Desc, In, Out } = ServerTest

           % Booleans -- these don't follow the convention used by redis's EVAL, but
           % instead encode true/false as integers 1 and 0, since this disambiguates nil
           % and false.
           {"Encode true as a bulk string",
            true,
            {ok, <<"$4\r\ntrue\r\n">>}},
           {"Encode false as a bulk string",
            false,
            {ok, <<"$5\r\nfalse\r\n">>}},

           {"Encode {error, binary()} fails",
            {error, <<"ERROR bad message">>},
            {error, badarg}},

           {"Encode {error, string()} fails",
            {error, "ERROR bad message"},
            {error, badarg}},

           {"Encode {error, {Class, Reason}} fails",
            {error, {error, "bad message"}},
            {error, badarg}},

           {"Encode nil as a bulk string",
            nil,
            {ok, <<"$3\r\nnil\r\n">>}},

           % ok is a special case and is sent as a simple string -- this is preferable
           % to converting it to a binary since clients often understand this as
           % a special case.
           {"Encode 'ok' as a bulk string",
            ok,
            {ok, <<"$2\r\nok\r\n">>}},

           {"Encode binaries as bulk strings",
            <<"foobar">>,
            {ok, <<"$6\r\nfoobar\r\n">>}},

           {"Encode {bulk, iolist()} fails",
            {bulk, [<<"foo">>, [$b] | "ar"]},
            {error, badarg}},

           {"Encode empty lists as bulk strings",
            [],
            {ok, <<"$0\r\n\r\n">>}},

           {"Encode lists as iolist bulk strings",
            [<<"foo">>, $b, "ar"],
            {ok, <<"$6\r\nfoobar\r\n">>}},

           % Floats
           {"Encode floats as bulk strings",
            -123.45,
            {ok, <<"$7\r\n-123.45\r\n">>}},

           % Integers
           {"Encode integers as bulk strings",
            123456,
            {ok, <<"$6\r\n123456\r\n">>}},

           {"Encode negative integers as bulk strings",
            -123456,
            {ok, <<"$7\r\n-123456\r\n">>}},

           %% Invalid

           {"Do not encode invalid iolists as bulk strings",
            {bulk, [<<"foo">>, an_atom, [$b] | "ar"]},
            {error, badarg}},

           {"Do not encode pids",
            self(),
            {error, badarg}}

          ]]}.


cmd_test_gen({Cmd, Args}, Out) ->
  fun() ->
      Got = case eresp:cmd(Cmd, Args) of
              {error, _} = Error -> Error;
              {ok, IOList} -> {ok, iolist_to_binary(IOList)}
            end,
      ?assertEqual(Out, Got)
  end;
cmd_test_gen(Cmd, Out) ->
  fun() ->
      Got = case eresp:cmd(Cmd) of
              {error, _} = Error -> Error;
              {ok, IOList} -> {ok, iolist_to_binary(IOList)}
            end,
      ?assertEqual(Out, Got)
  end.

cmd_test_() ->
  {inparallel,

   [{Desc, cmd_test_gen(In, Out)}
    || {Desc, In, Out}
       <- [ %% { Desc, {Cmd, Args} | Cmd, Out } = ClientTest

           %% Use ping as a simple command example
           {"Ping (cmd/1) - atom",
            ping,
            {ok, <<"*1\r\n$4\r\nPING\r\n">>}},

           {"Ping (cmd/2) - atom",
            {ping, []},
            {ok, <<"*1\r\n$4\r\nPING\r\n">>}},

           {"Ping (cmd/1) - binary",
            <<"Ping">>,
            {ok, <<"*1\r\n$4\r\nPING\r\n">>}},

           {"Ping (cmd/1) - list",
            "ping",
            {ok, <<"*1\r\n$4\r\nPING\r\n">>}},

           {"Ping (cmd/2) - pong",
            {ping, ["pong"]},
            {ok, <<"*2\r\n$4\r\nPING\r\n$4\r\npong\r\n">>}},

           {"Nested lists are iolists",
            {rpush, ["key", ["foo", <<"bar">>]]},
            {ok, <<"*3\r\n$5\r\nRPUSH\r\n$3\r\nkey\r\n$6\r\nfoobar\r\n">>}},

           {"All types are bulk strings",
            {type, [-123, "list", <<"bin">>, 123.45, true, false, nil, [], <<>>, [<<>>]]},
            {ok, <<"*11\r\n"
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
              "$0\r\n\r\n">>}},

           %% Invalid messages

           {"Do not encode tuples",
            {encode, [{bulk, [<<"foobar">>]}]},
            {error, badarg}}

          ]]}.


decode_test_gen({<<$+, _/binary>> = In, {ok, Term, _} = Out}) when Term =/= ok ->
  % Special case -- only encoding 'ok' will produce a simple string
  fun() -> ?assertEqual(Out, eresp:decode(In)) end;
decode_test_gen({{decode_only, In}, {ok, Term, _} = Out}) when Term =/= ok ->
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
      {ok, EncodedIOList} = eresp:encode_server(WantedTerm),
      Encoded = iolist_to_binary(EncodedIOList),
      ?assertEqual(In, <<Encoded/binary, Rest/binary>>),
      {ok, ReencodedIOList} = eresp:encode_server(GotTerm),
      Reencoded = iolist_to_binary(ReencodedIOList),
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
              "*0\r\n"
              "$5\r\nfalse\r\n"
              "$4\r\ntrue\r\n"
              "$2\r\nOK\r\n"
              "-PROTO protocol error\r\n"
              "+OK\r\n"
              "$-1\r\n"
              "*1\r\n" "$3\r\nfoo\r\n"
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
             <<"\r\n+Trailing\r\n">>}
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

           {"Decode a simple array",
            {decode_only, <<"*1\r\n+1\r\n\r\n">>},
            {ok, [<<"1">>], <<"\r\n">>}},

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

           {"Decode a short array and return an error",
            <<"*1\r\n\r\n">>,
            {error, {bad_resp, [array, unknown]}}},

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
