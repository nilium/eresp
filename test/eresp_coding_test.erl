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

encode_test_gen({In, Out, Options}) ->
   fun() ->
         ?assertEqual(Out, iolist_to_binary(eresp:encode(In, Options)))
   end;
encode_test_gen({In, Out}) ->
   fun() ->
         ?assertEqual(Out, iolist_to_binary(eresp:encode(In)))
   end.

encode_server_test_() ->
   {inparallel,

    [[{Desc ++ " (encode/2)", encode_test_gen({In, Out, #{}})},
      {Desc ++ " (encode/1)", encode_test_gen({In, Out})}]
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

            % nil is treated as a nil array
            {"Encode nil as a nil string",
             nil,
             <<"$-1\r\n">>},

            % ok is a special case and is sent as a simple string -- this is preferable
            % to converting it to a binary since clients often understand this as
            % a special case. If you passed the 'client
            {"Encode 'ok' as the simple string OK",
             ok,
             <<"+OK\r\n">>},

            % Encode binaries as bulk strings
            {"Encode binaries as bulk strings",
             <<"foobar">>,
             <<"$6\r\nfoobar\r\n">>},

            {"Encode lists as lists",
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
             <<":-123456\r\n">>}
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
