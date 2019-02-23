eresp
=====

[![CircleCI](https://circleci.com/gh/nilium/eresp/tree/master.svg?style=svg)](https://circleci.com/gh/nilium/eresp/tree/master)
[![codecov](https://codecov.io/gh/nilium/eresp/branch/master/graph/badge.svg)](https://codecov.io/gh/nilium/eresp)

eresp is a library to encode and decode [RESP][resp], the Redis serialization
protocol.

[resp]: https://redis.io/topics/protocol

Decoding
--------

eresp decoding is fairly straight-forward and converts binary RESP strings to
Erlang terms.

The table of RESP to Erlang decoding rules is in the edoc documentation.

Decoding is fairly simple, however:

```erlang
RESP = <<"$3\r\nFoo\r\n$3\r\nbar\r\n">>.

% Parse first term
{ok, Term1, Rest1} = eresp:decode(RESP).
io:format("Term1 = ~s~n", [Term1]).
% => Term1 = foo

% Parse next term
{ok, Term2, Rest2} = eresp:decode(Rest1).
io:format("Term2 = ~s~n", [Term2]).
% => Term2 = bar

% Done
{error, eof} = eresp:decode(Rest2).
```

Encoding
--------

eresp encoding is split into two categories: server and client. This is to
follow the RESP specification, where a client sends flat lists of bulk strings,
and servers have the full range of types supported by Redis.

Maps and tuples are not supported when encoding (maps are unsupported due to
poor iteration support, tuples are used only in special cases). Pids, ports, and
other types that are specific to Erlang are also unsupported.

encode\_server/1, encode\_client/1, cmd/1, and cmd/2 output is always a tuple of
`{ok, iolist()}` or `{error, badarg}`.

### Server Encoding

This is the encoding used by encode\_server/1. The full encoding table for
server messages can be seen in the edoc documentation.

### Client Encoding

Client encoding simply means encoding a term as a bulk string, which is all
encode\_client/1 will do. When using cmd/1 or cmd/2, it will encode a list using
client encoding and return the resulting iolist.

To encode a command list with eresp, use cmd/1 or cmd/2 (for a command with
arguments):

```erlang
% cmd/1
% Encodes as an array of
% - PING
{ok, Ping} = eresp:cmd(ping).

% cmd/2
% Encodes as an array of
% - PING
% - pong
{ok, PingPong} = eresp:cmd(ping, [<<"pong">>]).
```

You can use encode/2 to encode a client value as well, but this is primarily for
advanced and possibly unusual uses only. Using encode/2 requires you to pass the
mode=client option in the options map:

```erlang
% encode/2
% Requires the option mode=client to use client encoding.
% Returns an iolist for <<"$5\r\n12345\r\n">>.
{ok, Msg} = eresp:encode_client(12345).
```

- Everything is encoded as a bulk string.
- Lists are treated as iolists (these are not checked, so it is possible to
  create a non-iolist by passing a list of maps or something similarly strange).

Build
-----

    $ rebar3 compile

Run Tests
---------

    $ rebar3 as test do eunit -c, cover -v, dialyzer

Contributing
------------

Contributions are welcome, just submit a pull request. If you're not sure about
something, submit an issue first to talk about it. Patches require a couple
things:

- Eunit tests must pass.
- Dialyzer must pass.
- New code must have useful test coverage.

If reporting an issue, try to include a code sample that reproduces the issue
(especially if it's usable as an eunit test).
