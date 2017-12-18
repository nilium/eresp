eresp
=====

[![Build Status](https://travis-ci.org/nilium/eresp.svg?branch=master)](https://travis-ci.org/nilium/eresp)
[![Coverage Status](https://coveralls.io/repos/github/nilium/eresp/badge.svg?branch=master)](https://coveralls.io/github/nilium/eresp?branch=master)

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

Maps and tuples are not supported when encoding. Pids, ports, and other types
that are specific to Erlang are also unsupported.

eresp encode/1, encode/2, cmd/1, and cmd/2 output is always an iolist or an
`{error, Reason}` tuple.

### Server Encoding

This is the default encoding when using encode/1 (server encoding) and encode/2.
It's recommended to use resp/1 to ensure your code is clear about its intent
(i.e., is creating a server RESP response).

The full encoding table for server messages can be seen in the edoc
documentation.

### Client Encoding

To encode a command list with eresp, use cmd/1 or cmd/2 (for a command with
arguments):

```erlang
% cmd/1
% Encodes as an array of
% - PING
eresp:cmd(ping).

% cmd/2
% Encodes as an array of
% - PING
% - pong
eresp:cmd(ping, [<<"pong">>]).
```

You can use encode/2 to encode a client value as well, but this is primarily for
advanced and possibly unusual uses only. Using encode/2 requires you to pass the
mode=client option in the options map:

```erlang
% encode/2
% Requires the option mode=client to use client encoding.
% Returns an iolist for <<"$5\r\n12345\r\n">>.
eresp:encode(12345, #{mode => client}).
```

- Everything is encoded as a bulk string.
- Lists are treated as iolists (these are not checked, so it is possible to
  create an invalid message by passing a list of maps or something similarly
  strange).

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
