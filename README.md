eresp
=====

eresp is a library to encode and decode [RESP](resp), the Redis serialization
protocol.

[resp]: https://redis.io/topics/protocol

Decoding
--------

**TODO**

Encoding
--------

eresp encoding is split into two categories: server and client. This is to
follow the RESP specification, where a client sends flat lists of bulk strings,
and servers have the full range of types supported by Redis.

Maps and tuples are not supported when encoding. Pids, ports, and other types
that are specific to Erlang are also unsupported.

eresp encode/1, encode/2, cmd/1, cmd/2, and resp/1 output is always an iolist or
an `{error, Reason}` tuple.

### Server Encoding

This is the default encoding when using encode/1, encode/2, and is what is used
with the convenience function resp/1. It's recommended to use resp/1 to ensure
your code is clear about its intent (i.e., is creating a server RESP response).

- `'nil'` is a nil bulk string.
- `'true'` and `'false'` are encoded as integers `1` and `0`.
- `'ok'` is encoded as the simple string `OK`.
- Integers are encoded as integers.
- Floats are encoded as bulk strings.
  This is done with `float_to_binary(F, [{decimal, 14}, compact])`.
- Lists are always encoded as arrays -- no effort is made to treat a list as
  a bulk string.
- Atoms other than booleans, `'ok'`, and `'nil'` are encoded as bulk strings.
- Binaries are always bulk strings.

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
