-module(cerck_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("cerck/include/cerck.hrl").

-export([
    all/0
    ]).

-export([
        fail/1,
        pass/1,
        quality/1
    ]).

all() ->
    [fail, pass, quality].

fail(_Config) ->
    {error, "it is based on a dictionary word"} = cerck:check(<<"foobar">>),
    ok.

pass(_Config) ->
    ok = cerck:check(<<"f00b4r">>).

quality(_Config) ->
    Q = cerck:quality(<<"f00b4r">>),
    true = cerck:has(number, Q),
    false = cerck:has(other, Q),
    ok.
