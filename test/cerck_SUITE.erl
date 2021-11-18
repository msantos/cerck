-module(cerck_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("cerck/include/cerck.hrl").

-export([
    all/0
]).

-export([
    fail/1,
    pass/1,
    quality/1,
    bad_dictionary/1
]).

all() ->
    [fail, pass, quality, bad_dictionary].

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

bad_dictionary(Config) ->
    Bad_dict = filename:join([?config(data_dir, Config), "test"]),
    {error, "error loading dictionary"} = cerck:check(<<"foobar">>, <<"/foo">>),
    {error, "error loading dictionary"} = cerck:check(<<"foobar">>, Bad_dict).
