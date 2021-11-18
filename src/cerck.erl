%%% @copyright 2010-2021, Michael Santos <michael.santos@gmail.com>

%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(cerck).
-include_lib("cerck/include/cerck.hrl").

-export([
    dictpath/0,
    check/1, check/2,
    quality/1,
    has/2, has/3
]).

-on_load(on_load/0).

-type passwd_quality() :: #passwd_quality{}.
-export_type([passwd_quality/0]).

on_load() ->
    erlang:load_nif(niflib(), []).

% @doc Get the cracklib dictionary path
%
% Returns the base path and filename prefix to the cracklib database.
%
-spec dictpath() -> string().
dictpath() ->
    erlang:nif_error(not_implemented).

% @doc Test for weak or predictable passwords, specifying the dictionary path
-spec check(iodata(), iodata()) -> ok | {error, string()}.
check(_, _) ->
    erlang:nif_error(not_implemented).

% @doc Test for weak or predictable passwords
%
% ```
% 1> cerck:check(<<"foobar">>).
% {error,"it is based on a dictionary word"}
% 2> cerck:check(<<"f00b4r">>).
% ok
% '''
-spec check(iodata()) -> ok | {error, string()}.
check(Passwd) ->
    check(Passwd, dictpath()).

% @doc Analyze a password
-spec quality(string() | binary()) -> passwd_quality().
quality(Passwd) when is_binary(Passwd) ->
    quality(binary_to_list(Passwd));
quality(Passwd) when is_list(Passwd) ->
    quality1(Passwd, #passwd_quality{}).

quality1([], Stats) ->
    Stats;
quality1([H | T], #passwd_quality{lower = N} = Stats) when H >= $a, H =< $z ->
    quality1(T, Stats#passwd_quality{lower = N + 1});
quality1([H | T], #passwd_quality{upper = N} = Stats) when H >= $A, H =< $Z ->
    quality1(T, Stats#passwd_quality{upper = N + 1});
quality1([H | T], #passwd_quality{number = N} = Stats) when H >= $0, H =< $9 ->
    quality1(T, Stats#passwd_quality{number = N + 1});
quality1([_ | T], #passwd_quality{other = N} = Stats) ->
    quality1(T, Stats#passwd_quality{other = N + 1}).

% @doc Test password for character types
-spec has(
    'lower' | 'number' | 'other' | 'upper',
    passwd_quality()
) -> boolean().
has(lower, Stats) ->
    has(lower, Stats, 1);
has(upper, Stats) ->
    has(upper, Stats, 1);
has(number, Stats) ->
    has(number, Stats, 1);
has(other, Stats) ->
    has(other, Stats, 1).

% @doc Test password for character types specifying a minimum number
-spec has(
    'lower' | 'number' | 'other' | 'upper',
    passwd_quality(),
    non_neg_integer()
) -> boolean().
has(lower, #passwd_quality{lower = N}, Min) when N >= Min -> true;
has(upper, #passwd_quality{upper = N}, Min) when N >= Min -> true;
has(number, #passwd_quality{number = N}, Min) when N >= Min -> true;
has(other, #passwd_quality{other = N}, Min) when N >= Min -> true;
has(Type, #passwd_quality{}, _) when
    Type == lower; Type == upper; Type == number; Type == other
->
    false.

privdir(File) ->
    filename:join([
        filename:dirname(code:which(?MODULE)),
        "..",
        "priv",
        File
    ]).

niflib() ->
    privdir(?MODULE).
