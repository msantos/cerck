Erlang interface to libcrack.


## HOW TO BUILD IT

    apt-get install libcrack2-dev
    make


## HOW TO USE IT

    check(Password) -> ok | {error, string()}
        Types   Passwod = [ binary() | string() ]
    
    quality(Password) -> #passwd_quality{}
    
    has(Type, Stats) -> true | false
        Types   Type = [ upper | lower | number | other ]
                Stats = #passwd_quality{}
    
    has(Type, Stats, Min) -> true | false
        Types   Type = [ upper | lower | number | other ]
                Stats = #passwd_quality{}
                Min = integer()

## EXAMPLES

    1> cerck:check(<<"foobar">>).
    {error,"it is based on a dictionary word"}
    2> 4> cerck:check(<<"f00b4r">>).
    ok

For quality checks:

    1> rr("include/cerck.hrl").
    [passwd_quality]
    2> S = cerck:quality(<<"f00b4r">>).
    #passwd_quality{upper = 0,lower = 3,number = 3,other = 0}
    3> cerck:has(number, S).
    true
    4> cerck:has(other, S). 
    false


## *** WARNING ***

> Do NOT call check/2 without ensuring the dictionary files (.pwi, .pwd
> and .hwm extensions) exist and are valid. If the files do not exist,
> cracklib calls exit(-1), causing the Erlang VM to exit.


## TODO

* do all of this in pure Erlang
