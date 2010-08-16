/* Copyright (c) 2010, Michael Santos <michael.santos@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the author nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include <crack.h>
#include "erl_nif.h"


static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_enomem;
static ERL_NIF_TERM string_dictpath;

static ERL_NIF_TERM error_tuple(ErlNifEnv *env, char *err);


    static int
load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_enomem = enif_make_atom(env, "enomem");

    string_dictpath = enif_make_string(env, GetDefaultCracklibDict(), ERL_NIF_LATIN1);

    return (0);
}


/* 1: password, 2: dictpath */
    static ERL_NIF_TERM
nif_check(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary passwd;
    ErlNifBinary path;

    char *err = NULL;


    if (!enif_inspect_iolist_as_binary(env, argv[0], &passwd))
        return enif_make_badarg(env);

    if (!enif_inspect_iolist_as_binary(env, argv[1], &path))
        return enif_make_badarg(env);

    /* NULL terminate strings */
    if (!enif_realloc_binary(&passwd, passwd.size+1))
        return atom_enomem;
    if (!enif_realloc_binary(&path, path.size+1))
        return atom_enomem;

    passwd.data[passwd.size-1] = '\0';
    path.data[path.size-1] = '\0';

    err = (char *)FascistCheck((char *)passwd.data, (char *)path.data);

    enif_release_binary(&passwd);
    enif_release_binary(&path);

    return ( (err == NULL) ? atom_ok : error_tuple(env, err));
}

    static ERL_NIF_TERM
nif_dictpath(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return string_dictpath;
}

    static ERL_NIF_TERM
error_tuple(ErlNifEnv *env, char *err)
{
    return enif_make_tuple(env, 2,
            atom_error,
            enif_make_string(env, err, ERL_NIF_LATIN1));
}  


static ErlNifFunc nif_funcs[] = {
    {"check", 2, nif_check},
    {"dictpath", 0, nif_dictpath}
};

ERL_NIF_INIT(cerck, nif_funcs, load, NULL, NULL, NULL)


