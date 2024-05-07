-module(sourceNIF).
-author("David Leon").

-include("sourceNIFdefs.hrl").

-export([init/0]).
-export([nif_preload/0]).
-export([set_source_nif/4, source_get_batches_nif/0, source_more_batches_nif/0]).

-on_load(init/0).

init() ->
    SOURCE_NIF_LIB_PATH = ?NERLNET_PATH++?BUILD_TYPE_RELEASE++"/"++?SOURCE_NIF_LIB,
    erlang:load_nif(SOURCE_NIF_LIB_PATH, 0).

%% make sure nif can be loaded (activates on_load)
nif_preload() -> done.

%% Reads a csv file and returns a binary nerltensor of the given data type
%% This is user responsibility to validate that given file has the correct data type
%% generally - float can read int, but int data cannot read by float
%% data type size (# of bytes) must be equal or larger than data type size
%% E.g. int32 can read int8, int16, int32, but int8 cannot read int16, int32
set_source_nif(_SourceType, _DataType, _BatchSize, _CustomParamsStr) ->
    exit(nif_library_not_loaded).

source_get_batches_nif() ->
    exit(nif_library_not_loaded).

source_more_batches_nif() ->
    exit(nif_library_not_loaded).

