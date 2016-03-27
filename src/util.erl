-module(util).
-include("server.hrl").
-include("util.hrl").

-include_lib("kernel/include/file.hrl").

-export([  encode_range/2
         , file_size/1
        ]).


-spec encode_range(Range::range() | invalid_range,
                   Size::non_neg_integer()) -> ByteRange::iolist().
                   
encode_range(Range, Size) ->
    [<<"bytes ">>, encode_range_bytes(Range),
     <<"/">>, integer_to_list(Size)].

encode_range_bytes({Offset, Length}) ->
    [integer_to_list(Offset), <<"-">>, integer_to_list(Offset + Length - 1)];
encode_range_bytes(invalid_range) -> <<"*">>.


-spec file_size(Filename::file:name()) ->
                       non_neg_integer() | {error, Reason}
                           when Reason :: badarg | file:posix().
file_size(Filename) ->
    case file:read_file_info(Filename) of
        {ok, #file_info{size = Size}} -> Size;
        {error, Reason}               -> {error, Reason}
    end.
