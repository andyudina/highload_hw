-module(fileserve).

-include_lib("kernel/include/file.hrl").
-include("server.hrl").
-export([handle/1, handle_event/3, file_size/1]).

raw_path(#req{raw_path = Path})  -> Path.

handle(Req) ->
    Config = [{prefix, <<"/">>},
              {path, <<"./">>},
              {charset, "utf-8"}],
    case unprefix(raw_path(Req), prefix(Config)) of
        undefined ->
            {403, [], <<"undefined">>};
        FilePath ->
            Filename = local_path(Config, FilePath),
            case ?MODULE:file_size(Filename) of
                {error, illegal_path} ->
                    {403, [], <<"Not Allowed">>};
                {error, _Reason} ->   
                    case is_dir(raw_path(Req)) of
                        true ->  
                            {403, [], <<"Not Allowed">>};
                        false ->                          
                            {404, [], <<"Not found">>}
                        end;
                {ok, Size} ->
                    case Req#req.method of
                        'HEAD' -> 
                            {200, headers(Filename, Size, charset(Config)), <<>>};
                        _ ->
                            {200, headers(Filename, Size, charset(Config)), {file, Filename}}
                    end 
             end
    end.

handle_event(_, _, _) ->
    ok.

is_dir(FileName) ->
    DirName = filename:dirname(FileName),
    <<DirName/binary, "/">> =:= FileName.

default(Config) ->
    proplists:get_value(default, Config, <<"index.html">>).

path(Config) ->
    proplists:get_value(path, Config, <<"/tmp">>).

prefix(Config) ->
    proplists:get_value(prefix, Config, <<>>).

charset(Config) ->
    proplists:get_value(charset, Config).


unprefix(RawPath, {regex, Prefix}) ->
    case re:run(RawPath, Prefix, [{capture, all, binary}]) of
        nomatch ->
            undefined;
        _Result ->
            re:replace(RawPath, Prefix, "", [{return, binary}])
    end;

unprefix(RawPath, Prefix) ->
    PrefixSz = size(Prefix),
    case RawPath of
        <<Prefix:PrefixSz/binary, File/binary>> ->
            File;
        _ ->
            undefined
    end.


local_path(Config, <<"">>) ->
    filename:join(filename:flatten([path(Config), default(Config)]));

local_path(Config, FilePath) ->
    MappedPath = path(Config),
    DecodedFilePath = list_to_binary(http_uri:decode(binary_to_list(FilePath))),
    case binary:match(filename:dirname(DecodedFilePath), <<"..">>) of
        nomatch ->
            case binary:last(DecodedFilePath) of
                $/ ->
                    filename:join(filename:flatten([MappedPath, DecodedFilePath,
                                                    default(Config)]));
                _ ->
                    filename:join(filename:flatten([MappedPath, DecodedFilePath]))
            end;
        _       -> undefined
    end.

file_size(undefined) ->
    {error, illegal_path};

file_size(Filename) ->
    case file:read_file_info(Filename, [{time, posix}]) of
        {ok, #file_info{type = regular, access = Perm, size = Size}}
                when Perm =:= read orelse Perm =:= read_write ->
            {ok, Size};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, invalid_file}
    end.

headers(Filename, Size, Charset) ->
    case mime_type(Filename, Charset) of
        undefined ->
            [{"Content-Length", Size}];
        {MimeType, Charset} ->
            [{"Content-Length", Size}, {"Content-Type", content_type(MimeType, Charset)}];
        MimeType ->
            [{"Content-Length", Size}, {"Content-Type", content_type(MimeType, undefined)}]
    end.

content_type(MimeType, undefined) ->
    MimeType;
content_type(MimeType, Charset) ->
    MimeType ++ "; charset=" ++ Charset.


mime_type(Filename, Charset) when is_binary(Filename) ->
    case filename:extension(Filename) of
        <<".html">> -> "text/html";
        <<".txt">>  -> {"text", Charset};
        <<".css">>  -> "text/css";
        <<".js">>   -> "application/x-javascript";
        <<".jpg">>  -> "image/jpeg";
        <<".jpeg">> -> "image/jpeg";
        <<".png">>  -> "image/png";
        <<".gif">>  -> "image/gif";
        <<".swf">>  -> "application/x-shockwave-flash";
        _ ->
            undefined
    end.
