%% @doc Elli fileserve overview
%%
%% This middleware serves static files given a URL prefix and a local path,
%% any request containing "/../" is ignored.

-module(fileserve).

-include_lib("kernel/include/file.hrl").
-include("server.hrl").
-export([handle/1, handle_event/3, file_size/1, get_dir_index/1]).

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
            case filename:basename(Filename) of
                <<"index.html">> ->
                     {Headers, Body} = ?MODULE:get_dir_index(path(Config)),
                     {200, Headers, Body};
                 _ ->
                    case ?MODULE:file_size(Filename) of
                        {error, illegal_path} ->
                            {403, [], <<"Not Allowed">>};
                        {error, _Reason} ->
                            {404, [], <<"Not found">>};
                        {ok, Size} ->
                            {200, headers(Filename, Size, charset(Config)), {file, Filename}}
                    end
            end
    end.

handle_event(_, _, _) ->
    ok.

%%
%% Config
%%

default(Config) ->
    proplists:get_value(default, Config, <<"index.html">>).

path(Config) ->
    proplists:get_value(path, Config, <<"/tmp">>).

prefix(Config) ->
    proplists:get_value(prefix, Config, <<>>).

charset(Config) ->
    proplists:get_value(charset, Config).

%%
%% Helpers
%%

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

local_path(Config, <<"/", File/binary>>) ->
    local_path(Config, File);

local_path(Config, <<"">>) ->
    filename:join(filename:flatten([path(Config), default(Config)]));

local_path(Config, FilePath) ->
    MappedPath = path(Config),
    case binary:match(filename:dirname(FilePath), <<"..">>) of
        nomatch ->
            case binary:last(FilePath) of
                $/ ->
                    filename:join(filename:flatten([MappedPath, FilePath,
                                                    default(Config)]));
                _ ->
                    filename:join(filename:flatten([MappedPath, FilePath]))
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
    case mime_type(Filename) of
        undefined ->
            [{"Content-Length", Size}];
        MimeType ->
            [{"Content-Length", Size}, {"Content-Type", content_type(MimeType, Charset)}]
    end.

content_type(MimeType, undefined) ->
    MimeType;
content_type(MimeType, Charset) ->
    MimeType ++ "; charset=" ++ Charset.

%%
%% Mime types
%%

mime_types() ->
    %ct_expand:term(
        dict:from_list(element(2, httpd_conf:load_mime_types(
             filename:join(code:lib_dir(inets), "examples/server_root/conf/mime.types")))).%).

mime_type(Filename) when is_binary(Filename) ->
    case filename:extension(Filename) of
        <<>> ->
            undefined;
        <<$., Ext/binary>> ->
            case dict:find(binary_to_list(Ext), mime_types()) of
                {ok, MimeType} -> MimeType;
                error          -> undefined
            end
    end.
   
get_dir_index(FilePath) ->
    DirPath = filename:dirname(FilePath),
    {ok, Filenames} = file:list_dir(DirPath),
    Body = list_to_binary(generate_index_template(Filenames)),
    {headers(<<"index.html">>, byte_size(Body), "utf-8"), Body}.

generate_index_template(Filenames) ->
    HTMLList = ["<html><body><ul>"] ++ lists:map(fun format_file/1, Filenames) ++ ["</ul></body></html>"],
    string:join(HTMLList, "").
       
    
format_file(File) ->
    string:join(["<li>", File, "</li>"], " ").
