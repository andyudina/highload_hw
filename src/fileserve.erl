%% @doc Elli fileserve overview
%%
%% This middleware serves static files given a URL prefix and a local path,
%% any request containing "/../" is ignored.

-module(fileserve).

-include_lib("kernel/include/file.hrl").
-include("server.hrl").
-export([handle/1, handle_event/3, file_size/1]).

raw_path(#req{raw_path = Path})  -> Path.

handle(Req) ->
    Config = [{prefix, <<"/">>},
              {path, <<"./">>},
              {charset, "utf-8"}],
    %erlang:display("handling"),
    case unprefix(raw_path(Req), prefix(Config)) of
        undefined ->
            {403, [], <<"undefined">>};
        FilePath ->
            Filename = local_path(Config, FilePath),
            %DecodedFilename = list_to_binary(http_uri:decode(binary_to_list(Filename))),
            %erlang:display("got result")
            %case filename:basename(Filename) of
            %    <<"index.html">> ->
            %         %erlang:display("got index"),
            %         Result = ?MODULE:get_dir_index(Filename),
            %         %erlang:display("got result"),
            %         Result;
            %     _ ->
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
            %end
    end.

handle_event(_, _, _) ->
    ok.

is_dir(FileName) ->
    erlang:display(FileName),
    erlang:display(filename:dirname(FileName)),
    DirName = filename:dirname(FileName),
    <<DirName/binary, "/">> =:= FileName.
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

%local_path(Config, <<"/", File/binary>>) ->
%    DecodedFile = list_to_binary(http_uri:decode(binary_to_list(File))),
%    erlang:display(http_uri:decode(binary_to_list(File))),
%    local_path(Config, DecodedFile);

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

%%
%% Mime types
%%

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
   
%get_dir_index(FilePath) ->
%    DirPath = filename:dirname(FilePath),
%    %{ok, Filenames} = file:list_dir(DirPath),
%    %erlang:display("get dir"),
%    case file:read_file_info(DirPath, [{time, posix}]) of 
%        {ok, _} ->
%            Body = get_index_template(),
%            %erlang:display("got body"),
%            {200, headers(<<"index.html">>, 34, "utf-8"), Body};
%        {error, _} ->
%            {404, [], <<"Not found">>}
%    end.

%generate_index_template(Filenames) ->
%    HTMLList = ["<html><body><ul>"] ++ lists:map(fun format_file/1, Filenames) ++ ["</ul></body></html>"],
%    string:join(HTMLList, "").
       
%get_index_template() ->
%   <<"<html>Directory index file</html>\n">>.  
%format_file(File) ->
%    string:join(["<li>", File, "</li>"], " ").
