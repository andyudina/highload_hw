-type path() :: binary().
-type args() :: binary().
-type version() :: {0,9} | {1,0} | {1,1}.
-type header() :: {Key::binary(), Value::binary() | string()}.
-type headers() :: [header()].
-type body() :: binary() | iolist().
-type response() :: iolist().
-type http_method() ::  'GET' | 'HEAD'.


-type response_code() :: 100..999.
-type connection_token_atom() :: keep_alive | close.

-type http_range() :: {First::non_neg_integer(), Last::non_neg_integer()} |
                      {offset, Offset::non_neg_integer()} |
                      {suffix, Length::pos_integer()}.

-type range() :: {Offset::non_neg_integer(), Length::non_neg_integer()}.

-type timestamp() :: {integer(), integer(), integer()}.

-record(req, {
          method :: http_method(),
          path :: [binary()],
          args :: [{binary(), any()}],
          raw_path :: binary(),
          version :: version(),
          headers :: headers(),
          body :: body(),
          pid :: pid(),
          socket :: undefined | tcp:socket()
}).

-define(ACCEPT_TIMEOUT, 10000).
-define(REQUEST_TIMEOUT, 60000).
-define(MAX_HEADERS_NUMBER, 100).
-define(HEADERS_TIMEOUT, 10000).
-define(BODY_TIMEOUT, 30000).
-define(MAX_BODY_SIZE, 1024000).
