%%%-------------------------------------------------------------------
%%% @author zdeneksejcek
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Nov 2014 10:22 AM
%%%-------------------------------------------------------------------
-author("zdeneksejcek").

not_unknown([]) -> true;
not_unknown([undefined|_]) ->
    false;
not_unknown([_|H]) ->
    not_unknown(H).

valid_date_or(undefined, Or_date) ->
    Or_date;
valid_date_or(Date, Or_date) ->
    case calendar:valid_date(Date) of
        true -> Date;
        false -> Or_date
    end.

today_date() ->
    {Date, _} = calendar:universal_time(),
    Date.

search_key(Key, [{Key,_}=V|_]) -> {ok, V};
search_key(Key, [_|T]) -> search_key(Key, T);
search_key(_, []) -> not_found.

get_req_tagged_value(List, Key) ->
    case search_key(Key, List) of
        {ok, {_, Value}} -> Value;
        not_found -> throw(value_not_present)
    end.
