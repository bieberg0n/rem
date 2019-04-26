%%%-------------------------------------------------------------------
%%% @author bj
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 四月 2019 下午3:58
%%%-------------------------------------------------------------------
-module(main).
-author("bj").

%% API
-export([main/1]).

log(Arg) ->
  io:format(Arg).

log(Format, Args) ->
  io:format(Format, Args).


handle_loop(Lsocket, Rsocket) ->
  receive
    {tcp, Lsocket, Bin} ->
      gen_tcp:send(Rsocket, Bin),
      handle_loop(Lsocket, Rsocket);

    {tcp, Rsocket, Bin} ->
      gen_tcp:send(Lsocket, Bin),
      handle_loop(Lsocket, Rsocket);

    {tcp_closed, _} ->
      log("closed~n")
  end.


handle(Socket, IP_list) ->
  {ok, <<Ver, Len, _Methods:Len/bytes>>} = gen_tcp:recv(Socket, 0),
  gen_tcp:send(Socket, <<5, 0>>),

  {ok, <<Ver, 1, 0, Addr_type>>} = gen_tcp:recv(Socket, 4),
  {ok, Bin} = gen_tcp:recv(Socket, 0),
  case Addr_type of
    1 ->
      <<A:8, B:8, C:8, D:8, Port:16>> = Bin,
      IPAddr = {A, B, C, D},
      Host = integer_to_list(A) ++ "." ++ integer_to_list(B) ++ "." ++ integer_to_list(C) ++ "." ++ integer_to_list(D);

    3 ->
      <<D_len:8, Host_bytes:D_len/bytes, Port:16>> = Bin,
      Host = binary_to_list(Host_bytes),
      {ok, IPAddr} = inet:getaddr(Host, inet)
  end,

  case ip_list:is_cn_ip(IP_list, IPAddr) of
    true ->
      log("direct: ~p:~p~n", [Host, Port]),
      {ok, Rsocket} = gen_tcp:connect(IPAddr, Port, [binary, {active, true}]),
      gen_tcp:send(Socket, <<5, 0, 0, 1, 0, 0, 0, 0, 16#10, 16#10>>);

    false ->
      log("proxy: ~p:~p~n", [Host, Port]),
      {ok, Rsocket} = gen_tcp:connect({127, 0, 0, 1}, 1081, [binary, {active, false}]),
      gen_tcp:send(Rsocket, <<5, 1, 0>>),
      {ok, <<5, 0>>} = gen_tcp:recv(Rsocket, 2),
      gen_tcp:send(Rsocket, <<5, 1, 0, Addr_type, Bin/binary>>),
      inet:setopts(Rsocket, [{active, true}])
  end,
  inet:setopts(Socket, [{active, true}]),
  handle_loop(Socket, Rsocket).


main(_) ->
  IP_list = ip_list:read(),
  tcp_server:start_serv(1080, fun(Socket) -> handle(Socket, IP_list) end).
%%  log("~p~n", [ip_list:cidr_network({{116, 21, 181, 1}, 23})]).
%%  log("~p~n", [ip_list:is_cn_ip(Ip_list, {116, 21, 181, 106})]).
%%  log("~p~n", [ip_list:is_cn_ip(Ip_list, "47.89.69.253")]),
%%  log("~p~n", [ip_list:is_cn_ip(Ip_list, "23.76.73.196")]).
%%  log("~p~n", [maps:get(32, Ip_list)]).
%%  {ok, IPAddr} = inet:getaddr("ip.bjong.me", inet),
%%  log("~p~n", [IPAddr]).