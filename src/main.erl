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


-record(request, {
  socket,
  addr_type,
  raw_bin,
  host,
  ip,
  port
}).


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
      log("closed~n");

    N ->
      log("unknown msg: ~p~n", [N])
  end.


handle_https_loop(L_socket, R_socket) ->
  receive
    {tcp, L_socket, Bin} ->
%%      io:format("L: ~p~n", [Bin]),
      ssl:send(R_socket, Bin),
      handle_https_loop(L_socket, R_socket);

    {ssl, R_socket, Bin} ->
%%      io:format("R: ~p~n", [Bin]),
      gen_tcp:send(L_socket, Bin),
      handle_https_loop(L_socket, R_socket);

    {tcp_closed, _} ->
      log("closed~n");

    {ssl_closed, _} ->
      log("closed~n");

    N ->
%%      {ssl, R_socket, Bin} = N,
      log("unknown msg: ~p~n", [N])
  end.


recv_https(Sock, Bin) ->
  receive
    {ssl, Sock, B} ->
      NewBin = list_to_binary([Bin, B]),
      Len = (size(NewBin) - 4) * 8,
      case NewBin of
        <<_:Len, "\r\n\r\n">> ->
%%          log("~p~n", [NewBin]),
          ok;
        _ ->
          recv_https(Sock, NewBin)
      end
  end.


route(Req, direct, _) when is_record(Req, request) ->
  #request{socket = Socket, host = Host, ip = IP, port = Port} = Req,

  log("direct: ~p:~p~n", [Host, Port]),
  {ok, R_socket} = gen_tcp:connect(IP, Port, [binary, {active, true}]),
  gen_tcp:send(Socket, <<5, 0, 0, 1, 0, 0, 0, 0, 16#10, 16#10>>),

  inet:setopts(Socket, [{active, true}]),
  handle_loop(Socket, R_socket);

route(Req, proxy, {"socks5", P_host, P_port, _}) when is_record(Req, request) ->
  #request{socket = Socket, addr_type = Addr_type, raw_bin = Bin, host = Host, port = Port} = Req,

  log("proxy: ~p:~p~n", [Host, Port]),
  {ok, R_socket} = gen_tcp:connect(P_host, P_port, [binary, {active, false}]),
  gen_tcp:send(R_socket, <<5, 1, 0>>),
  {ok, <<5, 0>>} = gen_tcp:recv(R_socket, 2),
  gen_tcp:send(R_socket, <<5, 1, 0, Addr_type, Bin/binary>>),

  inet:setopts(Socket, [{active, true}]),
  inet:setopts(R_socket, [{active, true}]),
  handle_loop(Socket, R_socket);


route(Req, proxy, {"https", P_host, P_port, Key}) when is_record(Req, request) ->
  #request{socket = Socket, addr_type = Addr_type, raw_bin = Bin, host = Host, port = Port} = Req,
  log("proxy: ~p:~p~n", [Host, Port]),

  {ok, R_socket} = ssl:connect(P_host, P_port, [{active, true}]),
  case Key of
    "" ->
      Proxy_auth = "";
    K ->
      Proxy_auth = "Proxy-Authorization: Basic " ++ K ++"\r\n"
  end,
%%  log("~p~n", [<<"CONNECT ", (list_to_binary(Host))/binary, ":", (integer_to_binary(Port))/binary, " HTTP/1.1\r\n", (list_to_binary(Proxy_auth))/binary, "Proxy-Connection: Keep-Alive\r\n\r\n">>]),
  ssl:send(R_socket, list_to_binary(["CONNECT ", Host, ":", integer_to_binary(Port), " HTTP/1.1\r\n", Proxy_auth, "Proxy-Connection: Keep-Alive\r\n\r\n"])),
  recv_https(R_socket, <<>>),

  gen_tcp:send(Socket, <<5, 0, 0, 1, 0, 0, 0, 0, 16#10, 16#10>>),
  inet:setopts(Socket, [{active, true}]),
  handle_https_loop(Socket, R_socket);


route(IP_list, Req, Proxy_cfg) when is_record(Req, request) ->
  #request{ip = IP} = Req,
  route(Req, ip_list:by_direct(IP_list, IP), Proxy_cfg).


handle(Socket, IP_list, Proxy_Cfg) ->
  {ok, <<Ver, Len, _Methods:Len/bytes>>} = gen_tcp:recv(Socket, 0),
  gen_tcp:send(Socket, <<5, 0>>),

  {ok, <<Ver, 1, 0, Addr_type>>} = gen_tcp:recv(Socket, 4),
  {ok, Bin} = gen_tcp:recv(Socket, 0),
  case Addr_type of
    1 ->
      <<A:8, B:8, C:8, D:8, Port:16>> = Bin,
      IP = {A, B, C, D},
      Host = integer_to_list(A) ++ "." ++ integer_to_list(B) ++ "." ++ integer_to_list(C) ++ "." ++ integer_to_list(D),
      Request = #request{socket = Socket, addr_type = Addr_type, raw_bin = Bin, host = Host, ip = IP, port = Port},
      route(IP_list, Request, Proxy_Cfg);

    3 ->
      <<D_len:8, Host_bytes:D_len/bytes, Port:16>> = Bin,
      Host = binary_to_list(Host_bytes),
      {ok, IP} = inet:getaddr(Host, inet),
      Request = #request{socket = Socket, addr_type = Addr_type, raw_bin = Bin, host = Host, ip = IP, port = Port},
      route(IP_list, Request, Proxy_Cfg)
  end.


main(_) ->
  ok = application:start(asn1),
  ok = application:start(crypto),
  ok = application:start(public_key),
  ok = application:start(ssl),

  {ok, [[Host]]} = init:get_argument(h),
  {ok, [[P]]} = init:get_argument(p),
  Port = list_to_integer(P),
  {ok, [[Proto]]} = init:get_argument(m),
  {ok, [[Listen_p]]} = init:get_argument(l),
  Listen_port = list_to_integer(Listen_p),
  {ok, [[Key]]} = init:get_argument(k),
  log("~p~n", [{Host, Port, Proto, Listen_port, Key}]),

  IP_list = ip_list:read(),
  tcp_server:start_serv(Listen_port, fun(Socket) -> handle(Socket, IP_list, {Proto, Host, Port, Key}) end).
