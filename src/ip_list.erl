%%%-------------------------------------------------------------------
%%% @author bj
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 四月 2019 下午5:10
%%%-------------------------------------------------------------------
-module(ip_list).
-author("bj").

%% API
-export([
  read/0,
  by_direct/2
]).

read() ->
  {ok, File} = file:open("china_ip_list.txt", read),
  read_loop(File).


read_loop(File) ->
  read_loop(File, #{}).

read_loop(File, Ip_list) ->
  Line_ = io:get_line(File, ''),
  case Line_ of
    eof ->
      Ip_list;

    _ ->
      Line = string:strip(Line_, right, $\n),
      [Ip, Mask_str] = string:tokens(Line, "/"),
      {ok, IPAddr} = inet:parse_address(Ip),
      {Mask, _} = string:to_integer(Mask_str),
      case maps:is_key(Mask, Ip_list) of
        true ->
          Pool = maps:get(Mask, Ip_list),
          read_loop(File, Ip_list#{Mask => Pool#{IPAddr => true}});
        false ->
          read_loop(File, Ip_list#{Mask => #{IPAddr => true}})
      end
  end.


cidr_network({I1, I2, I3, I4}, Bits) when is_integer(Bits) andalso Bits =< 32 ->
  ZeroBits = 8 - (Bits rem 8),
  Last = (16#ff bsr ZeroBits) bsl ZeroBits,

  case (Bits div 8) of
    0 ->
      {(I1 band Last), 0, 0, 0};
    1 ->
      {I1, (I2 band Last), 0, 0};
    2 ->
      {I1, I2, (I3 band Last), 0};
    3 ->
      {I1, I2, I3, (I4 band Last)};
    4 ->
      {I1, I2, I3, I4}
  end.


is_private_ip(IP) ->
  IP_a = cidr_network(IP, 8),
  IP_b = cidr_network(IP, 12),
  IP_c = cidr_network(IP, 16),
  ({10, 0, 0, 0} == IP_a) or ({172, 16, 0, 0} == IP_b) or ({192, 168, 0, 0} == IP_c).


is_cn_ip(Ip_list, Ip) ->
  is_cn_ip(Ip_list, Ip, 32).


is_cn_ip(_Ip_list, _Ip, 0) ->
  false;

is_cn_ip(Ip_list, IP_addr, Mask) ->
  Segment = cidr_network(IP_addr, Mask),

  case maps:find(Mask, Ip_list) of
    error ->
      is_cn_ip(Ip_list, IP_addr, Mask-1);

    {ok, Set} ->
      case maps:find(Segment, Set) of
        {ok, _} ->
          true;

        error ->
          is_cn_ip(Ip_list, IP_addr, Mask-1)
      end
  end.


by_direct(false) ->
  proxy;
by_direct(true) ->
  direct.

by_direct(_, _, true) ->
  direct;

by_direct(Ip_list, IP, false) ->
  by_direct(is_cn_ip(Ip_list, IP)).

by_direct(Ip_list, IP) ->
  by_direct(Ip_list, IP, is_private_ip(IP)).
