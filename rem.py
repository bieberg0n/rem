
# copyright 2016 bjong

from struct import pack, unpack
import sys
import asyncio
import socket
import json
import IPInCN
# import gevent
# from gevent import socket, spawn
# from gevent.server import StreamServer


async def get_dst(reader_c):
    r = await reader_c.read(4)
    if r[:3] == b'\x05\x01\x00':
        if r[-1:] == b'\x01':
            r = await reader_c.read(4)
            dst = '.'.join([str(i) for i in unpack('BBBB', r)])
        elif r[-1:] == b'\x03':
            dst_length_bytes = await reader_c.read(1)
            dst_length = unpack('B', dst_length_bytes)[0]
            dst_bytes = await reader_c.read(dst_length)
            dst = dst_bytes.decode()
        else:
            return
        return dst
    else:
        return


async def get_port(reader_c):
    r = await reader_c.read(2)
    port = unpack('>H', r)[0]
    return port


async def get_dst_addr(reader_c, writer_c):
    r = await reader_c.read(3)
    if r == b'\x05\x01\x00':
        writer_c.write(b'\x05\x00')
    else:
        return

    dst = await get_dst(reader_c)
    port = await get_port(reader_c)
    # print(dst, port)
    return {'domain': dst, 'port': port}


async def reply(reader, writer, dst_name):
    while True:
        buf = await reader.read(2048*4)
        if not buf:
            break
        else:
            # print(buf[:200])
            writer.write(buf)
            await writer.drain()
    print('{} : close'.format(dst_name))


async def dig(hostname):
    dst_addrinfo = await loop.getaddrinfo(hostname, port)
    dst_ip = dst_addrinfo[0][4][0]
    return dst_ip


def valid_ip(address):
    try:
        ip_bytes = socket.inet_aton(address)
        return ip_bytes
    except:
        return False


async def create_pipe(reader_c, writer_c, reader_s, writer_s, dst):
    loop.create_task(reply(reader_c, writer_s, dst))
    await reply(reader_s, writer_c, dst)


async def reply_start(reader_c, writer_c, dst):
    reader_s, writer_s = await asyncio.open_connection(dst['domain'], port=dst['port'], loop=loop)
    print('direct connect to', dst['domain'], dst['port'])
    writer_c.write(b'\x05\x00\x00\x01\x00\x00\x00\x00\x10\x10')
    await writer_s.drain()
    await create_pipe(reader_c, writer_c, reader_s, writer_s, dst)


async def socks5_proxy(reader_c, writer_c, dst):
    dst_s, port_s = socks5_proxy_addr
    reader_s, writer_s = await asyncio.open_connection(dst_s, port_s, loop=loop)
    writer_s.write(b'\x05\x01\x00')
    await writer_s.drain()
    r = await reader_s.read(512)
    if r == b'\x05\x00':
        pass
    else:
        raise Exception("connect fail")

    ip_bytes = valid_ip(dst['domain'])
    port_bytes = pack('>H', dst['port'])
    if ip_bytes:
        writer_s.write(b'\x05\x01\x00\x01' + ip_bytes + port_bytes)
    else:
        dst_bytes = dst['domain'].encode()
        len_dst_bytes = pack('B', len(dst_bytes))
        writer_s.write(b'\x05\x01\x00\x03' + len_dst_bytes + dst_bytes
                       + port_bytes)
    await writer_s.drain()

    print('socks5 connect to', dst['domain'], dst['port'])
    await create_pipe(reader_c, writer_c, reader_s, writer_s, dst)


async def auto_switch(reader_c, writer_c, dst):
    dst_ip = await dig(dst['domain'])
    print('get ip')
    if ip_in_cn(dst_ip):
        print('over')
        await reply_start(reader_c, writer_c, dst)
    else:
        print('over')
        await socks5_proxy(reader_c, writer_c, dst)
        return


async def handle(reader_c, writer_c):
    dst_addr = await get_dst_addr(reader_c, writer_c)
    if dst_addr:
        print('handle')
        pass
    else:
        return

    await auto_switch(reader_c, writer_c, dst_addr)


if sys.argv[1:]:
    json_file = sys.argv[1]
    with open(json_file) as f:
        cfg = json.loads(f.read())

    ip, port_str = cfg['listen'].split(':')
    port = int(port_str)
    datfile = cfg['cn_ip_file']
    ip_in_cn = IPInCN.ip_in_cn_gen(datfile)
    socks5_proxy_addr = cfg['socks5_proxy_server'].split(':')

    loop = asyncio.get_event_loop()
    core = asyncio.start_server(handle, ip, port, loop=loop)
    server = loop.run_until_complete(core)
    loop.run_forever()

else:
    print('''{
    "listen":"127.0.0.1:1080",
    "cn_ip_file":"ip.txt",
    "socks5_proxy_server":"127.0.0.1:3389"
}''')
