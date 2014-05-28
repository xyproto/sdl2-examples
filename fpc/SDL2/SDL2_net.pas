{
  SDL_net:  An example cross-platform network library for use with SDL
  Copyright (C) 1997-2013 Sam Lantinga <slouken@libsdl.org>
  Copyright (C) 2012 Simeon Maxein <smaxein@googlemail.com>

=====

  SDL2_net header translation for Free Pascal
  https://bitbucket.org/p_daniel/sdl-2-for-free-pascal-compiler

=====

}
unit SDL2_net;

interface

{$IFDEF WITHOUT_SDL}
type
  PUint8=^Uint8;
  PUint16=^Uint16;
  PUint32=^Uint32;

  PSDLNet_version=^TSDLNet_version;
  TSDLNet_version=record
    major,
    minor,
    patch: Uint8;
  end;
{$ELSE WITHOUT_SDL}

uses SDL2;

{$IFDEF DARWIN}
  {$linkframework SDL2}
{$ENDIF}

type
  PSDLNet_version = ^TSDL_version;
{$ENDIF WITHOUT_SDL}

{$MACRO ON}
{$INLINE ON}
{$PACKRECORDS C}

{$DEFINE lSDL:=cdecl; external 'SDL2_net'}

{$IFDEF DARWIN}
  {$linkframework SDL2_net}
{$ENDIF}

const
  SDL_NET_MAJOR_VERSION=2;
  SDL_NET_MINOR_VERSION=0;
  SDL_NET_PATCHLEVEL   =0;

procedure SDL_NET_VERSION(x: PSDLNet_version); inline;
function SDLNet_Linked_Version: PSDLNet_version; lSDL;

function SDLNet_Init(flags: longint): longint; lSDL;
procedure SDLNet_Quit; lSDL;

type
  PIPaddress=^TIPaddress;
  TIPaddress=record
    host: Uint32;
    port: Uint16;
  end;

const
  INADDR_ANY      =$00000000;
  INADDR_NONE     =$FFFFFFFF;
  INADDR_LOOPBACK =$7f000001;
  INADDR_BROADCAST=$FFFFFFFF;

function SDLNet_ResolveHost(address: PIPaddress; const host: pchar;
                            port: Uint16): longint; lSDL;
function SDLNet_ResolveIP(const ip: PIPaddress): pchar; lSDL;
function SDLNet_GetLocalAddresses(addresses: PIPaddress;
                                  maxcount: longint): longint; lSDL;

type
  PTCPsocket=pointer;

function SDLNet_TCP_Open(ip: PIPaddress): PTCPsocket; lSDL;
function SDLNet_TCP_Accept(server: PTCPsocket): PTCPsocket; lSDL;
function SDLNet_TCP_GetPeerAddress(sock: PTCPsocket): PIPaddress; lSDL;
function SDLNet_TCP_Send(sock: PTCPsocket; const data: pointer;
                         len: longint): longint; lSDL;
function SDLNet_TCP_Recv(sock: PTCPsocket; data: pointer;
                         len: longint): longint; lSDL;
procedure SDLNet_TCP_Close(sock: PTCPsocket); lSDL;

const
  SDLNET_MAX_UDPCHANNELS=32;
  SDLNET_MAX_UDPADDRESSES=4;

type
  PUDPsocket=pointer;

  PPUDPpacket=^PUDPpacket;
  PUDPpacket=^TUDPpacket;
  TUDPpacket=record
    channel: longint;
    data: PUint8;
    len,
    maxlen,
    status: longint;
    address: TIPaddress;
  end;

function SDLNet_AllocPacket(size: longint): PUDPpacket; lSDL;
function SDLNet_ResizePacket(packet: PUDPpacket;
                             newsize: longint): longint; lSDL;
procedure SDLNet_FreePacket(packet: PUDPpacket); lSDL;

function SDLNet_AllocPacketV(howmany, size: longint): PPUDPpacket; lSDl;
procedure SDLNet_FreePacketV(packetV: PPUDPpacket); lSDl;

function SDLNet_UDP_Open(port: Uint16): PUDPsocket; lSDL;
procedure SDLNet_UDP_SetPacketLoss(sock: PUDPsocket; percent: longint); lSDL;
function SDLNet_UDP_Bind(sock: PUDPsocket; channel: longint;
                         const address: PIPaddress): longint; lSDL;
procedure SDLNet_UDP_Unbind(sock: PUDPsocket; channel: longint); lSDL;
function SDLNet_UDP_GetPeerAddress(sock: PUDPsocket;
                                   channel: longint): PIPaddress; lSDL;
function SDLNet_UDP_SendV(sock: PUDPsocket; packets: PPUDPpacket;
                          npackets: longint): longint; lSDL;
function SDLNet_UDP_Send(sock: PUDPsocket; channel: longint;
                         packet: PUDPpacket): longint; lSDL;
function SDLNet_UDP_RecvV(sock: PUDPsocket; packets: PPUDPpacket): longint; lSDL;
function SDLNet_UDP_Recv(sock: PUDPsocket; packet: PUDPpacket): longint; lSDL;
procedure SDLNet_UDP_Close(sock: PUDPsocket); lSDL;

type
  PSDLNet_SocketSet=pointer;

  PSDLNet_GenericSocket=^TSDLNet_GenericSocket;
  TSDLNet_GenericSocket=record
    ready: longint;
  end;

function SDLNet_AllocSocketSet(maxsockets: longint): PSDLNet_SocketSet; lSDL;
function SDLNet_AddSocket(set_: PSDLNet_SocketSet;
                          sock: PSDLNet_GenericSocket): longint; lSDL;
function SDLNet_TCP_AddSocket(set_: PSDLNet_SocketSet;
                              sock: PTCPsocket): longint; inline;
function SDLNet_UDP_AddSocket(set_: PSDLNet_SocketSet;
                              sock: PUDPsocket): longint; inline;

function SDLNet_DelSocket(set_: PSDLNet_SocketSet;
                          sock: PSDLNet_GenericSocket): longint; lSDL;
function SDLNet_TCP_DelSocket(set_: PSDLNet_SocketSet;
                              sock: PTCPsocket): longint; inline;
function SDLNet_UDP_DelSocket(set_: PSDLNet_SocketSet;
                              sock: PUDPsocket): longint; inline;

function SDLNet_CheckSockets(set_: PSDLNet_SocketSet;
                             timeout: Uint32): longint; lSDL;

function SDLNet_SocketReady(sock: PSDLNet_GenericSocket): boolean; inline;

procedure SDLNet_FreeSocketSet(set_: PSDLNet_SocketSet); lSDl;

procedure SDLNet_SetError(const fmt: pchar); lSDL; varargs;
function SDLNet_GetError: pchar; lSDL;

{$IF DEFINED(CPUSPARC) OR DEFINED(CPUARM)}
  {$DEFINE SDL_DATA_ALIGNED}
{$ENDIF}

procedure SDLNet_Write16(value: Uint16; areap: pointer); inline;
procedure SDLNet_Write32(value: Uint32; areap: pointer); inline;
function SDLNet_Read16(const areap: pointer): Uint16; inline;
function SDLNet_Read32(const areap: pointer): Uint32; inline;

implementation

procedure SDL_NET_VERSION(x: PSDLNet_version); inline;
begin
  x^.major:=SDL_NET_MAJOR_VERSION;
  x^.minor:=SDL_NET_MINOR_VERSION;
  x^.patch:=SDL_NET_PATCHLEVEL;
end;

function SDLNet_TCP_AddSocket(set_: PSDLNet_SocketSet;
                              sock: PTCPsocket): longint; inline;
begin
  SDLNet_TCP_AddSocket:=SDLNet_TCP_AddSocket(set_, PSDLNet_GenericSocket(sock));
end;

function SDLNet_UDP_AddSocket(set_: PSDLNet_SocketSet;
                              sock: PUDPsocket): longint; inline;
begin
  SDLNet_UDP_AddSocket:=SDLNet_TCP_AddSocket(set_, PSDLNet_GenericSocket(sock));
end;

function SDLNet_TCP_DelSocket(set_: PSDLNet_SocketSet;
                              sock: PTCPsocket): longint; inline;
begin
  SDLNet_TCP_DelSocket:=SDLNet_TCP_AddSocket(set_, PSDLNet_GenericSocket(sock));
end;

function SDLNet_UDP_DelSocket(set_: PSDLNet_SocketSet;
                              sock: PUDPsocket): longint; inline;
begin
  SDLNet_UDP_DelSocket:=SDLNet_TCP_AddSocket(set_, PSDLNet_GenericSocket(sock));
end;

function SDLNet_SocketReady(sock: PSDLNet_GenericSocket): boolean; inline;
begin
  SDLNet_SocketReady:=(sock<>NIL) and (sock^.ready>1);
end;

{$IF NOT DEFINED(WITHOUT_SDL) AND NOT DEFINED(SDL_DATA_ALIGNED)}

procedure SDLNet_Write16(value: Uint16; areap: pointer); inline;
begin
  PUint16(areap)[0]:=BEtoN(value);
end;

procedure SDLNet_Write32(value: Uint32; areap: pointer); inline;
begin
  PUint32(areap)[0]:=BEtoN(value);
end;

function SDLNet_Read16(const areap: pointer): Uint16; inline;
begin
  SDLNet_Read16:=BEtoN(PUint16(areap)[0]);
end;

function SDLNet_Read32(const areap: pointer): Uint32; inline;
begin
  SDLNet_Read32:=BEtoN(PUint32(areap)[0]);
end;

{$ELSE}

procedure SDLNet_Write16(value: Uint16; areap: pointer); inline;
var
  area: PUint8;
begin
  area:=areap;
  area[0]:=(value shr 8) and $FF;
  area[1]:=value         and $FF;
end;

procedure SDLNet_Write32(value: Uint32; areap: pointer); inline;
var
  area: PUint8;
begin
  area:=areap;
  area[0]:=(value shr 24) and $FF;
  area[1]:=(value shr 16) and $FF;
  area[2]:=(value shr  8) and $FF;
  area[3]:=value          and $FF;
end;

function SDLNet_Read16(const areap: pointer): Uint16; inline;
begin
  SDLNet_Read16:=PUint16(areap)[0] shl 8 or PUint16(areap)[1];
end;

function SDLNet_Read32(const areap: pointer): Uint32; inline;
begin
  SDLNet_Read32:=PUint32(areap)[0] shl 24 or
                 PUint32(areap)[1] shl 16 or
                 PUint32(areap)[2] shl 8 or
                 PUint32(areap)[3];
end;

{$ENDIF}

end.
