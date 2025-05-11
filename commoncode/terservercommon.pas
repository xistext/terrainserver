unit terservercommon;

{ These low level types and functions are used by both the
  Terrain Client and the Terrain Server }

interface

uses idglobal;

type tmsgtype = dword;
     ttilestatus = byte;


const { message types the terrain server sends for different responses }
      msg_undefined = 0;
      msg_ping      = 1;
      msg_string    = 2;
      msg_binary    = 3;
      msg_tile      = 4;
      msg_water     = 5;
      msg_splat     = 6;
      msg_flora     = 7;
      msg_LODUpdate = 8;  { contains terrain + water + flora }

      { tile status }
      tile_built    = 1;
      tile_locked   = 2;
      tile_dirty    = 4;

type TMsgHeader = record
        requestid : dword; { id of the request this message is a response to }
        msgtype   : tmsgtype; { type of the message }
        msglen    : dword; { length of the message data }
      end;

     PTileHeader = ^TTileHeader;
     TTileHeader = packed record
        TileX, TileY : smallint; { position index of tile within world }
        TileSz       : word;     { dimensions of tile in 'cells' }
      end;

{ encode splat data into an integer for the glsl }
function encodesplatcell( r, g, b, a : byte;
                          t1, t1a : byte ) : integer;
function decodesplatcell( v : integer ) : integer;

implementation

function encodesplatcell( r, g, b, a : byte;
                          t1, t1a : byte ) : integer;
 { encode 4 bit r,g,b,a and the texture + alpha into integer }
 begin
   assert(( r < 16 ) and ( g < 16 ) and ( b < 16 ) and ( a < 16 ));
   result := r + g shl 4 + b shl 8 + a shl 12 + t1 shl 16 + t1a shl 20;
 end;

function decodesplatcell( v : integer ) : integer;
 var r, g, b, a : integer;
 begin
   r := v shl 28 shr 28;
   g := v shl 24 shr 28;
   b := v shl 20 shr 28;
   a := v shl 16 shr 28;
   result := r + g * 16 + b * 256 + a * 4096;
 end;


end.
