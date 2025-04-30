unit terservercommon;

interface

uses idglobal;

type tmsgtype = dword;
     ttilestatus = byte;

const msg_undefined = 0;
      msg_ping      = 1;
      msg_string    = 2;
      msg_binary    = 3;
      msg_tile      = 4;
      msg_water     = 5;

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
        TileX, TileY : smallint; { position of tile within world }
        TileSz       : word;     { dimensions of tile in 'cells' }
      end;

implementation

end.
