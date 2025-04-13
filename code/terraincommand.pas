unit TerrainCommand;

interface

uses Classes, SysUtils, Collect, castleterrain,
     IdGlobal,
     BaseThread,
     TerServerCommon, CastleClientServer,
     TerrainParams, TerrainData,
     watergrid;

type TCommandCallback = procedure( msg : string ) of object;

     TCommandFunc = function( client : TClientConnection;
                              params : string;
                              callback : TCommandCallback) : integer;

     TSrvCommand = class
        Command : string;
        AFunc   : TCommandFunc;
        constructor create( iCommand : string; cmdfunc : TCommandFunc );
      end;

     TCmdList = class( TSortedCollection )
        function keyof( item : pointer ) : pointer; override;
        function compare( item1, item2 : pointer ) : integer; override;
        function registercmd( cmd : string; cmdfunc : TCommandFunc ) : boolean;
        function executecommand( client : TClientConnection;
                                 cmd : string;
                                 callback : TCommandCallback ) : integer;
      end;



    TTaskItem = class

       function runtask : boolean; virtual;

     end;

    TTask_SendTile = class( TTaskItem )
        Client : TClientConnection;
        Tile   : TTerTile;
        LOD    : dword;
        constructor create( const iClient : TClientConnection;
                            iTile : TTerTile;
                            iLOD : dword );
        function runtask : boolean; override;
      end;

    TTask_BuildTile = class( TTaskItem )
            Tile   : TTerTile;
            tileparams : TTerrainParams;
            constructor create( iTile : TTerTile;
                                iParams : tTerrainparams );
            function runtask : boolean; override;
          end;

procedure SendClientMsgHeader( AClient : TClientConnection;
                               msgtype : TMsgType;
                               msglen  : dword = 0;
                               requestid : dword = 0 );

const GCmdList : TCmdList = nil;
var GTaskList : array of TTaskItem;

implementation

const version : string = 'xisterra0.1a';

  function divofLOD( LOD : dword ) : dword;
   begin
     result := 60;
     case LOD of
        0, 1, 2 : result := 1;
        3 : result := 2;
        4 : result := 3;
        5 : result := 4;
        6 : result := 5;
        7 : result := 6;
        8 : result := 8;
        9 : result := 10;
        10 : result := 12;
        11 : result := 15;
        12 : result := 20;
        13 : result := 24;
        14 : result := 30;
        15 : result := 40;
      end;
   end;

  function UpdateTile( const tileparams : TTerrainParams;
                             TileX, TileY : integer ) : TTerTile;
   var tileix : integer;
       tileinfo : TTileHeader;
   begin
     if GTileList.findtile( TileX, TileY, tileix ) then
        result := TTerTile( GTileList.at( tileix ))
     else
      begin { not found, create }
        sethxy( tileinfo, TileX, TileY, GDefGridCellCount );
        result := TTerTile.create( tileinfo );
        Result.UpdateTerrainGridFromSource( TileParams.Noise );
        GTileList.atinsert( tileix, result );
      end;
   end;

procedure SendClientMsgHeader( AClient : TClientConnection;
                               msgtype : TMsgType;
                               msglen  : dword = 0;
                               requestid : dword = 0 );
 var h : TMsgHeader;
 begin
   h.requestid := requestid;
   h.msgtype := msgtype;
   h.msglen := msglen;
   AClient.Send( h, sizeof( h ));
 end;

procedure AddTask( ATask : TTAskItem );
 var len : integer;
 begin
   len := length( GTaskList );
   setlength( GTaskList, len + 1 );
   GTaskList[len] := ATask;
 end;

function TTaskItem.runtask : boolean;
 begin
   result := false;
 end;

//-----------------------------------

constructor TTask_BuildTile.create( iTile : TTerTile;
                                    iParams : tterrainparams );

 begin
   inherited create;
   Tile   := iTile;
   tileparams := iParams;
 end;

function TTask_BuildTile.runtask : boolean;
 begin
   result := true;
   assert( assigned( tile ) and assigned( tileparams ));
   Tile.UpdateTerrainGridFromSource( TileParams.Noise );
 end;

//-------------------------------------

constructor TTask_SendTile.create( const iClient : TClientConnection;
                                   iTile : TTerTile;
                                   iLOD : dword );
 begin
   inherited create;
   Client := iClient;
   Tile := iTile;
   LOD := iLOD;
 end;

function TTask_SendTile.RunTask : boolean;
 var buflen, x, y, tilesz, LODDiv : integer;
     buffer : TIdBytes;
     TerGrid : TSingleGrid;
     bufptr : ^single;
 begin
   loddiv := divOfLOD( LOD );
   tilesz := GDefGridCellCount div loddiv;
   Tile.info.tilesz := tilesz;
   buflen := tilesz * tilesz * sizeof( single ) ;

   TerGrid := Tile.TerrainGrid;
   setlength( buffer, buflen );
   if loddiv = 1 then { full resolution }
      move( TerGrid.depthptr^, buffer[0], buflen )
   else
    begin { reduced sample }
      bufptr := @buffer[0];
      for x := 0 to tilesz - 1 do for y := 0 to tilesz - 1 do
       begin
         bufptr^ := TerGrid.samplemax(x * loddiv, y * loddiv, loddiv, loddiv);
         inc( bufptr );
       end;
    end;

   { send message + tile headers }
   SendClientMsgHeader( client, msg_Tile, buflen + sizeof( TTileHeader ));
   client.Send( Tile.Info, sizeof( TTileHeader ));
   client.SendBuffer( buffer, buflen );
   result := true;
 end;

//-----------------------------

constructor TSrvCommand.create( iCommand : string; cmdfunc : TCommandFunc );
 begin
   Command := iCommand;
   AFunc := cmdFunc;
 end;

function TCmdList.keyof( item : pointer ) : pointer;
 begin
   result := pointer( TSrvCommand( item ).Command );
 end;

function TCmdList.compare( item1, item2 : pointer ) : integer;
 begin
   result := comparetext( String( Item1 ), String( item2 ));
 end;

function TCmdList.registercmd( cmd : string; cmdfunc : TCommandFunc ) : boolean;
 var i : integer;
 begin
   if search( pointer( cmd ), i ) then
      result := false
   else
     atinsert( i, TSrvCommand.create( cmd, cmdfunc ));
 end;

function TCmdList.executecommand( client : TClientConnection;
                                  cmd : string;
                                 callback : TCommandCallback ) : integer;
 var i, l : integer;
     params : string;
 begin
   result := 0;
   params := '';
   i := pos( ' ', cmd );
   if i > 0 then
    begin
      l := length( cmd );
      params := copy( cmd, i + 1, l - i );
      cmd := copy( cmd, 1, i - 1 );
    end;
   if search( pointer( cmd ), i ) then
      result := TSrvCommand( at( i )).AFunc( client, params, callback );
 end;

function nextparam( var params : string;
                    var param : string ) : boolean;
 var i : integer;
 begin
   param := params;
   i := pos( ',', params );
   result := i > 0;
   if result then
    begin
      param := copy( params, 1, i - 1 );
      delete( params, 1, i );
      while params[1] = ' ' do
         delete( params, 1, 1 );

    end
   else
    begin
      param := params;
      params := '';
      result := param <> '';
    end;

 end;

{ commands }

function cmdVersion( client : TClientConnection;
                     params : string;
                     callback : TCommandCallback ) : integer;
 begin
   SendClientMsgHeader( client, msg_string, 0 );
   client.SendString( version );
   Result := 1;
 end;

function intofstr( astr : string ) : integer;
 var code : integer;
 begin
   val( AStr, result, code );
 end;

procedure SendTile( client : TClientConnection;
                    Tile  : TTerTile;
                    LOD : dword = 0 );
 var task : TTask_SendTile;
 begin
   task := TTask_SendTile.create( client, Tile, LOD );
   AddTask( task );
 end;

procedure buildArea( client : TClientConnection;
                     CenterX, CenterY : integer;
                     Radius : integer;
                     const Params : TTerrainParams;
                     callback : TCommandCallback);
 procedure SendATile( tx, ty : integer; LOD : dword );
  var Tile : TTerTile;
  begin
    Tile := UpdateTile( Params, tx, ty );
    Sendtile( Client, Tile, LOD );
    Callback( 'Send Tile['+inttostr( tx )+','+inttostr(ty)+','+inttostr(LOD)+']' );
  end;

 var TileY, TileY2 : Integer;
     i, r : integer;
     LOD : dword;
 begin
   LOD := 0;
   SendATile( CenterX, CenterY, LOD );
   for r := 1 to radius do
    begin
      TileY := CenterY - r;
      TileY2 := CenterY + r;
      { do long end top and bottom }
      SendATile( CenterX, TileY, LOD );
      SendATile( CenterX, CenterY + r, LOD );
      { do sides left, right }
      SendATile( CenterX - r, CenterY, LOD );
      SendATile( CenterX + r, CenterY, LOD );
      for i := 1 to r - 1 do
        begin
          SendATile( CenterX - i, TileY, LOD );
          SendATile( CenterX + i, TileY, LOD );
          SendATile( CenterX - i, TileY2, LOD );
          SendATile( CenterX + i, TileY2, LOD );
          SendATile( CenterX - r, CenterY - i, LOD );
          SendATile( CenterX - r, CenterY + i, LOD );
          SendATile( CenterX + r, CenterY - i, LOD );
          SendATile( CenterX + r, CenterY + i, LOD );
        end;
      SendATile( CenterX - r, CenterY - r, LOD );
      SendATile( CenterX - r, CenterY + r, LOD );
      SendATile( CenterX + r, CenterY - r, LOD );
      SendATile( CenterX + r, CenterY + r, LOD );
      inc(LOD);
    end;
 end;


function cmdBuildTile( client : TClientConnection;
                       params : string;
                       callback : TCommandCallback) : integer;
 var tileparams : TTerrainParams;
     TileX, TileY : integer;
     param : string;
     Radius   : dword;
 begin
   result := 0;
   if TerrainTypes.findTerrainType( 'mountain', TileParams ) then
    begin
      TileX := 0;
      TileY := 0;
      Radius := 0;
      if nextparam( params, param ) then
       begin
         TileX := intofstr( param );
         if nextparam( params, param ) then
          begin
            TileY := intofstr( param );
            if nextparam( params, param ) then
               Radius := intofstr( param );
          end
         else
          begin
            Radius := TileX;
            TileX := 0;
            tileY := 0;
          end;
       end;
      buildArea( client, TileX, TileY, Radius, tileParams, callback );
    end;
 end;

initialization
  GCmdList := TCmdList.Create;
  GCmdList.RegisterCmd( 'version', @cmdVersion );
  GCmdList.RegisterCmd( 'build', @cmdBuildTile );
finalization
  GCmdList.Free;
end.
