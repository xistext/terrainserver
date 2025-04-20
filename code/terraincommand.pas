unit TerrainCommand;

interface

uses Classes, SysUtils, Collect, castleterrain,
     IdGlobal,
     BaseThread,
     TerServerCommon, CastleClientServer,
     TerrainParams, TerrainData,
     watergrid, debug;

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

    TClientTaskItem = class( TTaskItem )

       Client : TClientConnection;

       constructor create( const iClient : TClientConnection );
       function runtask : boolean; override;

     end;

    TTaskList = class
       items : array of TTaskItem;
       function count: integer;
       function pop : TTaskItem;
       function item( i : integer ) : TTaskItem;
       procedure remove( ix, len : integer );
       procedure removeclient( const AClient : TClientConnection );
       procedure AddTask( ATask : TTaskItem );
       procedure AddTasks( iTaskList : TTaskList );
     end;

    TTask_SendTile = class( TClientTaskItem )
        Tile   : TTerTile;
        LOD    : dword;
        constructor create( const iClient : TClientConnection;
                            iTile : TTerTile;
                            iLOD : dword );
        function runtask : boolean; override;
      end;

    TTask_BuildTile = class( TClientTaskItem )
            Tile   : TTerTile;
            tileparams : TTerrainParams;
            constructor create( const iClient : TClientConnection;
                                iTile : TTerTile;
                                iParams : tTerrainparams );
            function runtask : boolean; override;
          end;

    TTask_SaveTiles = class( TTaskItem )
        function runtask : boolean; override;
     end;

procedure SendClientMsgHeader( AClient : TClientConnection;
                               msgtype : TMsgType;
                               msglen  : dword = 0;
                               requestid : dword = 0 );


const GCmdList : TCmdList = nil;
      GTaskList : TTaskList = nil;

implementation

const version : string = 'xisterra0.1a';

  function divofLOD( LOD : dword ) : dword;
   begin
     result := 60;
     case LOD of
        0, 1 : result := 1;
        2 : result := 2;
        3 : result := 3;
        4 : result := 4;
        5 : result := 5;
        6 : result := 6;
        7 : result := 8;
        8 : result := 10;
        9 : result := 12;
        10 : result := 15;
        11 : result := 20;
        12 : result := 24;
        13 : result := 30;
        14 : result := 40;
      end;
   end;

  function UpdateTile( const tileparams : TTerrainParams;
                             TileX, TileY : integer;
                       var ATile : TTerTile ) : boolean;
  { returns true if creaated }
   var tileix : integer;
       tileinfo : TTileHeader;
   begin
     result := not GTileList.findtile( TileX, TileY, tileix );
     if result then
      begin { not found, create }
        sethxy( tileinfo, TileX, TileY, GDefGridCellCount );
        ATile := TTerTile.create( tileinfo );
        GTileList.atinsert( tileix, ATile );
      end
     else
        ATile := TTerTile( GTileList.at( tileix ))
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

//----------------------------

function TTaskList.count: integer;
 begin
   result := length( items );
 end;

function TTaskList.pop : TTaskItem;
 begin
   result := nil;
   if count > 0 then
    begin
      result := items[0];
      delete( items, 0, 1 );
    end;
 end;

function TTaskList.item( i : integer ) : TTaskItem;
 begin
   result := items[i];
 end;

procedure TTaskList.remove( ix, len : integer );
 begin
   delete( items, ix, len );
 end;

procedure TTaskList.AddTask( ATask : TTaskItem );
 var len : integer;
 begin
   len := count;
   setlength( items, len + 1 );
   items[len] := ATask;
 end;

procedure TTaskList.AddTasks( iTaskList : TTaskList );
 var newlist : TTaskList;
     item1, item2 : TTaskItem;
 begin
   newlist := ttasklist.create;
   repeat
      item1 := pop;
      if assigned( item1 ) then
         newlist.AddTask( item1 );
      item2 := itasklist.pop;
      if assigned( item2 ) then
         newlist.AddTask( item2 );
   until not assigned( item1 ) and not assigned( item2 );
   items := newlist.items;
   newlist.free;
 end;

procedure TTaskList.removeclient( const AClient : TClientConnection );
 var i, l : integer;
     it : TTaskItem;
 begin
   l := count;
   i := 0;
   while i < l do
    begin
      it := items[i];
      if ( it is TClientTaskItem ) and ( TClientTaskItem( it ).Client.Context = AClient.Context ) then
       begin
         remove( i, 1 );
         it.free;
         dec( l );
       end
      else
         inc( i );
    end;
 end;

//----------------------------

function TTaskItem.runtask : boolean;
 begin
   result := true;
 end;


//-----------------------------------
constructor TClientTaskItem.create( const iClient : TClientConnection );
 begin
   inherited create;
   Client := iClient;
 end;

function TClientTaskItem.runtask : boolean;
 begin
   result := assigned( Client.Context.Connection );
 end;

//-----------------------------------

constructor TTask_BuildTile.create( const iClient : TClientConnection;
                                    iTile : TTerTile;
                                    iParams : tterrainparams );

 begin
   inherited create( iClient );
   Tile   := iTile;
   tileparams := iParams;
 end;

function TTask_BuildTile.runtask : boolean;
 begin
   result := inherited runtask;
   if result then
    begin
      assert( assigned( tile ) and assigned( tileparams ));

      if not tile.loadfromfile then
         Tile.UpdateTerrainGridFromSource( TileParams.Noise );
    end;
 end;

//-------------------------------------

constructor TTask_SendTile.create( const iClient : TClientConnection;
                                   iTile : TTerTile;
                                   iLOD : dword );
 begin
   inherited create( iClient );
   Tile := iTile;
   LOD := iLOD;
 end;

function TTask_SendTile.RunTask : boolean;
 var buflen, x, y, tilesz, LODDiv : integer;
     buffer : TIdBytes;
     TerGrid : TSingleGrid;
     bufptr : ^single;
     resulttileinfo : TTileHeader;
     h : single;
     neighbor : TTerTile;
 begin
   result := inherited Runtask;
   if not result then
      exit;
   loddiv := divOfLOD( LOD );
   resulttileinfo := Tile.info;

   tilesz := GDefGridCellCount div loddiv;
   resulttileinfo.tilesz := tilesz + 1;
   buflen := resulttileinfo.tilesz * resulttileinfo.tilesz * sizeof( single ) ;

   TerGrid := Tile.TerrainGrid;
   setlength( buffer, buflen );
   bufptr := @buffer[0];
   neighbor := GTileList.getneighbor( tile, 0, 1 );
   h := 0;
   if loddiv = 1 then { full resolution }
    begin
      for x := 0 to tilesz - 1 do
       begin
         move( TerGrid.ptrxy(x,0)^, bufptr^, tilesz * sizeof( single ));
         { last point in line }
         if assigned( neighbor ) then
            h := neighbor.TerrainGrid.valuexy(x,0)
         else
            h := 0;
         inc( bufptr, tilesz );
         bufptr^ := h;
         inc( bufptr );
       end;
      { last row }
      neighbor := GTileList.getNeighbor( tile, 1, 0 );
      if assigned( neighbor ) then
         move( neighbor.TerrainGrid.ptrxy(0,0)^, bufptr^, tilesz * sizeof( single ));
      inc( bufptr, tilesz );
      neighbor := GTileList.getNeighbor( tile, 1, 1 );
      if assigned( neighbor ) then
         bufptr^ := neighbor.TerrainGrid.valuexy( 0, 0 );
    end
   else
    begin { reduced sample }
      for x := 0 to tilesz - 1 do
       begin
         for y := 0 to tilesz - 1 do
          begin
//            bufptr^ := TerGrid.samplemax(x * loddiv, y * loddiv, loddiv, loddiv);
            bufptr^ := TerGrid.valuexy(x * loddiv, y * loddiv);
            inc( bufptr );
          end;
         { last point in from neighbor grid }
         if assigned( neighbor ) then
            h := neighbor.TerrainGrid.valuexy(x * loddiv,0)
         else
            h := 0;
         bufptr^ := h;
         inc( bufptr );
       end;
      { last line from neighbor grid }
      neighbor := GTileList.getNeighbor( tile, 1, 0 );
      if assigned( neighbor ) then
       begin
         for y := 0 to tilesz - 1 do
          begin
            bufptr^ := neighbor.TerrainGrid.valuexy(0, y * loddiv );
            inc( bufptr );
          end;
       end
      else
         inc( bufptr, tilesz );
      { corner point from neighbor grid }
      neighbor := GTileList.getNeighbor( tile, 1, 1 );
      if assigned( neighbor ) then
         bufptr^ := neighbor.TerrainGrid.valuexy( 0, 0 );
    end;

   { send message + tile headers }
   SendClientMsgHeader( client, msg_Tile, buflen + sizeof( TTileHeader ));
   client.Send( resulttileinfo, sizeof( TTileHeader ));
   client.SendBuffer( buffer, buflen );

   dbgwrite( 'Send '+tile.tileid+'.   ' );

   result := true;
 end;

//---------------------------------

function TTask_SaveTiles.runtask : boolean;
 var i : integer;
 begin
   result := inherited runtask;
   if result then
      for i := 0 to GTilelist.count - 1 do
       begin
         ttertile( GTilelist.at(i)).SaveToFile;
       end;
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


procedure buildArea( client : TClientConnection;
                     CenterX, CenterY : integer;
                     Radius : integer;
                     const Params : TTerrainParams;
                     callback : TCommandCallback);
 var atasklist : TTasklist;

 procedure SendATile( tx, ty : integer );
  var Tile : TTerTile;
      LOD : dword;
  begin
    LOD := trunc(sqrt( sqr( tx - CenterX ) + sqr( ty - CenterY )));
    if LOD <= Radius then
     begin
       if UpdateTile( Params, tx, ty, tile ) then { if the tile was created then we have to add a task to build it }
          GTaskList.AddTask( TTask_BuildTile.create( client, Tile, Params ) );
       GTaskList.AddTask( TTask_SendTile.create( client, Tile, LOD ) );
       Callback('');
     end;
  end;

 var TileY, TileY2 : Integer;
     i, r : integer;
 begin
   atasklist := ttasklist.create;
   SendATile( CenterX, CenterY );
   for r := 1 to radius do
    begin
      TileY := CenterY - r;
      TileY2 := CenterY + r;
      { do long end top and bottom }
      SendATile( CenterX, TileY );
      SendATile( CenterX, CenterY + r );
      { do sides left, right }
      SendATile( CenterX - r, CenterY );
      SendATile( CenterX + r, CenterY );
      for i := 1 to r - 1 do
        begin
          SendATile( CenterX - i, TileY );
          SendATile( CenterX + i, TileY );
          SendATile( CenterX - i, TileY2 );
          SendATile( CenterX + i, TileY2 );
          SendATile( CenterX - r, CenterY - i );
          SendATile( CenterX - r, CenterY + i );
          SendATile( CenterX + r, CenterY - i );
          SendATile( CenterX + r, CenterY + i );
        end;
      SendATile( CenterX - r, CenterY - r );
      SendATile( CenterX - r, CenterY + r );
      SendATile( CenterX + r, CenterY - r );
      SendATile( CenterX + r, CenterY + r );
    end;
   GTaskList.AddTask( TTask_SaveTiles.create );
{   gtasklist.AddTasks(aTaskList);
   aTaskList.Free;}
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
  GTaskList := TTaskList.Create;
  GCmdList.RegisterCmd( 'version', @cmdVersion );
  GCmdList.RegisterCmd( 'build', @cmdBuildTile );
//  GCmdList.RegisterCmd( 'save', @cmdSave );
finalization
  GCmdList.Free;
  GTaskList.Free;
end.
