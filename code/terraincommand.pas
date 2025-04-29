unit TerrainCommand;

interface

uses Classes, SysUtils, Collect,
     IdGlobal,
     BaseThread,
     CastleClientServer, CastleVectors, castleterrain,
     TerServerCommon,
     TerrainParams, TerrainData,
     watergrid, waterflow, watercolor,
     debug;

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

     TTask_SendWater = class( TClientTaskItem )
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

procedure waterArea( client : TClientConnection;
                     CenterX, CenterY : integer;
                     Radius : integer;
                     callback : TCommandCallback);


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

  function UpdateTile(       TileX, TileY : integer;
                         var ATile : TTerTile;
                             docreate : boolean = true ) : boolean;
  { returns true if creaated }
   var tileix : integer;
       tileinfo : TTileHeader;
   begin
     result := not GTileList.findtile( TileX, TileY, tileix );
     if result then
      begin { not found, create }
        if docreate then
         begin
           sethxy( tileinfo, TileX, TileY, GDefGridCellCount );
           ATile := TTerTile.create( tileinfo );
           GTileList.atinsert( tileix, ATile );
         end
        else
          result := false;
      end
     else
      begin
        ATile := TTerTile( GTileList.at( tileix ));
        result := not docreate;
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

   function neighborlayer( neighbor : TTerTile;
                           layer : integer ) : TSingleGrid;
    begin
      result := nil;
      if assigned( neighbor ) then case layer of
        layer_terrain : result := neighbor.TerrainGrid;
        layer_water  : result := neighbor.WaterGrid;
       end;

    end;

   function gridvaluexy( grid : TSingleGrid;
                         x,y : integer ) : single;
    begin
      result := -1;
      if assigned( grid ) then
         result := grid.valuexy(x,y);
    end;

function BuildResultBuffer( tile : ttertile;
                            const resultinfo : TTileHeader;
                            LOD : dword;
                            layer : integer =  layer_terrain) : TIdBytes;
 var tilesz, LODDiv : integer;
     buffer : TIdBytes;
     ThisGrid : TSingleGrid;
     bufptr : ^single;
     neighbor : TTerTile;
  procedure fullresolutionsample;
   var x, y : integer;
       h : single;
       ngrid : TSingleGrid;
   begin
     ngrid := neighborlayer( neighbor, layer );
     for x := 0 to tilesz - 1 do
      begin
        move( ThisGrid.ptrxy(x,0)^, bufptr^, tilesz * sizeof( single ));
        { last point in line }
        h := gridvaluexy(ngrid,x,0);
        inc( bufptr, tilesz );
        bufptr^ := h;
        inc( bufptr );
      end;
     { last row }
     neighbor := GTileList.getNeighbor( tile, 1, 0 );
     ngrid := neighborlayer( neighbor, layer );
     if assigned( ngrid ) then
        move( nGrid.ptrxy(0,0)^, bufptr^, tilesz * sizeof( single ));
     inc( bufptr, tilesz );
     neighbor := GTileList.getNeighbor( tile, 1, 1 );
     ngrid := neighborlayer( neighbor, layer );
     bufptr^ := gridvaluexy( nGrid, 0, 0 );
   end;
  procedure reducedresolutionsample;
   var x, y : integer;
       h : single;
       ngrid : tsinglegrid;
   begin
      ngrid := neighborlayer( neighbor, layer );
      for x := 0 to tilesz - 1 do
        begin
          for y := 0 to tilesz - 1 do
           begin
   //            bufptr^ := TerGrid.samplemax(x * loddiv, y * loddiv, loddiv, loddiv);
             bufptr^ := ThisGrid.valuexy(x * loddiv, y * loddiv);
             inc( bufptr );
           end;
          { last point in from neighbor grid }
          h := gridvaluexy( ngrid, x * loddiv,0);
          bufptr^ := h;
          inc( bufptr );
        end;
       { last line from neighbor grid }
       neighbor := GTileList.getNeighbor( tile, 1, 0 );
       ngrid := neighborlayer( neighbor, layer );
       if assigned( ngrid ) then
        begin
          for y := 0 to tilesz - 1 do
           begin
             bufptr^ := ngrid.valuexy(0, y * loddiv );
             inc( bufptr );
           end;
        end
       else
          inc( bufptr, tilesz );
       { corner point from neighbor grid }
       neighbor := GTileList.getNeighbor( tile, 1, 1 );
       ngrid := neighborlayer( neighbor, layer );
       bufptr^ := gridvaluexy( ngrid, 0, 0 );
   end;
 var buflen : integer;
 begin
   loddiv := divOfLOD( LOD );
    buflen := resultinfo.tilesz * resultinfo.tilesz * sizeof( single ) ;
    tilesz := Tile.info.tilesz div loddiv;
    case layer of
      layer_terrain : ThisGrid := Tile.TerrainGrid;
      layer_water  : ThisGrid := Tile.WaterGrid;
     end;
    setlength( buffer, buflen );
    bufptr := @buffer[0];
    neighbor := GTileList.getneighbor( tile, 0, 1 );
    if loddiv = 1 then { full resolution }
       fullresolutionsample
    else
       reducedresolutionsample;
   result := buffer;
 end;

function BuildResultGrid( tile : ttertile;
                          const resultinfo : TTileHeader;
                          LOD : dword;
                          layer : integer =  layer_terrain) : TSingleGrid;
 var tilesz, LODDiv : integer;
     bufptr : ^single;
     ThisGrid : TSingleGrid;
     neighbor : TTerTile;
  procedure fullresolutionsample;
   var x, y : integer;
       ngrid : TSingleGrid;
   begin
     ngrid := neighborlayer( neighbor, layer );
     for x := 0 to tilesz - 1 do
      begin
        move( ThisGrid.ptrxy(x,0)^, bufptr^, tilesz * sizeof( single ));
        { last point in line }
        inc( bufptr, tilesz );
        bufptr^ := gridvaluexy(ngrid, x,0);
        inc( bufptr );
      end;
     { last row }
     neighbor := GTileList.getNeighbor( tile, 1, 0 );
     ngrid := neighborlayer( neighbor, layer );
     if assigned( ngrid ) then
        move( nGrid.ptrxy(0,0)^, bufptr^, tilesz * sizeof( single ));
     inc( bufptr, tilesz );
     neighbor := GTileList.getNeighbor( tile, 1, 1 );
     ngrid := neighborlayer( neighbor, layer );
     bufptr^ := gridvaluexy( ngrid, 0, 0 );
   end;
  procedure reducedresolutionsample;
   var x, y : integer;
       ngrid : tsinglegrid;
   begin
      ngrid := neighborlayer( neighbor, layer );
      for x := 0 to tilesz - 1 do
        begin
          for y := 0 to tilesz - 1 do
           begin
   //            bufptr^ := TerGrid.samplemax(x * loddiv, y * loddiv, loddiv, loddiv);
             bufptr^ := ThisGrid.valuexy(x * loddiv, y * loddiv);
             inc( bufptr );
           end;
          { last point in from neighbor grid }
          bufptr^ := gridvaluexy( ngrid, x * loddiv,0);
          inc( bufptr );
        end;
       { last line from neighbor grid }
       neighbor := GTileList.getNeighbor( tile, 1, 0 );
       ngrid := neighborlayer( neighbor, layer );
       if assigned( ngrid ) then
        begin
          for y := 0 to tilesz - 1 do
           begin
             bufptr^ := ngrid.valuexy(0, y * loddiv );
             inc( bufptr );
           end;
        end
       else
          inc( bufptr, tilesz );
       { corner point from neighbor grid }
       neighbor := GTileList.getNeighbor( tile, 1, 1 );
       ngrid := neighborlayer( neighbor, layer );
       bufptr^ := gridvaluexy( ngrid, 0, 0 );
   end;
 begin
   loddiv := divOfLOD( LOD );
    tilesz := Tile.info.tilesz div loddiv;
    case layer of
      layer_terrain : ThisGrid := Tile.TerrainGrid;
      layer_water  : ThisGrid := Tile.WaterGrid;
     end;
    Result := TSingleGrid.Create( 0, resultinfo.tilesz );
    bufptr := Result.ptrix(0);
    neighbor := GTileList.getneighbor( tile, 0, 1 );
    if loddiv = 1 then { full resolution }
       fullresolutionsample
    else
       reducedresolutionsample;
 end;


//------------------------

constructor TTask_SendTile.create( const iClient : TClientConnection;
                                   iTile : TTerTile;
                                   iLOD : dword );
 begin
   inherited create( iClient );
   Tile := iTile;
   LOD := iLOD;
 end;

function TTask_SendTile.RunTask : boolean;
 var buffer : TIdBytes;
     resulttileinfo : TTileHeader;
     buflen : integer;
     tilesz, loddiv : integer;
 begin
   result := inherited Runtask;
   if not result then
      exit;
   loddiv := divOfLOD( LOD );
   resulttileinfo := Tile.info;

   tilesz := GDefGridCellCount div loddiv;
   resulttileinfo.tilesz := tilesz + 1;
   buffer := BuildResultBuffer( tile, resulttileinfo, LOD );
   buflen := length( buffer );

   { send message + tile headers }
   SendClientMsgHeader( client, msg_Tile, buflen + sizeof( TTileHeader ));
   client.Send( resulttileinfo, sizeof( TTileHeader ));
   client.SendBuffer( buffer, buflen );

   result := true;
 end;

//----------------------------

constructor TTask_SendWater.create( const iClient : TClientConnection;
                                    iTile : TTerTile;
                                    iLOD : dword );
 begin
   inherited create( iClient );
   Tile := iTile;
   LOD := iLOD;
 end;

const zmargin : single = 0.03;

function TTask_SendWater.RunTask : boolean;
 var buffer : TIdBytes;
     resulttileinfo : TTileHeader;
     buflen : integer;
     tilesz, loddiv : integer;
     resultwater, resultterrain, resultflora : TSingleGrid;
     resultwatertex : array of tvector2;
     terrainh, waterh, florah : ^single;
     texptr : ^TVector2;
     i : integer;
     h : single;
     bufpos : integer;
 begin
   result := inherited Runtask;
   if not result then
      exit;
   loddiv := divOfLOD( LOD );
   resulttileinfo := Tile.info;

   tilesz := GDefGridCellCount div loddiv;
   resulttileinfo.tilesz := tilesz + 1;
   resultwater := BuildResultGrid( tile, resulttileinfo, LOD, layer_water );
   resultterrain := BuildResultGrid( tile, resulttileinfo, LOD, layer_terrain );
   resultflora := BuildResultGrid( tile, resulttileinfo, LOD, layer_flora );

   waterh := resultwater.ptrix(0);
   terrainh := resultterrain.ptrix(0);
   florah := resultflora.ptrix(0);
   setlength( resultwatertex, resultwater.wxh );
   texptr := @resultwatertex[0];

   for i := 0 to resultwater.wxh - 1 do
    begin
      h := waterh^;
      { prevent z-fighting }
      if h < zmargin then
         h := -zmargin;
      texptr^ := CalcDepthAndTexture( h, florah^, terrainh^, false );
      waterh^ := terrainh^ + h;

      { calculate water texture index }

      inc( texptr );
      inc( waterh );
      inc( terrainh );
      inc( florah );
    end;
   resultterrain.free;
   resultflora.free;

   buflen := resultwater.wxh*sizeof(single) + resultwater.wxh * sizeof( tvector2 );
   setlength( buffer, buflen );
   bufpos := 0;
   Move( resultwater.depthptr^, buffer[bufpos], resultwater.wxh*sizeof(single));
   inc( bufpos, resultwater.wxh*sizeof(single));

   Move( resultwatertex[0], buffer[bufpos], resultwater.wh * sizeof( tvector2 ));

   { send message + tile headers }
   SendClientMsgHeader( client, msg_Water, buflen + sizeof( TTileHeader ));
   client.Send( resulttileinfo, sizeof( TTileHeader ));
   client.SendBuffer( buffer, buflen );

   resultwater.free;

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

   procedure stripleadingspaces( var astr : string ); inline;
    var spacecount : integer;
        l : integer;
    begin
      spacecount := 0;
      l := length( astr );
      while ( spacecount < l ) and ( astr[spacecount+1] = ' ' ) do
         inc( spacecount );
      delete( astr, 1, spacecount );
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
 procedure SendATile( tx, ty : integer );
  var Tile : TTerTile;
      LOD : dword;
  begin
    LOD := trunc(sqrt( sqr( tx - CenterX ) + sqr( ty - CenterY )));
    if LOD <= Radius then
     begin
       if UpdateTile( tx, ty, tile ) then { if the tile was created then we have to add a task to build it }
          GTaskList.AddTask( TTask_BuildTile.create( client, Tile, Params ) );
       GTaskList.AddTask( TTask_SendTile.create( client, Tile, LOD ) );
       Callback('');
     end;
  end;

 var TileY, TileY2 : Integer;
     i, r : integer;
 begin
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
 end;

procedure waterArea( client : TClientConnection;
                     CenterX, CenterY : integer;
                     Radius : integer;
                     callback : TCommandCallback);
 procedure SendATile( tx, ty : integer );
  var Tile : TTerTile;
      LOD : dword;
  begin
    LOD := trunc(sqrt( sqr( tx - CenterX ) + sqr( ty - CenterY )));
    if LOD <= Radius then
     begin
       if UpdateTile( tx, ty, tile, false ) then { if the tile was created then we have to add a task to build it }
        begin
          GTaskList.AddTask( TTask_SendWater.create( client, Tile, LOD ) );
          Callback('');
        end;
     end;
  end;

 var TileY, TileY2 : Integer;
     i, r : integer;
 begin
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
 end;



function parseTileXY( var params : string;
                       var TileX, TileY : integer ) : boolean;
 var param : string;
 begin
   result := nextparam( params, param );
   if result then
    begin
      TileX := intofstr( param );
      result := nextparam( params, param );
      if result then
         TileY := intofstr( param );
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
      if parsetilexy( params, tilex, tiley ) and nextparam( params, param ) then
         Radius := intofstr( param );
      buildArea( client, TileX, TileY, Radius, tileParams, callback );
    end;
 end;

function cmdWater( client : TClientConnection;
                   params : string;
                   callback : TCommandCallback) : integer;
 var TileX, TileY : integer;
     param : string;
     Radius   : dword;
 begin
   TileX := 0;
   TileY := 0;
   Radius := 0;
   if parsetilexy( params, tilex, tiley ) and nextparam( params, param ) then
      Radius := intofstr( param );
   WaterArea( client, TileX, TileY, Radius, callback );


 end;

initialization
  GCmdList := TCmdList.Create;
  GTaskList := TTaskList.Create;
  GCmdList.RegisterCmd( 'version', @cmdVersion );
  GCmdList.RegisterCmd( 'build', @cmdBuildTile );
  GCmdList.RegisterCmd( 'water', @cmdWater );

//  GCmdList.RegisterCmd( 'save', @cmdSave );
finalization
  GCmdList.Free;
  GTaskList.Free;
end.
