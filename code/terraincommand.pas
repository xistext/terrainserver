unit TerrainCommand;

interface

uses Classes, SysUtils, Collect,
     IdGlobal, Math,
     BaseThread, StrTools,
     CastleClientServer, CastleVectors, castleterrain,
     TerServerCommon,
     ClientList,
     TerrainParams, TerrainData, TerrainObjects,
     watergrid, waterflow, waterparams, watercolor;

type TCommandCallback = procedure( msg : string ) of object;

     TCommandFunc = function( client : TTileClient;
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
        function executecommand( client : TTileClient;
                                 cmd : string;
                                 callback : TCommandCallback ) : integer;
      end;

    TTaskItem = class
      function runtask : boolean; virtual;
    end;

    TClientTaskItem = class( TTaskItem )

       Client : TTileClient;

       constructor create( const iClient : TTileClient );
       function runtask : boolean; override;

     end;

    TTaskList = class
       items : array of TTaskItem;
       function count: integer;
       function pop : TTaskItem;
       function item( i : integer ) : TTaskItem;
       procedure remove( ix, len : integer );
       procedure removeclient( const AClient : TTileClient );
       procedure AddTask( ATask : TTaskItem );
       procedure AddTasks( iTaskList : TTaskList );
     end;

    { tasks triggered by commands }

    { parent class for tasks that work with tiles, doesn't do anything }
    TTask_Tile = class( TClientTaskItem )
       Tile   : TTerTile;
       LOD    : dword;
       constructor create( const iClient : TTileClient;
                           iTile : TTerTile;
                           iLOD : dword );
     end;

    TTask_SendTile = class( TTask_Tile )
        function runtask : boolean; override;
      end;

     TTask_SendWater = class( TTask_Tile )
        function runtask : boolean; override;
      end;

     TTask_SendFlora = class( TTask_Tile )
        function runtask : boolean; override;
      end;

     TTask_SendSplat = class( TTask_Tile )
        function runtask : boolean; override;
      end;

     TTask_SendLODUpdateTile = class( TTask_Tile )
        function runtask : boolean; override;
      end;

     TTask_SendTrees = class( TTask_Tile )
        function runtask : boolean; override;
      end;

    TTask_BuildTile = class( TClientTaskItem )
            Tile   : TTerTile;
            tileparams : TTerrainParams;
            constructor create( const iClient : TTileClient;
                                iTile : TTerTile;
                                iParams : tTerrainparams );
            function runtask : boolean; override;
          end;

    TTask_SaveTiles = class( TTaskItem )
        function runtask : boolean; override;
     end;


const GCmdList : TCmdList = nil;
      GTaskList : TTaskList = nil;

procedure buildwaterArea( client : TTileClient;
                          CenterX, CenterY : integer;
                          Radius : integer;
                          LOD : integer;
                          callback : TCommandCallback);


implementation

const version : string = 'xisterra0.1a';

type Titerateareaproc = procedure( thisX, thisY : integer;
                                   data : pointer );

procedure iteratearea( X, Y, Radius : integer;
                       data : pointer;
                       proc : TIterateAreaProc );
 var Y1, Y2, i, r : integer;
 begin
   proc( X, Y, data );
   for r := 1 to radius do
    begin
      Y1 := Y - r;
      Y2 := Y + r;
      { do long end top and bottom }
      proc( X, Y1, data );
      proc( X, Y2, data );
      { do sides left, right }
      proc( X - r, Y, data );
      proc( X + r, Y, data );
      for i := 1 to r - 1 do
        begin
          proc( X - i, Y1, data );
          proc( X + i, Y1, data );
          proc( X - i, Y2, data );
          proc( X + i, Y2, data );
          proc( X - r, Y - i, data );
          proc( X - r, Y + i, data );
          proc( X + r, Y - i, data );
          proc( X + r, Y + i, data );
        end;
      proc( X - r, Y - r, data );
      proc( X - r, Y + r, data );
      proc( X + r, Y - r, data );
      proc( X + r, Y + r, data );
    end;
 end;

type TIterateRec = record
                     CenterX, CenterY, Radius, LOD : integer;
                     Client : TTileClient;
                     Params : TTerrainParams;
                     callback : TCommandCallback;
                    end;

    function initIteraterec( iCenterX, iCenterY, iRadius, iLOD : integer;
                             iClient : TTileClient;
                             iParams : TTerrainParams;
                             icallback : TCommandCallback ) : TIterateRec;
     begin
       with result do
        begin
          CenterX := iCenterX;
          CenterY := iCenterY;
          Radius := iRadius;
          LOD := iLOD;
          Params := iParams;
          callback := icallback;
          Client := iclient;
        end
     end;

  function UpdateTile(     TileX, TileY : integer;
                       var ATile : TTerTile;
                           docreate : boolean = true ) : boolean;
  { returns true if created }
   var tileix : integer;
       tileinfo : TTileHeader;
   begin
     GTileList.Lock;
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
     GTileList.Unlock;
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

procedure TTaskList.removeclient( const AClient : TTileClient );
 var i, l : integer;
     it : TTaskItem;
 begin
   l := count;
   i := 0;
   while i < l do
    begin
      it := items[i];
      if ( it is TClientTaskItem ) and TClientTaskItem( it ).Client.equals( AClient ) then
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
constructor TClientTaskItem.create( const iClient : TTileClient );
 begin
   inherited create;
   Client := iClient;
 end;

function TClientTaskItem.runtask : boolean;
 begin
   result := Client.Connected;
 end;

//-----------------------------------

constructor TTask_BuildTile.create( const iClient : TTileClient;
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
        layer_flora : result := neighbor.FloraGrid;
        layer_splat : result := neighbor.SplatGrid;
       end;
    end;

   function gridvaluexy( grid : TSingleGrid;
                         x,y : integer ) : single;
    begin
      result := -1;
      if assigned( grid ) then
         result := grid.valuexy(x,y);
    end;

   function splatgridvaluexy( grid : TIntGrid;
                              x,y : integer ) : integer;
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
   var x : integer;
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
                          loddiv : dword;
                          layer : integer =  layer_terrain) : TSingleGrid;
 var tilesz : integer;
     bufptr : ^single;
     ThisGrid : TSingleGrid;
     neighbor : TTerTile;
  procedure fullresolutionsample;
   var x : integer;
       ngrid : TSingleGrid;
   begin
     ngrid := neighborlayer( neighbor, layer );
     for x := 0 to tilesz - 1 do
      begin
        move( ThisGrid.ptrxy(x,0)^, bufptr^, tilesz * sizeof( single ));
        { last point in line }
        inc( bufptr, TileSz );
        if assigned( ngrid ) then
           bufptr^ := gridvaluexy(ngrid, x,0)
        else
           bufptr^ := gridvaluexy(thisgrid, x, tilesz-1);
        inc( bufptr );
      end;
     { last row }
     neighbor := GTileList.getNeighbor( tile, 1, 0 );
     ngrid := neighborlayer( neighbor, layer );
     if assigned( ngrid ) then
        move( nGrid.ptrxy(0,0)^, bufptr^, tilesz * sizeof( single ));
     inc( bufptr, TileSz );
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
        begin { copy last line if no neighbor }
          for y := 0 to tilesz - 1 do
           begin
             bufptr^ := thisgrid.valuexy( tilesz - 1, y * loddiv );
             inc( bufptr );
           end;
        end;
       { corner point from neighbor grid }
       neighbor := GTileList.getNeighbor( tile, 1, 1 );
       ngrid := neighborlayer( neighbor, layer );
       bufptr^ := gridvaluexy( ngrid, 0, 0 );
   end;
 begin
    tilesz := GDefGridCellCount div loddiv;
    case layer of
      layer_terrain : ThisGrid := Tile.TerrainGrid;
      layer_water  : ThisGrid := Tile.WaterGrid;
      layer_flora  : ThisGrid := Tile.FloraGrid;
      layer_splat  : assert( false ); { use BuildResultSplatGrid }
     end;
    Result := TSingleGrid.Create( 0, resultinfo.tilesz );
    bufptr := Result.ptrix(0);
    neighbor := GTileList.getneighbor( tile, 0, 1 );
    if ( loddiv = 1 ) then { full resolution }
       fullresolutionsample
    else
       reducedresolutionsample;
 end;


function BuildResultSplatGrid( tile : ttertile;
                               const resultinfo : TTileHeader) : TSingleGrid;
 var tilesz : integer;
     bufptr : ^single;
     ThisGrid : TSingleGrid;
  procedure fullresolutionsample;
   var x : integer;
       ngrid : TSingleGrid;
       neighbor : TTerTile;
   begin
     neighbor := GTileList.getneighbor( tile, 1, 1 );
     ngrid := neighborlayer( neighbor, layer_splat );
     for x := 0 to tilesz - 1 do
      begin
        move( ThisGrid.ptrxy(x,0)^, bufptr^, tilesz * sizeof( single ));
        { last point in line }
        inc( bufptr, TileSz );
        if assigned( ngrid ) then
           bufptr^ := gridvaluexy(ngrid, x, 0)
        else
           bufptr^ := gridvaluexy(thisgrid, x, tilesz-1);
        inc( bufptr, 1 );
      end;
     { last row }
     neighbor := GTileList.getNeighbor( tile, 1, 0 );
     ngrid := neighborlayer( neighbor, layer_splat );
     if assigned( ngrid ) then
        move( nGrid.ptrxy(0,0)^, bufptr^, tilesz * sizeof( single ))
     else
        move( thisgrid.ptrxy(tilesz - 1 ,0)^, bufptr^, tilesz * sizeof( single ));
     inc( bufptr, TileSz );
     neighbor := GTileList.getNeighbor( tile, 1, 1 );
     ngrid := neighborlayer( neighbor, layer_splat );
     if assigned( ngrid ) then
        bufptr^ := gridvaluexy( ngrid, 0, 0 )
     else
        bufptr^ := 0;
   end;
 begin
    tilesz := 60;
    ThisGrid := Tile.SplatGrid;
    Result := TSingleGrid.Create( 0, resultinfo.tilesz );
    bufptr := Result.ptrix(0);
    fullresolutionsample
 end;


//------------------------

constructor TTask_Tile.create( const iClient : TTileClient;
                               iTile : TTerTile;
                               iLOD : dword );
 begin
   inherited create( iClient );
   Tile := iTile;
   LOD := iLOD;
 end;

//------------------------

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
   client.SendClientMsgHeader( msg_Tile, buflen + sizeof( TTileHeader ));
   client.Send( resulttileinfo, sizeof( TTileHeader ));
   client.SendBuffer( buffer, buflen );

   result := true;
 end;

//----------------------------

function TTask_SendWater.RunTask : boolean;
 var buffer : TIdBytes;
     resulttileinfo : TTileHeader;
     buflen : integer;
     tilesz, loddiv : integer;
     resultwater : TSingleGrid;
     bufpos : integer;
 begin
   result := inherited Runtask;
   if not result then
      exit;
   resulttileinfo := Tile.info;
   loddiv := divOfLOD( LOD );
   tilesz := GDefGridCellCount div loddiv;

   resulttileinfo.tilesz := tilesz + 1;
   resultwater := BuildResultGrid( tile, resulttileinfo, LODdiv, layer_water );
   buflen := resultwater.wxh * sizeof( single );
   setlength( buffer, buflen );
   bufpos := 0;
   Move( resultwater.depthptr^, buffer[bufpos], resultwater.wxh*sizeof(single));
   client.SendClientMsgHeader( msg_water, buflen + sizeof( TTileHeader ));
   client.Send( resulttileinfo, sizeof( TTileHeader ));
   client.SendBuffer( buffer, buflen );
   resultwater.free;
   result := true;
 end;


function TTask_SendLODUpdateTile.RunTask : boolean;
{ send all data layers in one message for LOD change }
 var buffer : TIdBytes;
     resulttileinfo : TTileHeader;
     buflen : integer;
     tilesz, loddiv : integer;
     resultwater, resultterrain, resultflora : TSingleGrid;
     bufpos : integer;
 begin
   result := inherited Runtask;
   if not result then
      exit;
   resulttileinfo := Tile.info;
   loddiv := divOfLOD( LOD );
   tilesz := GDefGridCellCount div loddiv;

   resulttileinfo.tilesz := tilesz + 1;
   resultwater := BuildResultGrid( tile, resulttileinfo, LODdiv, layer_water );
   resultterrain := BuildResultGrid( tile, resulttileinfo, LODdiv, layer_terrain );
   resultflora := BuildResultGrid( tile, resulttileinfo, LODdiv, layer_flora );
   buflen := resultwater.wxh * sizeof( single ) * 3;
   setlength( buffer, buflen );
   bufpos := 0;
   Move( resultterrain.depthptr^, buffer[bufpos], resultterrain.wxh*sizeof(single));
   inc( bufpos, resultwater.wxh*sizeof(single) );
   Move( resultwater.depthptr^, buffer[bufpos], resultwater.wxh*sizeof(single));
   inc( bufpos, resultterrain.wxh*sizeof(single) );
   Move( resultflora.depthptr^, buffer[bufpos], resultflora.wxh*sizeof(single));
   client.SendClientMsgHeader( msg_LODUpdate, buflen + sizeof( TTileHeader ));
   client.Send( resulttileinfo, sizeof( TTileHeader ));
   client.SendBuffer( buffer, buflen );
   resultterrain.free;
   resultflora.free;
   resultwater.free;
 end;

//---------------------------

function TTask_SendFlora.RunTask : boolean;
 var buffer : TIdBytes;
     resulttileinfo : TTileHeader;
     buflen : integer;
     tilesz, loddiv : integer;
     resultflora : TSingleGrid;
     bufpos : integer;
 begin
   result := inherited Runtask;
   if not result then
      exit;
   resulttileinfo := Tile.info;
   loddiv := divOfLOD( LOD );
   tilesz := GDefGridCellCount div loddiv;

   resulttileinfo.tilesz := tilesz + 1;
   resultflora := BuildResultGrid( tile, resulttileinfo, LODdiv, layer_flora );
   buflen := resultflora.wxh * sizeof( single );
   setlength( buffer, buflen );
   bufpos := 0;
   Move( resultflora.depthptr^, buffer[bufpos], resultflora.wxh*sizeof(single));
   client.SendClientMsgHeader( msg_Flora, buflen + sizeof( TTileHeader ));
   client.Send( resulttileinfo, sizeof( TTileHeader ));
   client.SendBuffer( buffer, buflen );
   resultflora.free;
   result := true;
 end;

//---------------------------

function TTask_SendSplat.RunTask : boolean;
 var buffer : TIdBytes;
     resulttileinfo : TTileHeader;
     buflen : integer;
     tilesz : integer;
     resultsplat : TSingleGrid;
     bufpos : integer;
 begin
   result := inherited Runtask;
   if not result then
      exit;
   resulttileinfo := Tile.info;
   tilesz := 61;
   resulttileinfo.tilesz := tilesz;
   ResultSplat := BuildResultSplatGrid( tile, resulttileinfo );


   buflen := tilesz*tilesz*sizeof(integer);
   setlength( buffer, buflen );
   bufpos := 0;
   Move( resultsplat.depthptr^, buffer[bufpos], buflen );

   { send message + tile headers }
   client.SendClientMsgHeader( msg_Splat, buflen + sizeof( TTileHeader ));
   client.Send( resulttileinfo, sizeof( TTileHeader ));
   client.SendBuffer( buffer, buflen );

   resultsplat.free;
   result := true;
 end;

//---------------------------------

function TTask_SendTrees.runtask : boolean;
 var buffer : TIdBytes;
     buflen : integer;
     resultheader : TTileObjHeader;
     objlist : TTileObjList;
 begin
   result := inherited Runtask;
   if not result then
      exit;
   if tile.GetTypeList( tileobjtype_testtree, objlist ) then
    begin
      buflen := objlist.count;
      setlength( buffer, buflen );
      resultHeader.TileX := Tile.TileX;
      resultHeader.TileY := Tile.TileY;
      resultHeader.ObjType := tileobjtype_testtree;
      resultHeader.ObjCount := buflen;
      buflen := buflen * sizeof( ttileobj_rec );
      { send message + tile headers }
      client.SendClientMsgHeader( msg_Trees, buflen + sizeof( TTileObjHeader ));
      client.Send( resultheader, sizeof( TTileObjHeader ));
      client.Send( ObjList.ObjList[0], buflen );
    end;
 end;


//---------------------------------

function TTask_SaveTiles.runtask : boolean;
 var i : integer;
 begin
   result := inherited runtask;
   if result then
      for i := 0 to GTilelist.count - 1 do
         ttertile( GTilelist.at(i)).SaveToFile;
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

function TCmdList.executecommand( client : TTileClient;
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

//-----------------------------
      procedure BuildTerrainTile( tx, ty : integer; data : pointer );
       var Tile : TTerTile;
           thisLOD : integer;
       begin
         with  titeraterec( data^ ) do
          begin
            thisLOD := trunc(sqrt( sqr( tx - CenterX ) + sqr( ty - CenterY )));
            if ( thisLOD <= Radius ) then
             begin
               if UpdateTile( tx, ty, tile ) then { if the tile was created then we have to add a task to build it }
                  GTaskList.AddTask( TTask_BuildTile.create( client, Tile, Params ) )
             end
          end;
       end;

    procedure SendTerrainTile( tx, ty : integer; data : pointer );
     var Tile : TTerTile;
         thisLOD : integer;
         lastLOD : integer;
         subscription : TSubscription;
     begin
       with  titeraterec( data^ ) do
        begin
          thisLOD := trunc(sqrt( sqr( tx - CenterX ) + sqr( ty - CenterY )));
          if ( thisLOD <= Radius ) then
           begin
             lastLOD := -1;
             UpdateTile( tx, ty, tile );
             if Client.getSubscription( Tile, subscription ) then
                lastLOD := subscription.LOD;
             if lastLOD <> thisLOD then
              begin
                Client.setsubscription( Tile, thisLOD );
                GTaskList.AddTask( TTask_SendLODUpdateTile.create( client, Tile, thisLOD ) );
                GTaskList.AddTask( TTask_SendSplat.create( client, Tile, 1 ) ); { splatmap is always LOD 1 }
                Callback('');
              end;
           end
          else
          if updatetile( tx, ty, tile, false ) then
             Client.RemoveSubscription( tile );
        end;
     end;

   procedure SendWaterTile( tx, ty : integer; data : pointer );
    var Tile : TTerTile;
    begin
      with  titeraterec( data^ ) do
       begin
         if UpdateTile( tx, ty, tile, false ) then
          begin
            GTaskList.AddTask( TTask_SendWater.create( client, Tile, LOD ) );
          //  Callback('');
          end;
       end;
    end;

procedure buildTerrainArea( client : TTileClient;
                            CenterX, CenterY : integer;
                            Radius : integer;
                            const Params : TTerrainParams;
                            callback : TCommandCallback);
 var IterateRec : TIterateRec;
 begin
   iteraterec := initIterateRec( CenterX, CenterY, Radius, 1, Client, Params, callback );
   iteratearea( CenterX, CenterY, Radius,
                @iteraterec, {$ifdef FPC}@{$endif}BuildTerrainTile );
   iteratearea( CenterX, CenterY, Radius,
                @iteraterec, {$ifdef FPC}@{$endif}SendTerrainTile );
   GTaskList.AddTask( TTask_SaveTiles.create );
   Client.UnsubdistantTiles( point( centerx, centery ), radius );
 end;

procedure buildWaterArea( client : TTileClient;
                          CenterX, CenterY : integer;
                          Radius : integer;
                          LOD : integer;
                          callback : TCommandCallback);
 var IterateRec : TIterateRec;
 begin
   iteraterec := initIterateRec( CenterX, CenterY, Radius, LOD, Client, nil, callback );
   iteratearea( CenterX, CenterY, Radius,
                @iteraterec, {$ifdef FPC}@{$endif}SendWaterTile );
 end;

//---------------------------
// Commands

function cmdVersion( client : TTileClient;
                     params : string;
                     callback : TCommandCallback ) : integer;
 begin
   client.SendClientMsgHeader( msg_string, 0 );
   client.SendString( version );
   Result := 1;
 end;

function cmdBuildTile( client : TTileClient;
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
      buildTerrainArea( client, TileX, TileY, Radius, tileParams, callback );
    end;
 end;

function cmdWater( client : TTileClient;
                   params : string;
                   callback : TCommandCallback) : integer;
 var TileX, TileY : integer;
     param : string;
     Radius   : dword;
 begin
   result := 0;
   TileX := 0;
   TileY := 0;
   Radius := 0;
   if parsetilexy( params, tilex, tiley ) and nextparam( params, param ) then
      Radius := intofstr( param );
   buildWaterArea( client, TileX, TileY, Radius, 1, callback );
 end;

function cmdDig( client : TTileClient;
                 params : string;
                 callback : TCommandCallback ) : integer;
 var worldpos : tvector2;
     radius : integer;
     tile : ttertile;
 begin
   result :=0;
   WorldPos := vector2( 0, 0 );
   if parseworldxy( params, worldpos.x, worldpos.y ) then
    begin
      if gtilelist.findtileatlocation( WorldPos, Tile ) then
       begin
         radius := 1;
         parseint( params, radius );
         Tile.Dig( WorldPos, -0.01, radius );
         GTaskList.AddTask( TTask_SendTile.create( client, Tile, 1 ) );
       end;
    end;
 end;

function cmdPile( client : TTileClient;
                 params : string;
                 callback : TCommandCallback ) : integer;
 var worldpos : tvector2;
     tile : ttertile;
 begin
   result :=0;
   WorldPos := vector2( 0, 0 );
   if parseworldxy( params, worldpos.x, worldpos.y ) then
    begin
      if gtilelist.findtileatlocation( WorldPos, Tile ) then
       begin
         Tile.Dig( WorldPos, 0.01 );
         GTaskList.AddTask( TTask_SendTile.create( client, Tile, 1 ) );
       end;
    end;
 end;

function cmdPaint( client : TTileClient;
                   params : string;
                   callback : TCommandCallback ) : integer;
 var worldpos : tvector2;
     tile : ttertile;
     EncodedColor : integer;
     param : string;
 begin
   result :=0;
   WorldPos := vector2( 0, 0 );
   if parseworldxy( params, worldpos.x, worldpos.y ) then
    begin
      if gtilelist.findtileatlocation( WorldPos, Tile ) then
       begin
         EncodedColor := 0;
         if nextparam( params, param ) then
            EncodedColor := intofstr( Param );

         Tile.Paint( WorldPos, EncodedColor );
         GTaskList.AddTask( TTask_SendSplat.create( client, Tile, 1 ) );
       end;
    end;
 end;

function cmdSnowline( client : TTileClient;
                      params : string;
                      callback : TCommandCallback ) : integer;
 var newsnowline : single;
 begin
   result := 0;
   if parsesingle( params, newsnowline ) then
      DefaultSnowLine := newsnowline;
 end;

function cmdPlantTree( client : TTileClient;
                       params : string;
                       callback : TCommandCallback ) : integer;
 { addtree( posx, posy, radius : single; count : integer ) }
 var worldpos : tvector2;
     localpos : tvector2;
     wradius  : single;
     wcount   : integer;
     tile : ttertile;
     i : integer;
     treex, treey : word;
     objlist : TTileObjList;
     objrec : TTileObj_Rec;
     sn, cs, r : single;
 begin
   result := 0;
   if parseworldxy( params, worldpos.x, worldpos.y ) then
    begin
      if gtilelist.findtileatlocation( WorldPos, Tile ) then
       begin
         wradius := 0;
         wcount := 1;
         parsesingle( params, wradius );
         parseint( params, wcount );
         localpos := Tile.WorldToLocal( worldpos );
         objlist := nil;
         if Tile.GetTypeList( tileobjtype_testtree, objlist ) then
            for i := 0 to wcount - 1 do
             begin
               sincos( random * 2 * Pi, sn, cs );
               r := random * wradius;
               treex := trunc(( localpos.x + cs * r ));
               treey := trunc(( localpos.y + sn * r ));
               init_tileobj_rec( treex, treey,
                                 random( 65536 ), random( 65536 ), objrec );
               objlist.addobj( objrec );
             end;
         GTaskList.AddTask( TTask_SendTrees.create( client, Tile, 1 ) );
       end;
    end;
 end;

initialization
  GTaskList := TTaskList.Create;
  GCmdList := TCmdList.Create;
  GCmdList.RegisterCmd( 'version', @cmdVersion );
  GCmdList.RegisterCmd( 'build', @cmdBuildTile );
  GCmdList.RegisterCmd( 'water', @cmdWater );
  GCmdList.RegisterCmd( 'dig', @cmdDig );
  GCmdList.RegisterCmd( 'pile', @cmdPile );
  GCmdList.RegisterCmd( 'paint', @cmdPaint );
  GCmdList.RegisterCmd( 'snowline', @cmdSnowline );
  GCmdList.RegisterCmd( 'addtree', @cmdPlantTree );

//  GCmdList.RegisterCmd( 'save', @cmdSave );
finalization
  GCmdList.Free;
  GTaskList.Free;
end.
