unit TerrainData;

{ Connects a basetile to the data a tile contains.
  Used by both client and server but there are differences,
    like tiles on client are 1x1 bigger than server because
    they include the seams to the next tile, which they don't on server. }

interface

uses Classes, SysUtils, Collect,
     CastleVectors, CastleTerrain, castleuriutils,
     {$ifdef terserver}
     castlefindfiles, castlefilesutils,
     {$else}
     treebuilder,
     {$endif}
     livetime,
     math, castletransform, castlewindow,
     watergrid, basetools,
     basetile, terrainobjects,
     TerServerCommon, terrainparams;

const terrainpath = 'castle-data:/terrain/';
      terrainext  = '.is.terra';
      waterext    = '.is.water';
      splatext    = '.is.splat';
      floraext    = '.is.flora';

      { defined tile layers }
      layer_terrain = 0; { height map  120x120 single }
      layer_water   = 1; { water depth 120x120 single }
      layer_flora   = 2; { flora depth 120x120 single }
      layer_splat   = 3; { splat map    60x60 integer }

type TTileStatus = byte;

     PTerTile = ^TTerTile;
     TTerTile = class; { forward }
     TTileNeighbors = array[0..7] of TTerTile;

     TIterateTilesProc = procedure( atile : ttertile;
                                    var doremove : boolean;
                                    data : pointer );

     { TTilelist manages all the tiles and is essentially the World.
       On the client this also holds the graphics used to represent
         the data received from the server }

     TTileList = class( TBaseTileList )

        function tilexy( x, y : integer ) : TTerTile;
        function ptrxy( x, y : integer ) : PTerTile;

        function inittile( const tileinfo : TTileHeader ) : TTerTile; virtual;

        function initxy( x, y : integer;
                         tilesz : integer ) : TTerTile;
        function getinittile( const tileinfo : TTileHeader ) : TTerTile;

        function getneighbor( tile : TTerTile;
                              dx, dy : integer ) : TTerTile;

        function findtileatlocation( const Pos : TVector2;
                                     var tile : TTerTile ) : boolean;

        procedure iteratetiles( tileproc : TIterateTilesProc; data : pointer );

        function ElevationAtPos( const Pos : TVector2;
                                 out Elevation : single ) : boolean;
        function WaterAtPos( const Pos : TVector2;
                             out WaterDepth : single ) : boolean;

      end;

     { TileList for use on the server }
     {$ifdef terserver}
     TTileList_Server = class( TTileList )

        function readallterrainfiles( path : string ) : integer;

//      function inittile( const tileinfo : TTileHeader ) : TTerTile; override;

        private

        procedure foundterfile( const FileInfo : TFileInfo; var StopSearch : boolean );

      end;
     {$endif}

     TDataLayer = class
        DataGrid : TBaseDataGrid;
        LastUpdateTime : single;
        constructor create( igridsz : dword );
        procedure initgrid( igridsz : dword ); dynamic;
        function gridsz : dword;
      end;

     TIntLayer = class( TDataLayer )
        procedure initgrid( igridsz : dword ); override;
      end;

     TDataLayers = array of TDataLayer;

     TTerTile = class( tbasetile )

        datalayers : TDataLayers;

        constructor create( const iInfo : TTileHeader );
        destructor destroy; override;

        function GetNeighbors : TTileNeighbors;

        function ElevationAtPos( const Pos : TVector2;
                                 out Elevation : single ) : boolean;
        function WaterAtPos( const Pos : TVector2;
                             out WaterDepth : single ) : boolean;

        public
        objlist   : TTileObjList; { lists of types that contain lists of objects }

        {$ifdef terserver}
        { server stores all and manages the data }
        Status : TTileStatus;

        procedure InitializeWithDefaults;
        procedure UpdateTerrainGridFromSource( Source : TCastleTerrainNoise );

        { calculates distance between tiles in tile ix units }

        { file handling }
        function SaveToFile : boolean;
        function LoadFromFile : boolean;
        function DeleteMyFiles : boolean;

        { tools }
        procedure Dig( const WorldPos : TVector2; Amount : single; Radius : integer = 1 );
        procedure Paint( const WorldPos : TVector2; EncodedColor : integer );

        function getWaterUpdateTime : single;
        {$endif}
        private

        function getTerrainGrid : TSingleGrid;
        function getWaterGrid : TSingleGrid;
        function getSplatGrid : TIntGrid;
        function getFloraGrid : TSingleGrid;

        public
        {$ifdef terserver}
        procedure setWaterUpdateTime( updatetime : single );
        property WaterUpdateTime : single read getWaterUpdateTime write setWaterUpdateTime;
        {$else}
        { client links to graphics }
        TerrainGraphics : TCastleTransform;
        WaterGraphics : TCastleTransform;
        procedure BuildTileObjGraphics( parentgraphic : TCastleTransform; LOD : integer = 1 );
        {$endif}

        procedure setTerrainGrid( aGrid : TSingleGrid );
        procedure setWaterGrid( aGrid : TSingleGrid );
        procedure setFloraGrid( aGrid : TSingleGrid );

        property TerrainGrid : TSingleGrid read getTerrainGrid write setTerrainGrid;
        property WaterGrid : TSingleGrid read getWaterGrid write setWaterGrid;
        property FloraGrid : TSingleGrid read getFloraGrid write setFloraGrid;
        property SplatGrid : TIntGrid read getSplatGrid;

      end;


const GTileList : {$ifdef terserver}TTileList_Server{$else}TTileList{$endif} = nil;

implementation
{$ifdef terserver}
uses waterflow;
{$endif}

procedure TTileList.iteratetiles( tileproc : TIterateTilesProc;
                                  data : pointer );

 var i : integer;
     doremove : boolean;
 begin
   i := 0;
   while i < count do
    begin
      doremove := false;
      tileproc( ttertile( at( i )), doremove, data );
      if doremove then
         atfree( i )
      else
         inc( i );
    end;
 end;

function TTileList.tilexy( x, y : integer ) : TTerTile;
 var i : integer;
 begin
   result := nil;
   lock;
   if findtile( x, y, i ) then
      result := TTerTile( at( i ));
   unlock;
 end;

function TTileList.ptrxy( x, y : integer ) : PTerTile;
 var i : integer;
 begin
   result := nil;
   if findtile( x, y, i ) then
      result := PTerTile( @It^[ i ]);
 end;

function TTileList.inittile( const tileinfo : TTileHeader ) : TTerTile;
 { initialize a tile for this list.  Allows subclasses to have different tiles. }
 begin
   result := TTerTile.create( tileinfo );
 end;

function TTileList.initxy( x, y : integer;
                           tilesz : integer ) : TTerTile;
 { Find tile at x, y.  If not found initialize and add the tile. }
 var i : integer;
     h : TTileHeader;
 begin
   lock;
   if findtile( x, y, i ) then
    begin
      result := TTerTile( at( i ));
      result.Info.TileSz := tilesz;
    end
   else
    begin
      sethxy( h, x, y, tilesz );
      result := inittile( h );
      atinsert( i, result );
    end;
   unlock;
 end;

function TTileList.getinittile( const tileinfo : TTileHeader ) : TTerTile;
 { Find the tile defined by the tileinfo.  If not found initialize and add it. }
 var i : integer;
 begin
   lock;
   if search( @tileinfo, i ) then
      result := TTerTile( at( i ))
   else
    begin
      result := inittile( tileinfo );
      atinsert( i, result );
    end;
   unlock;
 end;

function TTileList.getneighbor( tile : TTerTile;
                                dx, dy : integer ) : TTerTile;
 { returns the neighbor with the given dx, dy offset. }
 var ix, x, y : integer;
 begin
   result := nil;
   x := tile.TileX + dx;
   y := tile.TileY + dy;
   lock;
   if findtile( x, y, ix ) then
      result := TTerTile( at( ix ));
   unlock;
 end;

function TTileList.findtileatlocation( const Pos : TVector2;
                                       var tile : TTerTile ) : boolean;
 { Find the tile at the world location.
   If not found returns false. }
 var pt : TPoint;
 begin
   pt := CalculateTileOffset( Pos );
   tile := tilexy( pt.x, pt.y );
   result := assigned( tile );
 end;

function TTileList.ElevationAtPos( const Pos : TVector2;
                                   out Elevation : single ) : boolean;
 { Find elevation at world location from tile's terrain data.
   If no tile, returns false. }
 var tile : TTerTile;
 begin
   result := FindTileAtLocation( Pos, tile ) and
             tile.ElevationAtPos( Pos, Elevation );
 end;

function TTileList.WaterAtPos( const Pos : TVector2;
                               out WaterDepth : single ) : boolean;
{ Find water at world location from tile's water data.
  Server: this is depth.  Client: this is water elevation (terrainh + waterdepth).
  If no tile, returns false. }
 var tile : TTerTile;
 begin
   result := FindTileAtLocation( Pos, tile ) and
             tile.WaterAtPos( Pos, WaterDepth );
 end;

//-----------------------------------
{$ifdef terserver}
procedure TTilelist_Server.FoundTerFile( const FileInfo : TFileInfo; var StopSearch : boolean );
 var filename : string;
     dotpos : integer;
     tilepos : tpoint;
     tile : ttertile;
 begin
   filename := fileinfo.name;
   dotpos := pos( '.', filename );
   if dotpos > 0 then
      system.delete( filename, dotpos, length( filename ) - dotpos + 1 );
//   dbgwrite( filename + '  ' );
   tilepos := parsetilepos( filename );
   tile := initxy( tilepos.x, tilepos.y, GDefGridCellCount );
   tile.LoadFromFile;
   Application.ProcessAllMessages;
 end;

function TTileList_Server.ReadAllTerrainFiles( path : string ) : integer;
 begin
   result :=FindFiles( Path, '*'+terrainext, false, {$ifdef fpc}@{$endif} FoundTerFile, [ffReadAllFirst] );
 end;

{$endif}
//-------------------------------

constructor TDataLayer.create( igridsz : dword );
 begin
   LastUpdateTime := gametime;
   initgrid( igridsz );
 end;

procedure TDataLayer.initgrid( igridsz : dword );
 begin
   DataGrid := tsinglegrid.create( 0, igridsz );
 end;

function TDataLayer.gridsz : dword;
 begin
   assert( assigned( datagrid ));
   result := datagrid.wh;
 end;

//-------------------------------

procedure TIntLayer.initgrid( igridsz : dword );
 begin
   DataGrid := tintgrid.create( 0, igridsz );

 end;

//-------------------------------

constructor TTerTile.create( const iInfo : TTileHeader );
 var layer : TDataLayer;
 begin
   Info := iInfo;
   {$ifdef terserver}
   SetLength( datalayers, 4 ); { terrain + water + flora + splat }
   {$else}
   SetLength( datalayers, 3 ); { client doesn't store splatmap }
   {$endif}
   { intialize terrain layer }
   layer := TDataLayer.create( Info.TileSz );
   datalayers[layer_terrain] := layer;
   { initialize water layer }
   layer := TDataLayer.create( Info.TileSz );
   datalayers[layer_water] := layer;
   { initialize flora layer }
   layer := TDataLayer.create( Info.TileSz );
   datalayers[layer_flora] := layer;

   ObjList := TTileObjList.Create;
   {$ifdef terserver}
   layer := TIntLayer.create( 60 );
   datalayers[layer_splat] := layer;

   InitializeWithDefaults;
   status := 0;
   WaterToFlowList_high.addtask( TWaterTask.create( self )); {! this shouldn't happen here}
   {$else}
   TerrainGraphics := nil;
   WaterGraphics := nil;
   {$endif}
 end;

destructor TTerTile.destroy;
 var i : integer;
     {$ifdef terserver}item : TWaterTask;{$endif}
 begin
   inherited;
   for i := 0 to length( datalayers ) - 1 do
      datalayers[i].Free;
   setlength( datalayers, 0 );
   ObjList.Free;

   {$ifdef terserver}

   i := 0;
   while i < WaterToFlowList_high.Count do
    begin
      item := twatertask( WaterToFlowList_high.items[i] );
      if ( item is TWaterTask ) and ( item.watertile = self ) then
       begin
         WaterToFlowList_high.AtDelete( i );
         item.free;
         break;
       end
      else
         inc( i );
    end;
   {$endif}
 end;

{$ifdef terserver}
procedure TTerTile.InitializeWithDefaults;
 var x ,y : integer;
     layer : TDataLayer;
     treerec : TTileObj_Rec;
     i : dword;
 begin
   TSingleGrid( datalayers[layer_water].DataGrid ).setvalue( 0.1 );
   TSingleGrid( datalayers[layer_flora].DataGrid ).setvalue( 0.01 );
   { randomized splat layer with subdued colors and textures }
   layer := datalayers[layer_splat];
   for x := 0 to 59 do for y := 0 to 59 do
      TIntGrid(layer.DataGrid).setvaluexy( x, y,
          encodesplatcell( random(6), random(8), random(6), random(6), random(4), random(16)));
   { randomized trees }
   treerec.objtype := tileobjtype_testtree;
   treerec.dbid := 0;
   x := 0;
   repeat
      treerec.IdPos.posx := random( 65536 );
      treerec.IdPos.posy := random( 65536 );
      if not objlist.search( dword( treerec.IdPos ), i )  then
       begin
         treerec.size := random( 65536 );
         objlist.atinsert( treerec, i );
         inc( x );
       end;
    until x = 999;
 end;
{$endif}

function TTerTile.ElevationAtPos( const Pos : TVector2;
                                  out Elevation : single ) : boolean;
 var LocalPos : TVector2;
 begin
   LocalPos := worldtolocal( Pos );
   result := ( LocalPos.x >= 0 ) and ( LocalPos.x < GridCellCount ) and
             ( LocalPos.y >= 0 ) and ( LocalPos.y < GridCellCount );
   if result then
      Elevation := TerrainGrid.value_LinearInterpolate( LocalPos.x, LocalPos.y );
 end;

function TTerTile.WaterAtPos( const Pos : TVector2;
                              out WaterDepth : single ) : boolean;
 var LocalPos : TVector2;
 begin
   LocalPos := worldtolocal( Pos );
   result := ( LocalPos.x >= 0 ) and ( LocalPos.x < self.GridCellCount ) and
             ( LocalPos.y >= 0 ) and ( LocalPos.y < self.GridCellCount );
   if result then
      WaterDepth := WaterGrid.value_LinearInterpolate( LocalPos.x, LocalPos.y ) -
                    TerrainGrid.value_LinearInterpolate( LocalPos.x, LocalPos.y );
 end;

function TTerTile.getTerrainGrid : TSingleGrid;
 begin
   Result := TSingleGrid( datalayers[layer_terrain].DataGrid );
 end;

function TTerTile.getWaterGrid : TSingleGrid;
 begin
   Result := TSingleGrid( datalayers[layer_water].DataGrid );
 end;

function TTerTile.getSplatGrid : TIntGrid;
 begin
   Result := TIntGrid( datalayers[layer_splat].DataGrid );
 end;

function TTerTile.getFloraGrid : TSingleGrid;
 begin
   Result := TSingleGrid( datalayers[layer_flora].DataGrid );
 end;

procedure TTerTile.setTerrainGrid( aGrid : TSingleGrid );
 var layer : TDataLayer;
 begin
   layer := datalayers[layer_terrain];
   if assigned( layer.datagrid ) then
      layer.datagrid.free;
   layer.datagrid := aGrid;
 end;

procedure TTerTile.setWaterGrid( aGrid : TSingleGrid );
 var layer : TDataLayer;
 begin
   layer := datalayers[layer_water];
   if assigned( layer.datagrid ) then
      layer.datagrid.free;
   layer.datagrid := aGrid;
 end;

procedure TTerTile.setFloraGrid( aGrid : TSingleGrid );
 var layer : TDataLayer;
 begin
   layer := datalayers[layer_flora];
   if assigned( layer.datagrid ) then
      layer.datagrid.free;
   layer.datagrid := aGrid;
 end;

{$ifdef terserver}
function TTerTile.getWaterUpdateTime : single;
 begin
   result := datalayers[layer_water].LastUpdateTime
 end;

procedure TTerTile.setWaterUpdateTime( updatetime : single );
 begin
   datalayers[layer_water].LastUpdateTime := updatetime;
 end;

procedure TTerTile.UpdateTerrainGridFromSource( Source : TCastleTerrainNoise );
 var step, sz2, h0, tilesize : single;
     pos, queryoffset : TVector2;
     y, x, factor : dword;
     Grid : TSingleGrid;
 begin
   assert( assigned( source ));
   tilesize := getWorldSize;
   sz2 := tilesize * 0.5;
   factor := GDefGridCellCount div Info.TileSz;
   QueryOffset := Vector2( TileX * tilesize, TileY * tilesize);
   pos := Vector2( queryoffset.x-sz2, queryoffset.y-sz2 );
   step := GridStep * factor;
   Grid := TerrainGrid;

   for y := 0 to Grid.wh - 1 do
    begin
      for x := 0 to Grid.wh - 1 do
       begin
         h0 := Source.Height( pos, pos );
         Grid.SetValuexy( x, y, h0 );
         pos.x := pos.x + step;
       end;
      pos := vector2( queryoffset.x-sz2, pos.y + step );
    end;
   status := status or tile_built or tile_dirty;
end;

  procedure savegrid( filename : string; datagrid : TSingleGrid );
   var stream : TStream;
   begin
     filename := uriToFilenameSafe( filename );
     stream := TFileStream.Create(filename, fmCreate );
     stream.Write( datagrid.Data^, datagrid.datasz);
     stream.Free;
   end;

  procedure readgrid( filename : string; datagrid : TSingleGrid );
   var stream : TStream;
   begin
     stream := TFileStream.Create(filename, fmOpenRead);
     stream.Read( datagrid.Data^, datagrid.datasz);
     stream.Free;
   end;

function TTerTile.SaveToFile : boolean;
 var fileroot : string;
     dirty : boolean;
 begin
   fileroot := terrainpath + tileid;
   dirty := ( status and tile_dirty > 0 );
   if dirty then
    begin
      savegrid( fileroot + terrainext, getTerrainGrid );
      savegrid( fileroot + waterext, getWaterGrid );
      savegrid( fileroot + splatext, getSplatGrid );
      savegrid( fileroot + floraext, getFloraGrid );
//      dbgwrite( 'Saved '+tileid+'.  ' );
      if dirty then
         status := status xor tile_dirty;
    end;
   result := true;
 end;

function TTerTile.LoadfromFile : boolean;
 var filename, fileroot : string;
 begin
   fileroot := terrainpath + tileid;
   filename := fileroot + terrainext;
   result := fileexists( filename );
   if result then
    begin
      readgrid( filename, getTerrainGrid );
      if status and tile_dirty > 0 then
         status := status xor tile_dirty;
    end;
 end;

function TTerTile.DeleteMyFiles : boolean;
 var filename, fileroot : string;
 begin
   fileroot := terrainpath + tileid;
   filename := uriToFilenameSafe( fileroot + terrainext );
   result := fileexists( filename );
   if result then
      CheckDeleteFile( filename );
   filename := uriToFilenameSafe( fileroot + waterext );
   result := fileexists( filename );
   if result then
      CheckDeleteFile( filename );
   filename := uriToFilenameSafe( fileroot + floraext );
   result := fileexists( filename );
   if result then
      CheckDeleteFile( filename );
   filename := uriToFilenameSafe( fileroot + splatext );
   result := fileexists( filename );
   if result then
      CheckDeleteFile( filename );
 end;


procedure TTerTile.Dig( const WorldPos : TVector2; Amount : single; radius : integer = 1 );
 var TilePos : TVector2;
     TilePosI : TPoint;
     MaxTileIx : integer;
 begin
   TilePos := WorldToLocal( WorldPos );
   TilePosI.X := round( tilepos.x );
   TilePosI.Y := round( tilepos.y );
   MaxTileIx := TerrainGrid.wh - 1;
   LimitMax( TilePosI.X, MaxTileIx );
   LimitMax( TilePosI.Y, MaxTileIx );


   TerrainGrid.addxyvalue( tileposI.x, tileposI.y, amount );
 end;

procedure TTerTile.Paint( const WorldPos : TVector2; EncodedColor : integer );
 var TilePos : TVector2;
     SplatPosI : TPoint;
     MaxTileIx : integer;
 begin
   TilePos := WorldToLocal( WorldPos );
   SplatPosI.X := trunc( tilepos.x * 0.5 );
   SplatPosI.Y := trunc( tilepos.y * 0.5 );
   MaxTileIx := SplatGrid.wh - 1;
   assert( splatposi.x <= maxtileix );
   assert( splatposi.y <= maxtileix );
//   LimitMax( SplatPosI.X, MaxTileIx );
//   LimitMax( SplatPosI.Y, MaxTileIx );
   SplatGrid.setvaluexy( SplatPosI.x, SplatPosI.Y, EncodedColor );
 end;

{$else}
procedure TTerTile.BuildTileObjGraphics( parentgraphic : TCastleTransform; LOD : integer = 1 );
 var i : integer;
     it : PTileObj_Rec_Client;
     g : TCastleTransform;
     pos, anchorpos : tvector2;
     pos3 : tvector3;
     szfactor, factor, tilesz, h : single;
 begin
   anchorpos := WorldCorner00;
   tilesz := ( GridCellCount - 1 ) * GridStep;
   factor := tilesz/65536;
   szfactor := 1/65536; { converts to 0..1 world units }
   for i := 0 to objlist.count - 1 do
    begin
      it := @objlist.items[i];
      if not assigned( it^.ObjGraphics ) then
       begin
         case it^.objtype of
            tileobjtype_testtree :
            begin { build the graphics based on the type and size of the object }
               pos := vector2( anchorpos.x + it^.IdPos.posx * factor,
                               anchorpos.y + it^.IdPos.posy * factor );
               if ElevationAtPos( pos, h ) then
                begin
                  { add the build request to a build queue so it doesn't clog the program }
                  pos3 := vector3( pos.x, h, pos.y );
                  g := GTreeBuilder.BuildGraphics( parentgraphic, pos3, it^.size * szfactor, LOD );
                  parentgraphic.Add( g );
                  it^.ObjGraphics := g;
                end;
             end;
         end;
       end;
    end;
 end;
{$endif}

function TTerTile.GetNeighbors : TTileNeighbors;
 { get tile's neighbors }
 var iY, iX : integer;
 begin
   iX := TileX;
   iY := TileY;
   result[0] := GTileList.tilexy( iX, iY - 1);
   result[1] := GTileList.tilexy( iX + 1, iY - 1);
   result[2] := GTileList.tilexy( iX + 1, iY );
   result[3] := GTileList.tilexy( iX + 1, iY + 1);
   result[4] := GTileList.tilexy( iX, iY + 1);
   result[5] := GTileList.tilexy( iX - 1, iY + 1);
   result[6] := GTileList.tilexy( iX - 1, iY);
   result[7] := GTileList.tilexy( iX - 1, iY - 1);
 end;


initialization
  {$ifdef terserver}
  GTileList := TTileList_Server.create;
  {$else}
  GTileList := TTileList.create;
  {$endif}
finalization
  GTileList.Free;
end.
