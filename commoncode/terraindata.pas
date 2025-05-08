unit TerrainData;

interface

uses Classes, SysUtils, Collect, TerServerCommon, terrainparams,
     CastleVectors, CastleTerrain,
     {$ifdef terserver}castlefindfiles, castlefilesutils,{$endif}
     math, castletransform, castlewindow,
     watergrid, basetools;

const terrainpath = 'data\terrain\';
      terrainext  = '.is.terra';
      waterext    = '.is.water';
      splatext    = '.is.splat';
      floraext    = '.is.flora';
      rootpath = 'e:\terrainserver\';

      { defined tile layers }

      layer_terrain = 0; { height map  120x120 single }
      layer_splat   = 1; { splat map    60x60 integer }
      layer_water   = 2; { water depth 120x120 single }
      layer_flora   = 3; { flora depth 120x120 single }

type PTerTile = ^TTerTile;
     TTerTile = class; { forward }
     TTileNeighbors = array[0..7] of TTerTile;

     TIterateTilesProc = procedure( atile : ttertile;
                                    var doremove : boolean;
                                    data : pointer );

     TLockingCollection = class( tsortedcollection )
        constructor Create;
        function lock : boolean;
        procedure unlock;
        private
        locks : integer;
      end;

     { TTilelist manages all the tiles and is essentially the World.
       On the server this stores all the data for all the tiles.
       On the client this indexes the graphics used to represent
         the data received from the server }

     TTileList = class( TLockingCollection )

        function tilexy( x, y : integer ) : TTerTile;
        function ptrxy( x, y : integer ) : PTerTile;

        function initxy( x, y : integer;
                         tilesz : integer ) : TTerTile;
        function getinittile( const tileinfo : TTileHeader ) : TTerTile;


        function keyof( item : pointer ) : pointer; override;
        function compare( item1, item2 : pointer ) : integer; override;

        function getneighbor( tile : TTerTile;
                              dx, dy : integer ) : TTerTile;

        function CalculateTileOffset( Pos : TVector2 ) : TPoint;

        function findtileatlocation( const Pos : TVector2;
                                     var tile : TTerTile ) : boolean;

        function findtile( x, y : integer;
                           var ix : integer ) : boolean;

        procedure iteratetiles( tileproc : TIterateTilesProc; data : pointer );

        {$ifdef terserver}
        function readallterrainfiles( path : string ) : integer;

        private

        procedure foundterfile( const FileInfo : TFileInfo; var StopSearch : boolean );
        {$endif}


      end;

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

     TTerTile = class

        Info   : TTileHeader;

        constructor create( const iInfo : TTileHeader );
        destructor destroy; override;

        function getWorldSize : single;
        function gridStep : single;
        function tileid : string;
        function GetNeighbors : TTileNeighbors;
        function WorldToLocal( const pos : TVector2 ) : TVector2;

        public
        {$ifdef terserver}
        { server stores all and manages the data }
        Status : TTileStatus;
        datalayers : TDataLayers;
//        LastUpdateTime : single;
        LastResulttilesz : integer; {? store per client or compute on client?}

        function getLastUpdateTime( layerid : integer ) : single;
        procedure setLastUpdateTime( layerid : integer; updatetime : single );
        procedure UpdateTerrainGridFromSource( Source : TCastleTerrainNoise );

        { file handling }
        function SaveToFile : boolean;
        function LoadFromFile : boolean;
        function DeleteMyFiles : boolean;

        { tools }
        procedure Dig( const WorldPos : TVector2; Amount : single; Radius : integer = 1 );
        procedure Paint( const WorldPos : TVector2; EncodedColor : integer );

        private
        function getTerrainGrid : TSingleGrid;
        function getWaterGrid : TSingleGrid;
        function getSplatGrid : TIntGrid;
        function getFloraGrid : TSingleGrid;
        public
        property TerrainGrid : TSingleGrid read getTerrainGrid;
        property WaterGrid : TSingleGrid read getWaterGrid;
        property FloraGrid : TSingleGrid read getFloraGrid;
        property SplatGrid : TIntGrid read getSplatGrid;
        {$else}
        { client links to graphics }
        TerrainGraphics : TCastleTransform;
        WaterGraphics : TCastleTransform;
        {$endif}
      end;

procedure sethxy( var h : TTileHeader; x, y : smallint; sz : word = 1 );

const GTileList : TTileList = nil;

implementation
{$ifdef terserver}
uses waterflow;
{$endif}

procedure sethxy( var h : TTileHeader; x, y : smallint; sz : word = 1 ); inline;
 begin
   with h do
    begin
      tilex := x;
      tiley := y;
      tilesz := sz;
    end;
 end;

function worldtotile( wpos : tvector2 ) : tvector2;
var sizefactor : single;
    tilesz : single;
 begin
   tilesz := GDefGridCellCount * GDefGridStep;
   sizefactor := 1/tilesz;
   result := vector2( frac( wPos.X * SizeFactor + 0.5 ), frac( wPos.Y * SizeFactor + 0.5 ));
 end;

//----------------------------------

constructor TLockingCollection.Create;
 begin
   inherited;
   locks := 0;
 end;

function TLockingCollection.lock : boolean;
 var timeout : integer;
 begin
   timeout := 10;
   while locks > 0 do
    begin
      dec( timeout );
      if timeout > 0 then
       begin
//         dbgwrite('.');
         sleep( 10 ); {!need timeout}
       end
      else
       begin
//         dbgwrite('!');
         result := false;
         break;
       end;
    end;
   inc( locks );
   {if locks > 1 then
      assert( locks = 1 );}
   result := true;
 end;

procedure TLockingCollection.Unlock;
 begin
 {  if locks = 0 then
      assert( locks > 0 );}
   dec( locks );
 end;

//----------------------------------

function TTileList.keyof( item : pointer ) : pointer;
 begin
   result := @TTerTile( item ).Info;
 end;

function TTileList.compare( item1, item2 : pointer ) : integer;
 var h1, h2 : TTileHeader;
 begin
   h1 := PTileHeader( item1 )^;
   h2 := PTileHeader( item2 )^;
   result := compareint( h1.TileY, h2.TileY );
   if result = 0 then
      result := compareint( h1.TileX, h2.TileX );
 end;

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

function TTileList.findtile( x, y : integer;
                             var ix : integer ) : boolean;
 var h : ttileheader;
 begin
   sethxy( h, x, y );
   result := search( @h, ix );
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

function TTileList.initxy( x, y : integer;
                           tilesz : integer ) : TTerTile;
 var h : TTileHeader;
     i : integer;
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
      result := TTerTile.create( h );
      atinsert( i, result );
    end;
   unlock;
 end;

function TTileList.getinittile( const tileinfo : TTileHeader ) : TTerTile;
 var i : integer;
 begin
   lock;
   if search( @tileinfo, i ) then
      result := TTerTile( at( i ))
   else
    begin
      result := TTerTile.create( tileinfo );
      atinsert( i, result );
    end;
   unlock;
 end;

function TTileList.getneighbor( tile : TTerTile;
                                dx, dy : integer ) : TTerTile;
 var ix : integer;
     x, y : integer;
 begin
   result := nil;
   x := tile.Info.TileX + dx;
   y := tile.Info.TileY + dy;
   lock;
   if findtile( x, y, ix ) then
      result := TTerTile( at( ix ));
   unlock;
 end;

function TTileList.CalculateTileOffset( Pos : TVector2 ) : TPoint;
 var sizefactor : single;
     tilesz : single;
 begin
   tilesz := GDefGridCellCount * GDefGridStep;
   sizefactor := 1/tilesz;
   Result := Point( floor( Pos.X * SizeFactor + 0.5 ), floor( Pos.Y * SizeFactor + 0.5 ));
 end;

function TTileList.findtileatlocation( const Pos : TVector2;
                                        var tile : TTerTile ) : boolean;
 var pt : TPoint;
 begin
   pt := CalculateTileOffset( Pos );
   tile := tilexy( pt.x, pt.y );
   result := assigned( tile );
 end;

function parsetilepos( tilestr : string ) : tpoint;
 begin
   result := Point(0,0);
   if length( tilestr ) = 6 then
    begin
      result.X := strtoint(copy(tilestr,2,2)) * ( 1-2*ord( tilestr[1] = 'W' ));
      result.Y := strtoint(copy(tilestr,5,2)) * ( 1-2*ord( tilestr[4] = 'S' ));
    end;
 end;

{$ifdef terserver}
procedure TTilelist.FoundTerFile( const FileInfo : TFileInfo; var StopSearch : boolean );
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

function TTileList.ReadAllTerrainFiles( path : string ) : integer;
 begin
   result :=FindFiles( Path, '*'+terrainext, false, {$ifdef fpc}@{$endif} FoundTerFile, [ffReadAllFirst] );
 end;
{$endif}

//-------------------------------

constructor TDataLayer.create( igridsz : dword );
 begin
   LastUpdateTime := -1;
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
     x, y : integer;
 begin
   Info := iInfo;
   {$ifdef terserver}
   SetLength( datalayers, 4 );
   { intialize terrain layer }
   layer := TDataLayer.create( Info.TileSz );
   datalayers[layer_terrain] := layer;
   { initialize splat layer with randomized subdued colors and textures }
   layer := TIntLayer.create( 60 );
   for x := 0 to 59 do for y := 0 to 59 do
      TIntGrid(layer.DataGrid).setvaluexy( x, y,
          encodesplatcell( random(6), random(8), random(6), random(6), random(4), random(16)));

   datalayers[layer_splat] := layer;
   { initialize water layer }
   layer := TDataLayer.create( Info.TileSz );
   TSingleGrid( layer.DataGrid ).setvalue( 0.1 );
   datalayers[layer_water] := layer;
   { initialize flora layer }
   layer := TDataLayer.create( Info.TileSz );
   TSingleGrid( layer.DataGrid ).setvalue( 0.01 );
   datalayers[layer_flora] := layer;

   status := 0;
   //lastupdatetime := -1;
   lastresulttilesz := 0;
   WaterToFlowList_high.addtask( TWaterTask.create( self ));
   {$else}
   TerrainGraphics := nil;
   WaterGraphics := nil;
   {$endif}
 end;

destructor TTerTile.destroy;
 {$ifdef terserver}var i : integer; item : TWaterTask;{$endif}
 begin
   inherited;
   {$ifdef terserver}
   for i := 0 to length( datalayers ) - 1 do
      datalayers[i].Free;
   setlength( datalayers, 0 );
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

function zeropad( value : integer; len : integer  = 2 ) : string;
 begin
   result := Inttostr( value );
   if length( result ) < len then
      insert( '0', result, 0 );
 end;

function TTerTile.tileid : string;
 var token1, token2 : string;
 begin
   if info.TileX < 0 then
      token1 := 'W'+zeropad( abs( info.Tilex ))
   else
      token1 := 'E'+zeropad( info.tilex );
   if info.TileY < 0 then
      token2 := 'S'+zeropad( abs( info.Tiley ))
   else
      token2 := 'N'+zeropad( info.tiley );
   result := token1+token2;
 end;

function TTerTile.gridStep : single;
 var loddiv : integer;
 begin
   loddiv := GDefGridCellCount div ( Info.TileSz{$ifndef terserver}-1{$endif} );
   result := GDefGridStep * loddiv;
 end;

function TTerTile.getWorldSize : single;
 begin
   result := GDefGridCellCount * GDefGridStep;
 end;

function TTerTile.WorldToLocal( const pos : TVector2 ) : TVector2;
 var factor : single;
     offset : TVector2;
     tilesize : single;
 begin
   factor := 1/GridStep;
   tilesize := getWorldSize;

   offset := vector2( info.tilex * tilesize, info.tiley * tilesize );
   result := vector2((( pos.x - Offset.x ) + tilesize * 0.5 )*factor,
                     (( pos.y - Offset.y ) + tilesize * 0.5 )*factor );
 end;

{$ifdef terserver}
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

function TTerTile.getLastUpdateTime( layerid : integer ) : single;
 begin
   Result := datalayers[layerid].LastUpdateTime;
 end;

procedure TTerTile.setLastUpdateTime( layerid : integer; updatetime : single );
 begin
   datalayers[layerid].LastUpdateTime := updatetime;
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
   QueryOffset := Vector2( Info.TileX * tilesize, Info.TileY * tilesize);
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
   fileroot := rootpath + terrainpath + tileid;
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
   fileroot := rootpath + terrainpath + tileid;
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
   fileroot := rootpath + terrainpath + tileid;
   filename := fileroot + terrainext;
   result := fileexists( filename );
   if result then
      CheckDeleteFile( filename );
   filename := fileroot + waterext;
   result := fileexists( filename );
   if result then
      CheckDeleteFile( filename );
   filename := fileroot + floraext;
   result := fileexists( filename );
   if result then
      CheckDeleteFile( filename );
   filename := fileroot + splatext;
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



{$endif}

function TTerTile.GetNeighbors : TTileNeighbors;
 { get tile's neighbors }
 var iY, iX : integer;
 begin
   iX := Info.TileX;
   iY := Info.TileY;
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
  GTileList := TTileList.create;
finalization
  GTileList.Free;
end.
