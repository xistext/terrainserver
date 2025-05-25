unit basetile;

{ Defines a tiles and tilelist to hold them.
  These are generalized logic without anything app specific
  other than the tile size parameters defined in terrainparams.  }

interface

uses classes, sysutils, collect, math,
     CastleVectors,
     basetools,
     terrainparams, terservercommon;

type TLockingCollection = class( tsortedcollection )
        constructor Create;
        function lock : boolean;
        procedure unlock;
        private
        locks : integer;
      end;

     { logical tist manages/sorts tiles in 2d }
     tbasetilelist = class( TLockingCollection )
        function keyof( item : pointer ) : pointer; override;
        function compare( item1, item2 : pointer ) : integer; override;
        function CalculateTileOffset( Pos : TVector2 ) : TPoint;
        function findtile( x, y : integer;
                           var ix : integer ) : boolean;
      end;

     { logical tile without app specific data }
     tbasetile = class
        Info   : TTileHeader;
        function tileid : string;
        function getWorldSize : single;
        function gridStep : single;
        function WorldToLocal( const pos : TVector2 ) : TVector2;
        function TileDist( const pos : tpoint ) : integer;
        property TileX : smallint read Info.TileX;
        property TileY : smallint read Info.TileY;
        property GridCellCount : word read Info.TileSz;

        function WorldCorner00 : TVector2;
      end;

procedure sethxy( var h : TTileHeader; x, y : smallint; sz : word = 1 ); inline;

implementation

procedure sethxy( var h : TTileHeader; x, y : smallint; sz : word = 1 ); inline;
 begin
   with h do
    begin
      tilex := x;
      tiley := y;
      tilesz := sz;
    end;
 end;

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
         write('.');
         sleep( 10 ); {!need timeout}
       end
      else
       begin
         write('!');
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

function TBaseTileList.keyof( item : pointer ) : pointer;
 begin
   result := @TBaseTile( item ).Info;
 end;

function TBaseTileList.compare( item1, item2 : pointer ) : integer;
 var h1, h2 : TTileHeader;
 begin
   h1 := PTileHeader( item1 )^;
   h2 := PTileHeader( item2 )^;
   result := compareint( h1.TileY, h2.TileY );
   if result = 0 then
      result := compareint( h1.TileX, h2.TileX );
 end;

function TBaseTileList.CalculateTileOffset( Pos : TVector2 ) : TPoint;
 var sizefactor : single;
     tilesz : single;
 begin
   tilesz := GDefGridCellCount * GDefGridStep;
   sizefactor := 1/tilesz;
   Result := Point( floor( Pos.X * SizeFactor + 0.5 ), floor( Pos.Y * SizeFactor + 0.5 ));
 end;

function TBaseTileList.findtile( x, y : integer;
                                 var ix : integer ) : boolean;
 var h : ttileheader;
 begin
   sethxy( h, x, y );
   result := search( @h, ix );
 end;


//----------------------------------

   function zeropad( value : integer; len : integer  = 2 ) : string;
    begin
      result := Inttostr( value );
      if length( result ) < len then
         insert( '0', result, 0 );
    end;

function TBaseTile.tileid : string;
 var token1, token2 : string;
 begin
   if TileX < 0 then
      token1 := 'W'+zeropad( abs( Tilex ))
   else
      token1 := 'E'+zeropad( tilex );
   if TileY < 0 then
      token2 := 'S'+zeropad( abs( Tiley ))
   else
      token2 := 'N'+zeropad( Tiley );
   result := token1+token2;
 end;

function TBaseTile.gridStep : single;
 var loddiv : integer;
 begin
   { client gets all tiles 1 row+col larger to handle seams, but still same 'cell count' }
   loddiv := GDefGridCellCount div ( Info.TileSz{$ifndef terserver}-1{$endif} );
   result := GDefGridStep * loddiv;
 end;

function TBaseTile.getWorldSize : single;
 begin
   result := GDefGridCellCount * GDefGridStep;
 end;

function TBaseTile.WorldToLocal( const pos : TVector2 ) : TVector2;
 var factor : single;
     offset : TVector2;
     tilesize : single;
 begin
   factor := 1/GridStep;
   tilesize := getWorldSize;
   offset := vector2( tilex * tilesize, tiley * tilesize );
   result := vector2((( pos.x - Offset.x ) + tilesize * 0.5 )*factor,
                     (( pos.y - Offset.y ) + tilesize * 0.5 )*factor );
 end;

function TBaseTile.TileDist( const pos : tpoint ) : integer;
 { computes distance between two tiles in tile index coordinates }
 begin
   result := trunc( sqrt( sqr( pos.x - tilex ) + sqr( pos.y - tiley )));
 end;

function TBaseTile.WorldCorner00 : TVector2;
 { returns world coordinates for corner that is -y and -x from the tile center }
 var offset : TVector2;
     tilesize : single;
 begin
   tilesize := getWorldSize;
   offset := vector2( tilex * tilesize, tiley * tilesize );
   result := vector2( Offset.x - tilesize * 0.5, Offset.y - tilesize * 0.5 );
 end;

end.
