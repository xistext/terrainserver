unit TerrainObjects;

{ defines objects that are part of a terrain tile }

interface

uses classes,
     collect,
     basetools,
     CastleVectors,
     TerrainParams;

const converttosingle : single = 65536;
      converttoword : single = 1/65536;
      invGridCount : single = 0; { set in initialation }

      tertype_undefined = 0;
      tertype_tree      = 1;

type tterobjinfo = packed record
        id      : word; { 65536 objs of a given type in a tile }
        posx    : word;
        posy    : word;
        height  : word;
      end;

    TTerObject = class

       info : tterobjinfo;

       private

       function getheight : single;
       procedure setheight( h : single );

       function getTilePos : TVector2;
       procedure setTilePos( const iPos : TVector2 );

       public

       property height : single read getheight write setheight;
       property id : word read info.id write info.id;

    end;

    { holds a list of TTerObject by type }
    TTerObjTypeList = class( TSortedCollection ) { of TTerObject }
       objtype  : dword;
       function keyof( item : pointer ) : pointer; override;
       function compare( item1, item2 : pointer ) : integer; override;

     end;


implementation

function TTerObjTypeList.keyof( item : pointer ) : pointer;
 begin
   result := @TTerObject( item ).id;
 end;

function TTerObjTypeList.compare( item1, item2 : pointer ) : integer;
 begin
   result := compareint( pinteger( item1 )^, pinteger( item2 )^ );
 end;



//---------------------------

function TTerObject.getheight : single;
 begin
   result := converttosingle * info.height;
 end;

procedure TTerObject.setheight( h : single );
 begin
   info.height := trunc( h * converttoword );
 end;

function TTerObject.getTilePos : TVector2;
 begin
   result := Vector2( info.posx * converttoword, info.posy * converttoword ) * GDefGridCellCount;
 end;

procedure TTerObject.setTilePos( const iPos : TVector2 );
 begin
   info.posx := trunc( iPos.x * converttosingle * invGridCount );
   info.posy := trunc( iPos.y * converttosingle * invGridCount );
 end;

initialization
  invGridCount := 1/GDefGridCellCount;
end.
