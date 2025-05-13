unit TerrainObjects;

{ defines objects that are part of a terrain tile }

interface

uses sysutils, classes,
     basetools,
     CastleVectors,
     TerrainParams;

const converttosingle : single = 65536;
      converttoword : single = 1/65536;
      invGridCount : single = 0; { set in initialation }

      tileobjtype_undefined = 0;
      tilebopjtype_testtree = 1;

type ttileobj_rec  = packed record
        posx    : word; { 1/65536 per tile size, about 1cm in current 600m per tile scaling  }
        posy    : word;
        height  : word; { height and width could be 1/65536 of the max size of the type }
        width   : word;
      end;

    { wraps a ttileobj_info to convert to world units and work with or subclass }
    TTileObject = class

       info : ttileobj_rec;

       private

       function getheight : single;
       procedure setheight( h : single );

       function getwidth : single;
       procedure setwidth( w : single );

       function getTilePos : TVector2;
       procedure setTilePos( const iPos : TVector2 );

       public

       property WorldHeight : single read getheight write setheight;
       property WorldWidth  : single read getwidth write setwidth;

       property WorldPosition : TVector2 read getTilePos write setTilePos; { gets world position within tile }

    end;

    { holds a list of TTileObj_info of the same }
    TTileObjTypeList = class
       objtype  : dword;
       objlist  : array of TTileObj_Rec;
       constructor create( itype : dword = tileobjtype_undefined;
                           isize : dword = 1 );
       function addobj( const info : ttileobj_rec ) : integer;

       private

       function getcount : integer;

       public

       property count : integer read getcount;

     end;

procedure init_tileobj_info( iposx, iposy, iheight, iwidth : word;
                             out info : ttileobj_rec );

implementation //===============================================================

procedure init_tileobj_info( iposx, iposy, iheight, iwidth : word;
                             out info : ttileobj_rec );
 begin
   with info do
    begin
      posx := iposx;
      posy := iposy;
      height := iheight;
      width := iwidth;
    end;
 end;

//----------------------------

constructor TTileObjTypeList.create( itype : dword = tileobjtype_undefined;
                                     isize : dword = 1 );
 begin
   inherited create;
   objtype := itype;
   setlength( objlist, isize );
   fillchar( objlist[0], isize * sizeof( ttileobj_rec  ), 0 );
 end;

function TTileObjTypeList.getcount : integer;
 begin
   result := length( objlist );
 end;

function TTileObjTypeList.addobj( const info : ttileobj_rec ) : integer;
 { returns handle, its index in objlist }
 var c : integer;
 begin
   c := length( objlist );
   result := c + 1;
   setlength( objlist, result );
   objlist[c] := info;
 end;

//---------------------------

function TTileObject.getheight : single;
 begin
   result := converttosingle * info.height;
 end;

procedure TTileObject.setheight( h : single );
 begin
   info.height := trunc( h * converttoword );
 end;

function TTileObject.getwidth : single;
 begin
   result := converttosingle * info.width;
 end;

procedure TTileObject.setwidth( w : single );
 begin
   info.height := trunc( w * converttoword );
 end;

function TTileObject.getTilePos : TVector2;
 begin
   result := Vector2( info.posx * converttoword, info.posy * converttoword ) * GDefGridCellCount;
 end;

procedure TTileObject.setTilePos( const iPos : TVector2 );
 begin
   info.posx := trunc( iPos.x * converttosingle * invGridCount );
   info.posy := trunc( iPos.y * converttosingle * invGridCount );
 end;

initialization
  invGridCount := 1/GDefGridCellCount;
end.
