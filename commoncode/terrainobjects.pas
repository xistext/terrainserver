unit TerrainObjects;

{ defines objects that are part of a terrain tile and do not move
  like trees, roads, intersections }

interface

uses sysutils, classes,
     collect,
     basetools,
     {$ifdef tersever}geobase,{$endif}
     CastleVectors,
     TerrainParams;

const converttosingle : single = 65536;
      converttoword : single = 1/65536;
      invGridCount : single = 0; { set in initialation }

      tileobjtype_undefined = 0;
      tileobjtype_testtree = 1;

type  tternodetype = word;
      ttersegtype  = word;

      { smallints are almost big enough to tile earth at current 600m tilesize }
      ttertileid = dword;
      ttertileid_unpacked = packed record
         posx, posy : smallint;
       end;

      { the ids for tiles and nodes are based on their positions.
        tile ids unique per tile, node ids unique within a tile }
      tternodeid = dword;
      tternodeid_unpacked = packed record
         nodetype   : tternodetype;
         posx, posy : word;   {0..65535 scaled in to the tile dimensions }
       end;

type {! this is used for the tree protyptypes and will be replaced by ttilenode_rec }
     ttileobj_rec  = packed record
       IdPos   : tternodeid_unpacked;  { position within tile }
       height  : word; { height and width could be 1/65536 of the max size of the type }
       radius  : word;
     end;


type ttersegid = dword; { terrain segments are global, id'ed by their position in a file }


type TTileObj_RecList = array of ttileobj_rec;

    { wraps a ttileobj_rec to convert to world units and work with or subclass }
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

    { holds a list of TTileObj_rec of the same type }
    TTileObjList = class

       objtype  : dword;
       objlist  : TTileObj_RecList;

       constructor create( itype : dword = tileobjtype_undefined;
                           isize : dword = 1 );
       function addobj( const info : ttileobj_rec ) : integer;

       private

       function getcount : integer;

       public

       property count : integer read getcount;

     end;

    { a list of TTileObjList sorted by objtype }
    TTileObjTypes = class( tsortedcollection ) { of TTileObjList }

       function objlistfortype( itype : dword;
                                var objlist : TTileObjList ) : boolean;
       function getobjlisttype( itype : dword ) : TTileObjList;

       function keyof( item : pointer ) : pointer; override;
       function compare( item1, item2 : pointer ) : integer; override;

     end;

implementation //===============================================================

function TTileObjTypes.objlistfortype( itype : dword;
                                       var objlist : TTileObjList ) : boolean;
 var i : integer;
 begin
   result := search( @itype, i );
   objlist := TTileObjList( at( i ));
 end;

function TTileObjTypes.getobjlisttype( itype : dword ) : TTileObjList;
 { will get the list for the type if exists, or create it and and it to the tile }
 var i : integer;
 begin
   if search( @itype, i ) then
      result := TTileObjList( at( i ))
   else
    begin
      result := TTileObjList.create( itype );
      atinsert( i, result );
    end;
 end;

function TTileObjTypes.keyof( item : pointer ) : pointer;
 begin
   result := @TTileObjList( item ).objtype;
 end;

function TTileObjTypes.compare( item1, item2 : pointer ) : integer;
 begin
   result := compareint( pinteger( item1 )^, pinteger( item2 )^ );
 end;

//----------------------------

constructor TTileObjList.create( itype : dword = tileobjtype_undefined;
                                     isize : dword = 1 );
 begin
   inherited create;
   objtype := itype;
   setlength( objlist, isize );
   fillchar( objlist[0], isize * sizeof( ttileobj_rec  ), 0 );
 end;

function TTileObjList.getcount : integer;
 begin
   result := length( objlist );
 end;

function TTileObjList.addobj( const info : ttileobj_rec ) : integer;
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
   assert( false );
//   result := converttosingle * info.width;
 end;

procedure TTileObject.setwidth( w : single );
 begin
   info.height := trunc( w * converttoword );
 end;

function TTileObject.getTilePos : TVector2;
 begin
   result := Vector2( info.idpos.posx * converttoword, info.idpos.posy * converttoword ) * GDefGridCellCount;
 end;

procedure TTileObject.setTilePos( const iPos : TVector2 );
 begin
   info.idpos.posx := trunc( iPos.x * converttosingle * invGridCount );
   info.idpos.posy := trunc( iPos.y * converttosingle * invGridCount );
 end;

initialization
  invGridCount := 1/GDefGridCellCount;
end.
