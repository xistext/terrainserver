unit TerrainObjects;

{ Defines objects that are part of a terrain tile and do not move.
    They only need to store their position in the tile.
  This is for things that you wouldn't need to reference from another tile,
    like trees, rocks.
 }

interface

uses sysutils, classes,
     collect,
     basetools,
     {$ifdef tersever}geobase,{$endif}
     CastleVectors,
     TerrainParams;

const converttosingle : single = 65536;
      converttoword : single = 1/65536;

      tileobjtype_undefined = 0;
      tileobjtype_testtree  = 1;
      tileobjtype_testrock  = 2;

type  ttileobj_type = word;

      { the ids for tiles and nodes are based on their positions.
        tile ids unique per tile, node ids unique within a tile }
      ttileobjid = dword;
      ttileobjid_unpacked = packed record
         posx, posy : word;   {0..65535 scaled into the tile dimensions }
       end;

type TTileObj_Rec  = packed record { 12 bytes }
        IdPos    : ttileobjid_unpacked; {4} { position within tile }
        objtype  : ttileobj_type;       {2}
        size     : word;                {2} { object size 1/65536 of the max size of the type }
        dbid     : dword;               {4} { if there is more data for this object }
      end;

    PTileObj_Rec_Client = ^TTileObj_Rec_Client;
    TTileObj_Rec_Client = packed record { 12 bytes }
       IdPos    : ttileobjid_unpacked; {4} { position within tile }
       objtype  : ttileobj_type;       {2}
       size     : word;                {2} { object size 1/65536 of the max size of the type }
       dbid     : dword;               {4} { if there is more data for this object }
       ObjGraphics : pointer;              { on the client the rec is expanded to hold a reference to the graphics }
     end;

    TTileObj_RecList = array of ttileobj_rec;
    TTileObj_RecList_Client = array of ttileobj_rec_client;

    { wraps a ttileobj_rec to convert to world units and work with or subclass }
    TTileObject = class

       info : TTileObj_Rec;

       private

       function getsize : single;
       procedure setsize( h : single );

       function getTilePos : TVector2;
       procedure setTilePos( const iPos : TVector2 );

       public

       property WorldSize : single read getsize write setsize;
       property WorldPosition : TVector2 read getTilePos write setTilePos; { gets world position within tile }

    end;

    { holds a list of TTileObj_rec }
    TTileObjList = class

       {$ifdef terserver}
       items  : TTileObj_RecList;
       {$else}
       items : TTileObj_RecList_Client;
       {$endif}

       constructor create( isize : dword = 1 );
       function search( key : dword;
                        out index : dword ) : boolean;
       {$ifdef terserver}
       function addobj( const info : ttileobj_rec ) : boolean;
       procedure atinsert( const info : ttileobj_rec; i : integer );
       {$else}
       function addobj( const info : ttileobj_rec_client ) : boolean;
       procedure atinsert( const info : ttileobj_rec_client; i : integer );
       {$endif}

       private

       function getcount : integer;

       public

       property count : integer read getcount;

     end;

implementation //===============================================================

const invGridCount : single = 0; { set in initialation }

function comparekey( k1, k2 : dword ) : integer; inline;
begin
  result := ord( k1 > k2 ) - ord( k1 < k2 ); { branchless }
end;

//----------------------------

constructor TTileObjList.create( isize : dword = 1 );
 begin
   inherited create;
   setlength( items, isize );
   fillchar( items[0], isize * sizeof( ttileobj_rec  ), 0 );
 end;

function TTileObjList.getcount : integer;
 begin
   result := length( items );
 end;
{$ifdef terserver}
   function poskeyof( const item : TTileObj_Rec ) : dword;
    begin
      result := dword( item.IdPos );
    end;

function TTileObjList.addobj( const info : ttileobj_rec ) : boolean;
 { returns false if there is already an object with that id/position }
 var i : dword;
 begin
   i := 0;
   result := not search( poskeyof( info ), i );
   if result then
      insert( info, items, i );
 end;

procedure TTileObjList.atinsert( const info : ttileobj_rec; i : integer );
 var c : integer;
 begin
   c := count;
   if i = c then
    begin
      setlength( items, c + 1 );
      items[c] := info;
    end
   else
      insert( info, items, i );
 end;
{$else}
function poskeyof( const item : TTileObj_Rec_client ) : dword;
 begin
   result := dword( item.IdPos );
 end;

function TTileObjList.addobj( const info : ttileobj_rec_client ) : boolean;
{ returns false if there is already an object with that id/position }
var i : dword;
begin
i := 0;
result := not search( poskeyof( info ), i );
if result then
   insert( info, items, i );
end;

procedure TTileObjList.atinsert( const info : ttileobj_rec_client; i : integer );
var c : integer;
begin
c := count;
if i = c then
 begin
   setlength( items, c + 1 );
   items[c] := info;
 end
else
   insert( info, items, i );
end;
{$endif}

function TTileObjList.Search( key : dword;
                              out Index : dword) : boolean;
var L, H, I, C: Integer;
    cnt : integer;
begin
  Result := False;
  L := 0;
  cnt := count;
  H := cnt - 1;
  while L <= H do
   begin
     I := (L + H) shr 1; { div 2 }
     C := CompareKey( poskeyof( items[I] ), Key );
     if ( c < 0 ) then
        L := I + 1
     else
      begin
        H := I - 1;
        Result := ( c = 0 );
        IF Result THEN
           L := I;
      end;
   end;
  Index := L;
end;


//---------------------------

function TTileObject.getsize : single;
 begin
   result := converttosingle * info.size;
 end;

procedure TTileObject.setsize( h : single );
 begin
   info.size := trunc( h * converttoword );
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
