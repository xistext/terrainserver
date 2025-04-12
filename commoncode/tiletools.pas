unit TileTools;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$ifndef FPC}System.types,{$endif}
  Classes, SysUtils, collect;

type TTileList = CLASS( TSortedCollection )
        function compare(item1, item2 : pointer) : integer; override;
        function keyof( item : pointer ) : pointer; override;
        function itemxy( x, y : integer ) : pointer;
      end;

implementation
uses BaseTileGrid;

type ppoint = ^tpoint;

function TTileList.compare(item1, item2 : pointer) : integer;
var p1, p2 : TPoint;
begin
   result := 0;
   p1 := ppoint( item1 )^;
   p2 := ppoint( item2 )^;
   if p1.Y < p2.Y then
      result := -1
   else
   if p1.Y > p2.Y then
      result := 1
   else
   if p1.X < p2.X then
      result := -1
   else
   if p1.X > p2.X then
      result := 1
 end;

function TTileList.keyof( item : pointer ) : pointer;
 begin
   Result := @TBaseTile(item).offset;
 end;

function TTileList.itemxy( x, y : integer ) : pointer;
 var p : tpoint;
     i : integer;
 begin
   result := nil;
   p := point( x, y );
   if search( @p, i ) then
      result := tbasetile( at(i) ).tilecontents
 end;

 end.

