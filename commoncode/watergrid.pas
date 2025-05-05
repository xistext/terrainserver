unit watergrid;

interface

{ data grid, not just for water, but anything
  optimized for walking pointers
  supports types single, smallint }

uses
  {$ifndef FPC}System.types,{$endif}
  Classes, SysUtils,
  basetools;

const defaultdepthscale : single = 1000; { 1000 allows +- 32.767 in 0.001 increments }

TYPE { never instantiated, used to typecase pointer }
     Tsingledata = array[0..99999999] of single;
     Tsmallidata = array[0..99999999] of smallint;

     Psingledata = ^Tsingledata;
     Psmallidata = ^Tsmallidata;

     { abstract untyped base grid }
     tbasedatagrid = class

        wh : dword;

        constructor create( initialSize : dword );
        destructor destroy; override;
        function wxh : dword;
        function valuesz : dword; dynamic; abstract;
        function datasz : dword;
        procedure zerogrid;
        procedure copyto( dest : tbasedatagrid );  {! types and sizes need to match!}

        public

        data : pointer; { untyped in base class, subclass will provide type }

     end;

     { grid of single }
     tsinglegrid = class( tbasedatagrid )

        constructor create( initialValue : single;
                            initialSize : dword  );
        constructor createsize( initialSize : dword );

        function valuesz : dword; override;

        procedure setvalue( value : single );
        procedure addvalue( value : single ;
                            minvalue : single = 0 );
        procedure addgrid( added : tsinglegrid );
        procedure addxyvalue( x, y : dword;
                              value: single );
        procedure setvaluexy( x, y : dword;
                              v : single ); overload;
        procedure setvaluexy( x, y : dword;
                              v : single;
                              var len : dword ); overload;
        function valuexy( x, y : dword ) : single;
        function valueix( ix : dword ) : single;
        function ptrxy( x, y : dword ) : psingle;
        function ptrix( ix : dword ) : psingle;
        function depthptr : psingle;

        function value_LinearInterpolate( xs, ys : single ) : single;

        function samplemax( var ptr : psingle; len : integer ) : single; overload;
        function samplemax( samplex, sampley, samplew, sampleh : dword ) : single; overload;

        private

        function depth : psingledata;

      end;

     { grid of smallint }
     tsmalligrid = class( tbasedatagrid )

        constructor create( initialValue : smallint;
                            initialSize : dword );
        function valuesz : dword; override;

        procedure setvalue( value : smallint );
        procedure addvalue( value : smallint;
                            minvalue : smallint = 0 );
        procedure addgrid( added : tsmalligrid );
        procedure addxyvalue( x, y : dword;
                              value: smallint );
        procedure setvaluexy( x, y : dword;
                              v : smallint ); overload;
        procedure setvaluexy( x, y : dword;
                              v : smallint;
                              var len : dword ); overload;
        function valuexy( x, y : dword ) : smallint;
        function valueix( ix : dword ) : smallint;
        function ptrxy( x, y : dword ) : psmallint;
        function ptrix( ix : dword ) : psmallint;

        private

        function depth : psmallidata;

      end;

     { grid of smallint that scale to single }
     tdepthgrid = class( tsmalligrid )

       scalefactor, iscalefactor : single;

       constructor create( initialValue : single;
                           initialSize : dword );

       function depthtoworld( dvalue : smallint ) : single;
       function worldtodepth( wvalue : single ) : smallint;

       procedure setvaluexy( x, y : dword;
                             v : single ); overload;
       procedure setvaluexy( x, y : dword;
                             v : single;
                             var len : dword ); overload;
       function valueix( ix : dword ) : single;
       function valuexy( x, y : dword ) : single;

       procedure setscalefactor( factor : single );

     end;

     tintgrid = class( tsinglegrid )

       constructor create( initialValue : integer;
                           initialSize : dword );

       procedure setvaluexy( x, y : dword;
                             v : integer); overload;
       procedure setvaluexy( x, y : dword;
                             v : integer;
                             var len : dword ); overload;
       function valueix( ix : dword ) : integer;
       function valuexy( x, y : dword ) : integer;

     end;


implementation //===============================================================

//-----------------------------------

constructor tbasedatagrid.Create( initialSize : dword );
 begin
   inherited create;
   wh := initialsize;
   data := allocmem( datasz );
 end;

destructor tbasedatagrid.destroy;
 begin
   Reallocmem( data, 0 );
   inherited destroy;
 end;

function tbasedatagrid.wxh : dword;
 begin
   result := wh * wh;
 end;

function tbasedatagrid.datasz : dword;
 begin
   Result := wh * wh * valuesz;
 end;

procedure tbasedatagrid.zerogrid;
 begin
   FillChar(data^, datasz, 0 );
 end;

procedure tbasedatagrid.copyto( dest : tbasedatagrid );
 begin
   assert( wh = dest.wh );
   Move( data^, dest.data^, datasz );
 end;

//-----------------------------------

constructor Tsinglegrid.create( initialValue : single;
                                initialSize : dword );
begin
  inherited create( initialSize );
  if initialvalue = 0 then
     zerogrid
  else
     setvalue( initialValue );
end;

constructor TSinglegrid.createsize( initialSize : dword );
 begin
   inherited create( initialSize );
   { does not initialize data values }
 end;

function TSingleGrid.valuesz : dword;
 begin
   result := sizeof( single );
 end;


procedure Tsinglegrid.setvalue( value : single );
 var dptr : psingle;
     i : dword;
 begin
   dptr := psingle( data );
   for i := wxh - 1 downto 0 do
    begin
      dptr^ := value;
      inc(dptr);
    end;
 end;

procedure Tsinglegrid.addvalue( value : single;
                                minvalue : single = 0 );
 var dptr : psingle;
     i : dword;
 begin
   dptr := psingle( data );
   for i := wxh - 1 downto 0 do
    begin
      dptr^ := dptr^ + value;
      inc( dptr );
    end;
 end;

function Tsinglegrid.depthptr : psingle;
 begin
   result := psingle(data);
 end;

function Tsinglegrid.ptrxy( x, y : dword ) : psingle;
 var dptr : psingle;
     ix : integer;
 begin
   assert (( x < wh ) and ( y < wh ));
   result := @depth^[x*wh+y];
 end;

function Tsinglegrid.ptrix( ix : dword ) : psingle;
 begin
   result := @depth^[ix];
 end;

procedure Tsinglegrid.addxyvalue( x, y : dword;
                                  value : single );
 var d : psingle;
 begin
   d := ptrxy( x,y );
   d^ := d^ + value;
 end;

function Tsinglegrid.valuexy( x, y : dword ) : single;
 begin
   assert(( x < wh ) and ( y < wh ));
   result := ptrxy( x, y )^;
 end;

function Tsinglegrid.valueix( ix : dword ) : single;
 begin
   result := depth^[ix];
 end;

procedure Tsinglegrid.setvaluexy( x, y : dword;
                                v : single;
                                var len : dword );
 var ptr : psingle;
     i : dword;
     fitlen : boolean;
 begin
   fitlen := x + len >= wh;
   len := ord( not fitlen ) * len + ord( fitlen ) * ( wh - x );
   ptr := ptrxy( x, y );
   for i := len - 1 downto 0 do
    begin
      ptr^ := v;
      inc( ptr );
    end;
 end;

procedure Tsinglegrid.setvaluexy( x, y : dword; v : single );
 begin
   ptrxy( x, y )^ := v;
 end;

function max( a, b : single ) : single; inline;
 var usea : boolean;
 begin
   usea := a >= b;
   result := a * ord( usea ) + ( b * ord( not usea ));
 end;

function Tsinglegrid.samplemax( var ptr : psingle;
                                    len : integer ) : single;
 var i : integer;
 begin
   result := ptr^;
   inc( ptr );
   for i := len - 2 downto 0 do
    begin
      result := max( result, ptr^ );
      inc( ptr );
    end;
 end;

function Tsinglegrid.samplemax( samplex, sampley, samplew, sampleh : dword ) : single;
 var x : integer;
     ptr : psingle;
 begin { gets max point in area }
   result := -9999999;
   for x := samplex to samplex + samplew - 1 do
    begin
      ptr := ptrxy( x, sampley );
      result := max( result, samplemax( ptr, sampleh ))
    end;
 end;



procedure Tsinglegrid.addgrid( added : Tsinglegrid );
 var srcptr, destptr : PSingle;
     i : dword;
 begin
   srcptr := psingle( added.data );
   destptr := psingle( data );
   for i := wxh - 1 downto 0 do
    begin
      destptr^ := destptr^ + srcptr^;
      inc( srcptr );
      inc( destptr );
    end;
 end;

function Tsinglegrid.depth : psingledata;
 begin
   result := Psingledata( data );
 end;

function Tsinglegrid.value_LinearInterpolate( xs, ys : single ) : single;
{ uses a bilinear interpolation }
VAR Ratio1, Ratio2, Ratio3, Ratio4 : single;
  R1, R2, R3, R4 : single;
  P : Psingle;
  x, y : integer;
 BEGIN
   Result := 0;
   X := TRUNC( xs );
   Y := TRUNC( ys );
   IF ( X >= 0 ) AND ( Y >= 0 ) AND ( X < wh - 1 ) AND ( Y < wh - 1 ) THEN
    BEGIN
      Ratio4 := Xs - X;
      Ratio3 := 1 - Ratio4;
      Ratio2 := Ys - Y;
      Ratio1 := 1 - Ratio2;
      P := ptrxy( x, y );
      Result := p^; { exact point.  no interpolation }
      IF ( Ratio2 <> 0  ) OR ( Ratio4 <> 0 ) THEN
       BEGIN { get neighbors and interpolate }
         R1 := Result;
         INC( P );
         R2 := P^;
         INC( P, ( wh - 1 ));
         r3 := p^;
         INC( P );
         r4 := P^;
         Result := Ratio3 * ( R1 * Ratio1 + R2 * Ratio2 ) + Ratio4 * ( R3 * Ratio1 + R4 * Ratio2 );
       END;
    END;
 END;



//-------------------------------------

constructor Tsmalligrid.create( initialValue : smallint;
                                initialSize : dword );
begin
  inherited create( initialSize );
  if initialvalue = 0 then
     zerogrid
  else
     setvalue( initialValue );
end;

function Tsmalligrid.valuesz : dword;
 begin
   result := sizeof( smallint )
 end;

procedure Tsmalligrid.setvalue( value : smallint );
 var dptr : psmallint;
     i : dword;
 begin
   dptr := psmallint( data );
   for i := wxh - 1 downto 0 do
    begin
      dptr^ := value;
      inc(dptr);
    end;
 end;

procedure Tsmalligrid.addvalue( value : smallint;
                                minvalue : smallint = 0 );
 var dptr : psmallint;
     depthvalue : smallint;
     i,c : dword;
 begin
   dptr := psmallint( data );
   for i := wxh shr 2 - 1 downto 0 do
    begin
      depthvalue := dptr^ + value;
      limitmin( depthvalue, minvalue );
      dptr^ := depthvalue;
      inc(dptr);
      {2 at once make any perforance difference?}
      depthvalue := dptr^ + value;
      limitmin( depthvalue, minvalue );
      dptr^ := depthvalue;
      inc(dptr);
    end;
 end;

function Tsmalligrid.ptrxy( x, y : dword ) : psmallint;
 begin
   assert (( x < wh ) and ( y < wh ));
   result := @depth^[x*wh+y];
 end;

function Tsmalligrid.ptrix( ix : dword ) : psmallint;
 begin
   result := @depth^[ix];
 end;

procedure Tsmalligrid.addxyvalue( x, y : dword;
                                  value : smallint );
 var d : psmallint;
 begin
   d := ptrxy( x,y );
   d^ := d^ + value;
 end;

function Tsmalligrid.valuexy( x, y : dword ) : smallint;
 begin
   result := ptrxy( x, y )^;
 end;

function Tsmalligrid.valueix( ix : dword ) : smallint;
 begin
   result := depth^[ix];
 end;

procedure Tsmalligrid.setvaluexy( x, y : dword;
                                  v : smallint;
                                  var len : dword );
 var ptr : psmallint;
     i : dword;
 begin
   if x + len >= wh then
      len := wh - x;
   ptr := ptrxy( x, y );
   for i := len - 1 downto 0 do
    begin
      ptr^ := v;
      inc( ptr );
    end;
 end;

procedure Tsmalligrid.setvaluexy( x, y : dword; v : smallint );
 begin
   ptrxy( x, y )^ := v;
 end;

procedure Tsmalligrid.addgrid( added : Tsmalligrid );
 var srcptr, destptr : psmallint;
     i : dword;
 begin
   srcptr := psmallint( added.data );
   destptr := psmallint( data );
   for i := wxh - 1 downto 0 do
    begin
      destptr^ := destptr^ + srcptr^;
      inc( srcptr );
      inc( destptr );
    end;
 end;

function Tsmalligrid.depth : psmallidata;
 begin
   result := Psmallidata( data );
 end;

//-------------------------------------

constructor tdepthgrid.create( initialValue : single;
                               initialSize : dword );
 begin
   setscalefactor( defaultdepthscale ); { scale the values for converting between smallint and single }
   inherited create( trunc( initialValue * scalefactor ), initialsize);
 end;

procedure tdepthgrid.setscalefactor( factor : single );
 begin
   scalefactor := factor;
   iscalefactor := 1/factor;
 end;

function tdepthgrid.depthtoworld( dvalue : smallint ) : single;
 begin
   result := dvalue * iscalefactor;
 end;

function tdepthgrid.worldtodepth( wvalue : single ) : smallint;
 begin
   result := trunc( wvalue * scalefactor );
 end;

procedure tdepthgrid.setvaluexy( x, y : dword;
                                 v : single );
 begin
   inherited setvaluexy( x, y, trunc( v * scalefactor ));
 end;


procedure tdepthgrid.setvaluexy( x, y : dword;
                                 v : single;
                                 var len : dword );
 begin
   inherited setvaluexy( x, y, trunc( v * scalefactor ), len );
 end;

function tdepthgrid.valueix( ix : dword ) : single;
 begin
   result := inherited valueix( ix ) * iscalefactor;
 end;

function tdepthgrid.valuexy( x, y : dword ) : single;
 begin
   result := inherited valuexy( x, y ) * iscalefactor;
 end;

//-------------------------------------

constructor tintgrid.create( initialValue : integer;
                             initialSize : dword );
 begin
   inherited create( initialValue, initialsize );
 end;

type pinteger = ^integer;

procedure tintgrid.setvaluexy( x, y : dword;
                               v : integer);
 begin
   pinteger( ptrxy( x, y ))^ := v;
 end;


procedure tintgrid.setvaluexy( x, y : dword;
                                 v : integer;
                                 var len : dword );
 var ptr : ^integer;
     i : dword;
     fitlen : boolean;
 begin
   fitlen := x + len >= wh;
   len := ord( not fitlen ) * len + ord( fitlen ) * ( wh - x );
   ptr := pinteger( ptrxy( x, y ));
   for i := len - 1 downto 0 do
    begin
      ptr^ := v;
      inc( ptr );
    end;
 end;

function tintgrid.valueix( ix : dword ) : integer;
 begin
   result := integer( depth^[ix] );
 end;

function tintgrid.valuexy( x, y : dword ) : integer;
 begin
   assert(( x < wh ) and ( y < wh ));
   result := pinteger( ptrxy( x, y ))^;
 end;


end.

