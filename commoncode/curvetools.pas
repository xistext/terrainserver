unit curvetools;

//{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  CastleVectors,
  basetools;

type TStepInfo = class
        steps : integer;
      end;

     TArcPt = record
        pt    : TVector2;
        angle : single;
//        heading : single;
      end;

     TArcIterateProc = procedure ( const arcpt : TArcPt;
                                   data : pointer );

     TArcDef = record
        dir     : shortint;  {+1=clockwise, -1=counterclockwise, 0=notcurved}
        radius  : single;
        center  : TVector2;
        startangle, endangle : single; { start and end angle of arc from center }
        heading1  : single;   {from heading leading into p1}
        heading2  : single;   {to heading leading from p2 }
      end;

     {! want to switch to this simpler, safer curve definition }
     TArcDef2 = record
        center : TVector2;
        radius : single;
        startangle : single;
        deltaangle : single;
      end;

     TCurveStepInfo = class( TStepInfo )

       curvedef : TArcDef;

       anglestep : single;
       continuesstraight : boolean;
       validarcintersect : boolean;
       arcintersect : TVector2;

       constructor create; overload;
       constructor create( const icurvedef : TArcDef ); overload;

       procedure copyfrom( curve : TCurveStepInfo );

       property radius : single read curvedef.radius write curvedef.radius;
       property heading1 : single read curvedef.heading1 write curvedef.heading1;
       property heading2 : single read curvedef.heading2 write curvedef.heading2;
       property dir : shortint read curvedef.dir write curvedef.dir;
       property center : TVector2 read curvedef.center write curvedef.center;
       property startangle : single read curvedef.startangle write curvedef.startangle;
       property endangle : single read curvedef.endangle write curvedef.endangle;

       function length : single; { calculate length of arc in world coordinates }
       function curved : boolean;

       function ptatangle( angle : single ) : TVector2;

       function calculateCurveIntersect( const p1, p2 : TVector2;
                                         var intersect : TVector2 ) : boolean;
       procedure calculateStepInfo;
       procedure normalizeangles;
       procedure reset;
       procedure setLR( var pL, pR : TVector3;
                            Offset : single );

       procedure nextangle;
       function ptatcurrangle( r : single ) : TVector2; overload;
       function ptatcurrangle : TVector2; overload;

       private

       Fsina, Fcosa : single; { sin & cos of currangle }
       Fcurrangle : single;

       procedure setcurrangle( a : single );

       public

       property currAngle : single read Fcurrangle write setcurrangle;

     end;

function _ptatangle( const arcdef : tarcdef;
                     angle : single ) : TVector2; overload;
function _ptatangle( const center : tvector2;
                     radius, angle : single;
                     dir : integer ) : TVector2; overload;

function _angleofpt( const pt : tvector2;
                     const center : tvector2;
                     dir : integer ) : single;

procedure _iteratearc( const arcdef : tarcdef;
                       steps : integer;
                       data : pointer;
                       IterateProc : TArcIterateProc ); overload;
procedure _iteratearc( const center : tvector2;
                       radius : single;
                       startangle, endangle : single;
                       dir : integer;
                       steps : integer;
                       data : pointer;
                       IterateProc : TArcIterateProc ); overload;

implementation  //==============================================================

{ calculate the point on the given angle and radius from the center }

function _ptatangle( const arcdef : tarcdef;
                     angle : single ) : TVector2;
 begin
   with arcdef do
      result := _ptatangle( center, radius, angle, dir );
 end;

function _ptatangle( const center : tvector2;
                     radius, angle : single;
                     dir : integer ) : TVector2;
 var sina, cosa : single;
 begin
   sincos( angle, sina, cosa );
   radius := dir * abs( radius );
   result := center + Vector2( cosa * radius, sina * radius );
 end;

function _angleofpt( const pt : tvector2;
                     const center : tvector2;
                     dir : integer ) : single;
 var delta : TVector2;
 begin
   delta := vector2( dir * ( pt.x - center.x ), dir * ( pt.y - center.y ));
   if abs( delta.x ) > 0 then
      Result := arctan2(delta.y,delta.x)
   else
      result := 0;
 end;

//--------------------------------

procedure _iteratearc( const arcdef : tarcdef;
                       steps : integer;
                       data : pointer;
                       IterateProc : TArcIterateProc );
 begin
   with arcdef do
      _iteratearc( center, radius, startangle, endangle, dir, steps, data, IterateProc );
 end;

procedure _iteratearc( const center : tvector2;
                       radius : single;
                       startangle, endangle : single;
                       dir : integer;
                       steps : integer;
                       data : pointer;
                       IterateProc : TArcIterateProc );
 var i : integer;
     arcpt : TArcPt;
     deltaangle : single;
 begin
   assert( steps > 0 );
   deltaangle := ( EndAngle - StartAngle ) / steps; {! normalized by direction? }
   arcpt.angle := StartAngle;
   arcpt.Pt := _ptatangle( center, radius, arcpt.angle, dir );
   IterateProc( arcpt, data );
   for i := 0 to steps - 1 do
    begin
      arcpt.angle := arcpt.angle + deltaangle;
      arcpt.Pt := _ptatangle( center, radius, arcpt.angle, dir );
      IterateProc( arcpt, data );
    end;
 end;

//-------------------------------------

constructor TCurveStepInfo.create;
 begin
   curvedef.dir := 0;
   curvedef.radius := 0;
   curvedef.center := Vector2( 0, 0 );
   curvedef.heading1 := 0;
   curvedef.heading2 := 0;
   curvedef.startangle := 0;
   curvedef.endangle := 0;
   validarcintersect := false;
   continuesstraight := false;
   arcintersect := Vector2( 0, 0 );
   anglestep := 0;
   steps := 0;
   Fcurrangle := 0;
   Fsina := 0;
   Fcosa := 1;
 end;

constructor TCurveStepInfo.create( const icurvedef : TArcDef );
 begin
   curvedef := icurvedef;
   validarcintersect := false;
   continuesstraight := false;
   arcintersect := Vector2( 0, 0 );
   anglestep := 0;
   steps := 0;
 end;

procedure TCurveStepInfo.copyfrom( curve : TCurveStepInfo );
 begin
   curvedef := curve.curvedef;
   anglestep := curve.anglestep;
   steps := curve.steps;
   validarcintersect := curve.validarcintersect;
   continuesstraight := curve.continuesstraight;
   arcintersect := curve.arcintersect;
 end;

procedure TCurveStepInfo.reset;
 begin
   validarcintersect := false;
 end;

function TCurveStepInfo.length : single;
 begin { length of the arc in world units }
   result := radius * abs( anglestep ) * steps;
 end;

function TCurveStepInfo.curved : boolean;
 begin
   result := dir <> 0;
 end;

function TCurveStepInfo.ptatangle( angle : single ) : TVector2;
 begin
   result := _ptatangle( curvedef, angle );
 end;

procedure TCurveStepInfo.setCurrAngle( a : single );
 begin
   Fcurrangle := a;
   sincos( a, FSinA, FCosA );
 end;

function TCurveStepInfo.ptatcurrangle( r : single ) : TVector2;
 begin
   Result := Vector2( center.x - dir * Fcosa * r, center.y - dir * Fsina * r )
 end;

function TCurveStepInfo.ptatcurrangle : TVector2;
 begin
   Result := Vector2( center.x - dir * Fcosa * radius, center.y - dir * Fsina * radius )
 end;

procedure TCurveStepInfo.nextAngle;
 begin
   Fcurrangle := Fcurrangle + anglestep;
   sincos( Fcurrangle, FSinA, FCosA );
 end;

function TCurveStepInfo.calculateCurveIntersect( const p1, p2 : TVector2;
                                             var intersect : TVector2 ) : boolean;
 var P12dist, P23dist : single;
     angledif : single;
     p21heading : single;
     delta : TVector2;
     pdelta : TVector2;
     sintheta, costheta : single;
     cL, cR : TVector2;
     angle12 : single;
     r : single;
 begin
   result := false;
   validarcintersect := false;
   r := abs( radius );
   calccorners( p1, heading1 + Pi/2 { 90 deg }, r, cL, cR );
   angle12 := AngleFromDelta( p2 - p1 );
   { determine if turning left or right }
   angledif := normalizeheading( normalizeheading( angle12 ) - normalizeHeading( heading1 ));
   if angledif < 0 then
    begin { turn left }
      dir := -1;
      delta := cR - p2;
      center := cR;
    end
   else { turn right }
    begin
      dir := +1;
      delta := cL - p2;
      center := cL;
    end;
   P12dist := hypot( delta.X, delta.Y );
   if abs( p12dist - r ) < 1E-5 then
    begin
      intersect := p2;
      continuesstraight := false;
      result := true;
    end
   else
   if p12dist > r then
    begin { with the curve intersect point, we can determine the start and end angle and step info }
      P23dist := sqrt( abs( sqr( P12dist ) - sqr( r )));

      angledif := arcsin( r / P12dist );
      p21heading := anglefromdelta( delta ) + angledif * dir;
      sincos( p21heading, sintheta, costheta );
      pdelta := Vector2( costheta * P23dist, sintheta * P23dist );
      intersect := P2 + pDelta;
      continuesstraight := true;
      result := true;
    end;

   if result then
    begin
      { determine angle from center of P1 }
      startangle := heading1 - Pi/2;
      { determine angle from center of P1c }
      endangle := anglefromdelta( center - intersect );
      normalizeangles;
      calculateStepInfo;
      validarcintersect := true;
      arcintersect := intersect;
    end;
 end;

procedure TCurveStepInfo.normalizeangles;
 { normalizes start/end angle based on direction }
 begin
   { insure end angle and start angle are 'together' according to dir }
   if dir = -1 then
    begin
      if endangle > startangle then
         endangle := endangle - 2 * Pi;
    end
   else
    begin
      endangle := endangle - Pi; {why is this kludge necessary?? }
      if endangle < startangle then
         startangle := startangle - 2 * Pi;
    end;
   { normalize start angle to -Pi..+Pi and endangle will be whatever it adjusts to }
   if startangle < -Pi then
    begin
      startangle := startangle + 2*Pi;
      endangle := endangle + 2*Pi;
    end
   else
   if startangle > Pi then
    begin
      startangle := startangle - 2*Pi;
      endangle := endangle - 2*Pi;
    end;
 end;

procedure TCurveStepInfo.calculateStepInfo;
 var l : single;
     const stepsperunit = 0.33;
 begin
   anglestep :=  endangle - startangle;
   l := abs( anglestep * radius );
   steps := ceil( l / stepsperunit ); { 0.5 units per step }

   { determine number of steps to use in the curve }
(*   steps := trunc( abs( anglestep ) / Pi * 180 / 10 )+1; { every 10 deg }*)
   anglestep := anglestep / steps;
 end;


procedure TCurveStepInfo.setLR( var pL, pR : TVector3;
                                Offset : single );
 var r2 : single;
 begin
   r2 := radius + offset;
   PL := Vector3( center.x - dir * Fcosa * r2, 0, center.y - dir * Fsina * r2 );
   r2 := radius - offset;
   PR := Vector3( center.x - dir * Fcosa * r2, 0, center.y - dir * Fsina * r2 );
 end;


end.

