unit layermarkup;

interface

uses
  Classes, SysUtils, math,
  castlevectors, castlecolors, castlescene, castlerenderoptions, castlerectangles, castletransform,
  castlebehaviors,
  x3dnodes, castlefonts,
  BaseTools, curvetools, x3dtools;

const defaultheightaboveterrain = 0.1;

type  TMarkupLayer = class( TCastleTransform )

         constructor create( AOwner : TComponent ); override;

         procedure clear;

         public

         heightaboveterrain : single;
         terrainheight : THeightAboveTerrainEvent;

         procedure addmark( const markcenter : TVector2;
                            radius : single;
                            color : TCastleColorRGB;
                            LineWidth : single = 1;
                            LineType  : TLineType = ltSolid ); overload;
         procedure addmark( const markcenter : TVector3;
                            radius : single;
                            color : TCastleColorRGB;
                            LineWidth : single = 1;
                            LineType  : TLineType = ltSolid ); overload;
         procedure addarc( const arccenter : TVector2;
                           radius : single;
                           startangle : single;
                           deltaangle : single;
                           steps : integer;
                           color : TCastleColorRGB;
                           LineWidth : single = 1;
                           LineType  : TLineType = ltSolid );

         procedure addseg( const pos1, pos2 : TVector2;
                           color : TCastleColorRGB;
                           LineWidth : single = 1;
                           LineType  : TLineType = ltSolid );
         procedure addseg( const pos1, pos2 : TVector3;
                           color : TCastleColorRGB;
                           LineWidth : single = 1;
                           LineType  : TLineType = ltSolid );
         procedure addlabel( const pos : TVector2;
                             texttodisplay : string;
                             const color : TVector4;
                             size : single = 0.1;
                             font : tcastlefont = nil;
                             billboard : boolean = false ); overload;
         procedure addlabel( const pos : TVector3;
                             texttodisplay : string;
                             const color : TVector4;
                             size : single = 0.1;
                             font : tcastlefont = nil;
                             billboard : boolean = false ); overload;

         procedure addpoly2( poly : TPoly2;
                             color : TCastleColorRGB;
                             LineWidth : single = 1;
                             LineType  : TLineType = ltSolid );
         function addx3dNode( x3dnode : TX3dRootNode ) : TCastleScene;

        end;

const GMarkupLayer : TMarkupLayer = nil;

implementation //===============================================================

constructor TMarkupLayer.create( AOwner : TComponent );
 begin
   inherited;
   heightaboveterrain := defaultheightaboveterrain;
   Collides := false;
   Pickable := false;
//   RenderLayer := rlFront;
   terrainheight := nil;
 end;

procedure TMarkupLayer.clear;
 begin
   { inherited clear doesn't free the children }
   while count > 0 do
      items[0].Free;
 end;

function TMarkupLayer.addx3dNode( x3dnode : TX3dRootNode ) : TCastleScene;
 begin
   Result := TCastleScene.Create( self );
   Result.Pickable := false;
   Result.Collides := false;
   Result.Load( x3dnode, true );
   Add( Result );
 end;

procedure TMarkupLayer.addarc( const arccenter : TVector2;
                               radius : single;
                               startangle : single;
                               deltaangle : single;
                               steps : integer;
                               color : TCastleColorRGB;
                               LineWidth : single = 1;
                               LineType  : TLineType = ltSolid );
 var newnode : TX3dRootNode;
     Lines : TCoordinateNode;
     angle, stepdelta : single;
     i, dir : integer;
     pt2 : TVector2;
     pt3 : TVector3;
 begin
   newnode := buildcolorline( steps + 1, color, Lines, LineWidth, LineType );
   angle := startangle;
   stepdelta := deltaangle / steps;
   dir := sign( deltaangle );
   for i := 0 to steps do
    begin
      pt2 := _ptatangle( arccenter, radius, angle, dir );
      pt3 := Vector3( pt2.x, 0, pt2.y );
      if assigned( terrainheight ) then
         terrainheight( pt3, pt3.y );
      pt3.y := pt3.y + heightaboveterrain;
      Lines.FdPoint.Items.L[i] := pt3;
      angle := angle + stepdelta;
    end;
   Addx3dNode( newnode );
 end;


procedure TMarkupLayer.addmark( const markcenter : TVector3;
                                radius : single;
                                color : TCastleColorRGB;
                                LineWidth : single = 1;
                                LineType  : TLineType = ltSolid );
var newnode : TX3dRootNode;
     Lines : TCoordinateNode;
     pt3 : TVector3;
 begin
   newnode := buildcolorline( 8, color, Lines, LineWidth, LineType );

   pt3 := Vector3( markcenter.x - radius, markcenter.y, markcenter.z );
   Lines.FdPoint.Items.L[0] := pt3;

   pt3 := Vector3( markcenter.x + radius, markcenter.y, markcenter.z );
   Lines.FdPoint.Items.L[1] := pt3;

   pt3 := Vector3( markcenter.x, markcenter.y, markcenter.z );
   Lines.FdPoint.Items.L[2] := pt3;

   pt3 := Vector3( markcenter.x, markcenter.y, markcenter.z - radius );
   Lines.FdPoint.Items.L[3] := pt3;

   pt3 := Vector3( markcenter.x, markcenter.y, markcenter.z + radius );
   Lines.FdPoint.Items.L[4] := pt3;

   pt3 := Vector3( markcenter.x, markcenter.y, markcenter.z );
   Lines.FdPoint.Items.L[5] := pt3;

   pt3 := Vector3( markcenter.x, markcenter.y - radius, markcenter.z );
   Lines.FdPoint.Items.L[6] := pt3;

   pt3 := Vector3( markcenter.x, markcenter.y + radius, markcenter.z );
   Lines.FdPoint.Items.L[7] := pt3;

   AddX3dNode( newnode );
  end;

procedure TMarkupLayer.addmark( const markcenter : TVector2;
                                radius : single;
                                color : TCastleColorRGB;
                                LineWidth : single = 1;
                                LineType  : TLineType = ltSolid );
 var newnode : TX3dRootNode;
     Lines : TCoordinateNode;
     pt3 : TVector3;
 begin
   newnode := buildcolorline( 5, color, Lines, LineWidth, LineType );

   pt3 := Vector3( markcenter.x - radius, 0, markcenter.y );
   if assigned( terrainheight ) then
      terrainheight( pt3, pt3.y );
   pt3.y := pt3.y + heightaboveterrain;
   Lines.FdPoint.Items.L[0] := pt3;

   pt3 := Vector3( markcenter.x + radius, 0, markcenter.y );
   if assigned( terrainheight ) then
      terrainheight( pt3, pt3.y );
   pt3.y := pt3.y + heightaboveterrain;
   Lines.FdPoint.Items.L[1] := pt3;

   pt3 := Vector3( markcenter.x, 0, markcenter.y );
   if assigned( terrainheight ) then
      terrainheight( pt3, pt3.y );
   pt3.y := pt3.y + heightaboveterrain;
   Lines.FdPoint.Items.L[2] := pt3;

   pt3 := Vector3( markcenter.x, 0, markcenter.y - radius );
   if assigned( terrainheight ) then
      terrainheight( pt3, pt3.y );
   pt3.y := pt3.y + heightaboveterrain;
   Lines.FdPoint.Items.L[3] := pt3;

   pt3 := Vector3( markcenter.x, 0, markcenter.y + radius );
   if assigned( terrainheight ) then
      terrainheight( pt3, pt3.y );
   pt3.y := pt3.y + heightaboveterrain;
   Lines.FdPoint.Items.L[4] := pt3;

   AddX3dNode( newnode );
 end;

procedure TMarkupLayer.addseg( const pos1, pos2 : TVector2;
                               color : TCastleColorRGB;
                               LineWidth : single = 1;
                               LineType  : TLineType = ltSolid );
 var newnode : TX3dRootNode;
     Lines : TCoordinateNode;
     pt3 : TVector3;
 begin
   newnode := buildcolorline( 2, color, Lines, LineWidth, LineType );

   pt3 := Vector3( pos1.x, 0, pos1.y );
   if assigned( terrainheight ) then
      terrainheight( pt3, pt3.y );
   pt3.y := pt3.y + heightaboveterrain;
   Lines.FdPoint.Items.L[0] := pt3;

   pt3 := Vector3( pos2.x, 0, pos2.y );
   if assigned( terrainheight ) then
      terrainheight( pt3, pt3.y );
   pt3.y := pt3.y + heightaboveterrain;
   Lines.FdPoint.Items.L[1] := pt3;

   AddX3dNode( newnode );
 end;

procedure TMarkupLayer.addseg( const pos1, pos2 : TVector3;
                               color : TCastleColorRGB;
                               LineWidth : single = 1;
                               LineType  : TLineType = ltSolid );
 var newnode : TX3dRootNode;
     Lines : TCoordinateNode;
     pt3 : TVector3;
 begin
   newnode := buildcolorline( 2, color, Lines, LineWidth, LineType );

   pt3 := pos1;
   Lines.FdPoint.Items.L[0] := pt3;

   pt3 := pos2;
   Lines.FdPoint.Items.L[1] := pt3;

   AddX3dNode( newnode );
 end;


procedure TMarkupLayer.addlabel( const pos : TVector2;
                                 texttodisplay : string;
                                 const color : TVector4;
                                 size : single = 0.1;
                                 font : TCastleFont = nil;
                                 billboard : boolean = false );
 var g : TCastleText;
     g1 : TCastleTransform;
     pt3 : TVector3;
     abillboard : TCastleBillboard;
 { text lies on ground and always orients toward camera }
 begin
   g := TCastleText.Create( self );
   if assigned( font ) then
      g.CustomFont := font;
   pt3 := Vector3( pos.x, 0, pos.y );
   if assigned( terrainheight ) then
      terrainheight( pt3, pt3.y, ht_road );
   pt3.y := pt3.y + heightaboveterrain;
   g.Color := color;
   g.Caption := texttodisplay;
   g.rotation := Vector4( 1, 0, 0, -Pi/2 );
   g.Size := size;
   g.VerticalAlignment := vpMiddle;
   g.Alignment := hpMiddle;

   g1 := TCastleTransform.Create( self );
   g1.Translation := pt3;
   g1.Add( g );
   if billboard then
    begin
      abillboard := TCastlebillboard.create( self );
      abillboard.AxisOfRotation := Vector3( 0, 1, 0 );
      g1.AddBehavior( abillboard );
    end;
   Add( g1 );
 end;

procedure TMarkupLayer.addlabel( const pos : TVector3;
                                 texttodisplay : string;
                                 const color : TVector4;
                                 size : single = 0.1;
                                 font : tcastlefont = nil;
                                 billboard : boolean = false );
 var g : TCastleText;
     g1 : TCastleTransform;
     pt3 : TVector3;
     abillboard : TCastleBillboard;
 { text lies on ground and always orients toward camera }
 begin
   g := TCastleText.Create( self );
   if assigned( font ) then
      g.CustomFont := font;
   pt3 := pos;
   g.Color := color;
   g.Caption := texttodisplay;
//   g.rotation := Vector4( 1, 0, 0, -Pi/2 );
   g.Size := size;
   g.VerticalAlignment := vpMiddle;
   g.Alignment := hpMiddle;

   g1 := TCastleTransform.Create( self );
   g1.Translation := pt3;
   g1.Add( g );
   if billboard then
    begin
      abillboard := TCastlebillboard.create( self );
      abillboard.AxisOfRotation := Vector3( 0, 1, 0 );
      g1.AddBehavior( abillboard );
    end;
   Add( g1 );
 end;


procedure TMarkupLayer.addpoly2( poly : TPoly2;
                                 color : TCastleColorRGB;
                                 LineWidth : single = 1;
                                 LineType  : TLineType = ltSolid );
 var newnode : TX3dRootNode;
     Lines : TCoordinateNode;
     pt2 : TVector2;
     pt3 : TVector3;
     i, cnt : integer;
 begin
   cnt := length( poly );
   newnode := buildcolorline( cnt+1, color, Lines, LineWidth, LineType );
   for i := 0 to cnt - 1 do
    begin
      pt2 := poly[i];
      pt3 := Vector3( pt2.x, 0, pt2.y );
      if assigned( terrainheight ) then
         terrainheight( pt3, pt3.y );
      pt3.y := pt3.y + heightaboveterrain;
      Lines.FdPoint.Items.L[i] := pt3;
    end;
   pt2 := poly[0];
   pt3 := Vector3( pt2.x, 0, pt2.y );
   if assigned( terrainheight ) then
      terrainheight( pt3, pt3.y );
   pt3.y := pt3.y + heightaboveterrain;
   Lines.FdPoint.Items.L[cnt] := pt3;
   Addx3dNode( newnode );
 end;


end.

