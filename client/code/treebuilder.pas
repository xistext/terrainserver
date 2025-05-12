unit TreeBuilder;

{ build Tree Model based on LOD and class }

interface

uses
   Classes, SysUtils,
   CastleVectors, CastleTransform, CastleBehaviors, CastleScene;

type TTreeBuilder = class

        function BuildTree( aowner : TComponent;
                            var pos : TVector3;
                            LOD : integer ) : TCastleTransform;

      end;


const GTreeBuilder : TTreeBuilder = nil;

implementation

function TTreeBuilder.BuildTree( aowner : TComponent;
                                 var pos : TVector3;
                                 LOD : integer ) : TCastleTransform;
 var g : TCastlePlane;
     treesz : single;
     b : TCastleBillBoard;
 begin
   g := TCastlePlane.Create(aowner);
   g.Texture := 'castle-data:/testtree.png';
   treesz := random + random;
   g.Size := Vector2( treesz, treesz );
   g.Axis := 2;
   pos.y := pos.y + g.Size.Y * 0.5;
   g.Translation := pos;
   b  := TCastleBillboard.Create( g );
   b.AxisOfRotation := vector3( 0, 1, 0 );
   g.AddBehavior( b );
   Result := g;
 end;


initialization
  GTreeBuilder := TTreeBuilder.create;
finalization
  GTreeBuilder.Free;
end.

