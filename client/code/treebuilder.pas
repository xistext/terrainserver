unit TreeBuilder;

{ build Tree Model based on LOD and class }

interface

uses
   Classes, SysUtils,
   CastleVectors, CastleTransform, CastleBehaviors, CastleScene,
   x3dNodes, x3dtools;

type TTreeBuilder = class

        function BuildTree( aowner : TComponent;
                            var pos : TVector3;
                            LOD : integer = 1 ) : TCastleTransform;
        function init2Plane( aowner : TComponent;
                             texurl : string;
                             height : single ) : TCastleTransform;
        function initPlaneBillboard( aowner : TComponent;
                                     texurl : string;
                                     height : single) : TCastleTransform;
        function initColorTriangleBillboard( aowner : TComponent;
                                             texurl : string;
                                             height : single ) : TCastleTransform;
      end;


const GTreeBuilder : TTreeBuilder = nil;

implementation

function TTreeBuilder.initPlaneBillboard( aowner : TComponent;
                                          texurl : string;
                                          height : single ) : TCastleTransform;

 var g : TCastlePlane;
     b : TCastleBillBoard;
 begin
   g := TCastlePlane.Create(aowner);
   g.DoubleSided := true;
   g.Texture := 'castle-data:/testtree.png';
   g.Size := Vector2( height, height);
   g.Axis := 2;
   g.translation := vector3( 0, g.Size.Y * 0.5, 0 );
   result.add( g );
   b  := TCastleBillboard.Create( g );
   b.AxisOfRotation := vector3( 0, 1, 0 );
   g.AddBehavior( b );

   result := g;
 end;

function TTreeBuilder.init2Plane( aowner : TComponent;
                                  texurl : string;
                                  height : single ) : TCastleTransform;

 var g : TCastlePlane;
     b : TCastleBillBoard;
 begin
   Result := TCastleTransform.Create( aowner );
   g := TCastlePlane.Create(aowner);
   g.DoubleSided := true;
   g.Texture := 'castle-data:/testtree.png';
   g.Size := Vector2( height, height);
   g.Axis := 2;
   g.translation := vector3( 0, g.Size.Y * 0.5, 0 );
   result.add( g );

   g := TCastlePlane.Create(aowner);
   g.DoubleSided := true;
   g.Texture := 'castle-data:/testtree.png';
   g.Size := Vector2( height, height);
   g.Axis := 2;
   g.Direction := vector3( 1, 0, 0 );
   g.translation := vector3( 0, g.Size.Y * 0.5, 0 );
   result.add( g );
 end;

(*
procedure TLiteMesh.InitVertices( Coord : TCoordinateNode );
 var Vertices  : TVector3List;
     step, sz2 : single;
     i, j, vcount : integer;
     VertexPtr : ^TVector3;
     Vertex : TVector3;
     GCount : integer;
     aOffset : TVector2;
 begin
   aoffset := offset;
   step := gridstep;
   GCount := GridCount;
   vcount := GCount * GCount;
   Vertices := TVector3List.Create;
   Vertices.Count := vcount;
   VertexPtr := Vertices.Ptr(0);
   sz2 := CellCount * Step * 0.5;
   vertex.y := 0;
   vertex.z := aoffset.y-sz2;
   for i := 0 to GCount - 1 do
    begin
      Vertex.x := aoffset.x-sz2; { world x offset to align tiles }
      for j := 0 to GCount - 1 do
       begin
         VertexPtr^ := Vertex;
         vertex.x := vertex.x + step;
         inc( vertexptr );
       end;
      vertex.z := vertex.z + step;
    end;
   Coord.SetPoint( Vertices );
   Vertices.Free;
 end;
  *)

function TTreeBuilder.initColorTriangleBillboard( aowner : TComponent;
                                                  texurl : string;
                                                  height : single ) : TCastleTransform;
 var g : TCastleScene;
     b : TCastleBillBoard;
     Vertices : TVector3List;
     Appearance : TAppearanceNode;
     Root : TX3dRootNode;
     Triangles : TTriangleSetNode;
     Shape : TShapeNode;
 begin
   Triangles := TTriangleSetNode.Create;
   Triangles.Coord := TCoordinateNode.Create;

   Vertices := TVector3List.Create;
   Vertices.Count := 3;
   Vertices.Items[0] := vector3( 0, height, 0 );
   Vertices.Items[1] := Vector3( -0.5, 0, 0 );
   Vertices.Items[2] := vector3( 0.5, 0, 0 );
   TCoordinateNode( Triangles.coord ).SetPoint( Vertices );
   Vertices.Free;

   Shape := TShapeNode.Create;
   Shape.Geometry := Triangles;

   Appearance := TAppearanceNode.create;
     { make the material lit }
   Appearance.Material := makePhysicalMaterial( vector3( 0.0, 0.5, 0 ));

   Shape.Appearance := Appearance;

   Root := TX3DRootNode.Create;
   Root.AddChildren( Shape );

   g := TCastleScene.Create(aowner);
   g.Load( Root, true );
   b  := TCastleBillboard.Create( g );
   b.AxisOfRotation := vector3( 0, 1, 0 );
   g.AddBehavior( b );
   result := g;
 end;

function TTreeBuilder.BuildTree( aowner : TComponent;
                                 var pos : TVector3;
                                 LOD : integer = 1 ) : TCastleTransform;
 var treesz : single;
 begin
   { plane billboard with transparent texture }
   treesz := random + random;

//   result := initPlaneBillboard( aowner, 'castle-data:/testtree.png', treesz  );
   result := initColorTriangleBillboard( aowner, '', treesz );
   result.Translation := vector3( pos.x, pos.y + result.translation.y, pos.z );
 end;


initialization
  GTreeBuilder := TTreeBuilder.create;
finalization
  GTreeBuilder.Free;
end.

