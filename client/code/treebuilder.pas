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
        function initTriangleFanBillboard( aowner : TComponent;
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
     TexCoords : TVector2List;
     h2 : single;
 begin
   Triangles := TTriangleSetNode.Create;
   Triangles.Coord := TCoordinateNode.Create;
   Triangles.TexCoord := TTextureCoordinateNode.Create;
   TexCoords := TVector2List.Create;
   TexCoords.Count := 3;
   Vertices := TVector3List.Create;
   Vertices.Count := 3;
   h2 := height * 0.3;
   Vertices.Items[0] := vector3( 0, height, 0 );
   Vertices.Items[1] := Vector3( -h2, 0, 0 );
   Vertices.Items[2] := vector3( h2, 0, 0 );
   TexCoords.Items[0] := vector2( 0.5, 1 );
   if random(2)=1 then { randomly flip texture }
    begin
      TexCoords.Items[1] := Vector2( 0, 0 );
      TexCoords.Items[2] := Vector2( 1, 0 );
    end
   else
    begin
      TexCoords.Items[1] := Vector2( 1, 0 );
      TexCoords.Items[2] := Vector2( 0, 0 );
    end;
   TCoordinateNode( Triangles.coord ).SetPoint( Vertices );
   TTextureCoordinateNode( Triangles.TexCoord ).SetPoint( TexCoords );
   Vertices.Free;
   TexCoords.Free;

   Shape := TShapeNode.Create;
   Shape.Geometry := Triangles;

   Appearance := TAppearanceNode.create;
   Appearance.Material := makePhysicalMaterial( vector3( 0.0, 0.5, 0 ));

   if texurl <> '' then
    begin
      Appearance.Texture := TImageTextureNode.Create;
 //     Appearance.AlphaMode := amOpaque;
      TImageTextureNode( Appearance.Texture ).SetUrl( ['castle-data:/testtree.png'] );
    end;

   Shape.Appearance := Appearance;

   Root := TX3DRootNode.Create;
   Root.AddChildren( Shape );

   g := TCastleScene.Create(aowner);
//   g.castshadowvolumes:= true;
   g.RenderOptions.WholeSceneManifold := true;
   g.Load( Root, true );
   b  := TCastleBillboard.Create( g );
   b.AxisOfRotation := vector3( 0, 1, 0 );
   g.AddBehavior( b );
   result := g;
 end;

function TTreeBuilder.initTriangleFanBillboard( aowner : TComponent;
                                                texurl : string;
                                                height : single ) : TCastleTransform;
 var g : TCastleScene;
     b : TCastleBillBoard;
     Vertices : TVector3List;
     Appearance : TAppearanceNode;
     Root : TX3dRootNode;
     Triangles : TTriangleFanSetNode;
     Shape : TShapeNode;
     TexCoords : TVector2List;
     h2 : single;
     trunkw : single;
     trunkh : single;
     maintreew : single;
 begin
   Triangles := TTriangleFanSetNode.Create;
   Triangles.SetFanCount([9]);
   Triangles.Coord := TCoordinateNode.Create;
   Triangles.TexCoord := TTextureCoordinateNode.Create;
   TexCoords := TVector2List.Create;
   TexCoords.Count := 9;
   Vertices := TVector3List.Create;
   Vertices.Count := 9;
   h2 := height * 0.3;
   trunkw := height / 40;
   trunkh := height / 6;
   maintreew := height / 3;

   Vertices[0] := vector3( 0, h2, 0 ); // center
   vertices[1] := vector3( -trunkw, 0, 0 );
   vertices[2] := vector3( trunkw, 0, 0 );
   vertices[3] := vector3( trunkw, trunkh, 0 );
   vertices[4] := vector3( maintreew, trunkh, 0 );
   vertices[5] := vector3( 0, height, 0 );   // peak
   vertices[6] := vector3( -maintreew, trunkh, 0 );
   vertices[7] := vector3( -trunkw, trunkh, 0 );
   vertices[8] := vector3( -trunkw, 0, 0 );

   TexCoords.Items[0] := Vector2( 0.5, 0.3 ); // center
   TexCoords.Items[1] := Vector2( 0.52, 0 );
   TexCoords.Items[2] := Vector2( 0.48, 0 );
   TexCoords.Items[3] := Vector2( 0.48, 0.02 );
   TexCoords.Items[4] := Vector2( 0.25, 0.02 );
   TexCoords.Items[5] := Vector2( 0.5, 1 ); // peak
   TexCoords.Items[6] := Vector2( 0.75, 0.02 );
   TexCoords.Items[7] := Vector2( 0.52, 0.02 );
   TexCoords.Items[8] := Vector2( 0.52, 0 );

   TCoordinateNode( Triangles.coord ).SetPoint( Vertices );
   TTextureCoordinateNode( Triangles.TexCoord ).SetPoint( TexCoords );
   Vertices.Free;
   TexCoords.Free;

   Shape := TShapeNode.Create;
   Shape.Geometry := Triangles;

   Appearance := TAppearanceNode.create;
   Appearance.Material := makePhysicalMaterial( vector3( 0.0, 0.5, 0 ));

   if texurl <> '' then
    begin
      Appearance.Texture := TImageTextureNode.Create;
 //     Appearance.AlphaMode := amOpaque;
      TImageTextureNode( Appearance.Texture ).SetUrl( ['castle-data:/testtree.png'] );
    end;

   Shape.Appearance := Appearance;

   Root := TX3DRootNode.Create;
   Root.AddChildren( Shape );

   g := TCastleScene.Create(aowner);
//   g.castshadowvolumes:= true;
   g.RenderOptions.WholeSceneManifold := true;
   g.Load( Root, true );
  b  := TCastleBillboard.Create( g );
   b.AxisOfRotation := vector3( 0, 1, 0 );
   g.AddBehavior( b );
//   g.RenderOptions.WireframeEffect := weSolidWireframe;
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
//   result := initColorTriangleBillboard( aowner, 'castle-data:/testtree.png', treesz );
   result := initTriangleFanBillboard( aowner, 'castle-data:/testtree.png', treesz );
   result.Translation := vector3( pos.x, pos.y + result.translation.y, pos.z );
   result.CastShadows := true;
 end;


initialization
  GTreeBuilder := TTreeBuilder.create;
finalization
  GTreeBuilder.Free;
end.

