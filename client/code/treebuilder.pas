unit TreeBuilder;

{ build Tree Model based on LOD and class }

interface

uses
   Classes, SysUtils,
   CastleVectors, CastleTransform, CastleBehaviors, CastleScene,
   x3dNodes, x3dtools,
{   TerrainData,} TerrainObjects;




type TTreeShapeEdge = array of TVector2;

     TBuilder_TileObj = class

        function BuildGraphics( aowner : TComponent;
                                var pos : TVector3;
                                LOD : integer = 1 ) : TCastleTransform; virtual; abstract;
      end;


     TTreeBuilder = class( TBuilder_TileObj )

      function BuildGraphics( aowner : TComponent;
                                var pos : TVector3;
                                LOD : integer = 1 ) : TCastleTransform; override;
        private
        function initColorTriangleBillboard( aowner : TComponent;
                                             texurl : string;
                                             height : single ) : TCastleTransform;
        function initTriangleFanBillboard( aowner : TComponent;
                                           texurl : string;
                                           height : single ) : TCastleTransform;
      end;


const GTreeBuilder : TTreeBuilder = nil;

implementation

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
   TPhysicalMaterialNode( Appearance.Material ).Roughness := 0.9;
   if texurl <> '' then
    begin
      Appearance.Texture := TImageTextureNode.Create;
//      Appearance.AlphaMode := amOpaque;
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

var gtreeedge : TTreeShapeEdge;

function TTreeBuilder.initTriangleFanBillboard( aowner : TComponent;
                                                texurl : string;
                                                height : single ) : TCastleTransform;
 var g : TCastleScene;
     b : TCastleBillBoard;
     Vertices : TVector3List;
     Root : TX3dRootNode;
     Triangles : TTriangleFanSetNode;
     Shape : TShapeNode;
     TexCoords : TVector2List;
     h, h2 : single;
     r1 : single;
 begin
   Triangles := TTriangleFanSetNode.Create;
   Triangles.SetFanCount([10]);
   Triangles.Coord := TCoordinateNode.Create;
   Triangles.TexCoord := TTextureCoordinateNode.Create;
   TexCoords := TVector2List.Create;
   TexCoords.Count := 10;
   Vertices := TVector3List.Create;
   Vertices.Count := 10;
   h := height;
   h2 := height * 0.3;

   r1 := random*0.1;
   Vertices[0] := vector3( 0, h2, 0 ); // center of trangle fan
   vertices[1] := vector3( -gtreeedge[0].x * h - r1, gtreeedge[0].y * h, 0 );
   vertices[2] := vector3( gtreeedge[0].x * h + random*0.1, gtreeedge[0].y, 0 );
   vertices[3] := vector3( gtreeedge[1].x * h + random*0.1, gtreeedge[1].y * h + random*0.1, 0 );
   vertices[4] := vector3( gtreeedge[2].x * h + random*0.1, gtreeedge[2].y * h + random*0.1, 0 );
   vertices[5] := vector3( gtreeedge[3].x * h + random*0.1, gtreeedge[3].y * h - random*0.2*h, 0 );   // peak
   vertices[6] := vector3( -gtreeedge[3].x * h - random*0.1, gtreeedge[3].y * h -random*0.2, 0 );   // peak
   vertices[7] := vector3( -gtreeedge[2].x * h - random*0.1, gtreeedge[2].y * h + random*0.1, 0 );
   vertices[8] := vector3( -gtreeedge[1].x * h - random*0.1, gtreeedge[1].y * h + random*0.1, 0 );
   vertices[9] := vector3( -gtreeedge[0].x * h - r1, gtreeedge[0].y * h, 0 );

   r1 := random*0.2-0.1;
   TexCoords.Items[0] := vector2( 0.5+r1, 0.3 ); // center of trangle fan
   TexCoords.Items[1] := Vector2( 0.5+random*0.2-0.1 + gtreeedge[0].x, gtreeedge[0].y ); // center of triangle fan
   TexCoords.Items[2] := Vector2( 0.5+random*0.2-0.1 - gtreeedge[0].x, gtreeedge[0].y );
   TexCoords.Items[3] := Vector2( 0.5+random*0.2-0.1 - gtreeedge[1].x, gtreeedge[1].y );
   TexCoords.Items[4] := Vector2( 0.5+random*0.2-0.1 - gtreeedge[2].x, gtreeedge[2].y );
   TexCoords.Items[5] := Vector2( 0.5-random*0.3 - gtreeedge[3].x, gtreeedge[3].y-random*0.2 ); // peak
   TexCoords.Items[6] := Vector2( 0.5+random*0.3 + gtreeedge[3].x, gtreeedge[3].y-random*0.2 ); // peak
   TexCoords.Items[7] := Vector2( 0.5+random*0.2-0.1 + gtreeedge[2].x, gtreeedge[2].y );
   TexCoords.Items[8] := Vector2( 0.5+random*0.2-0.1 + gtreeedge[1].x, gtreeedge[1].y );
   TexCoords.Items[9] := Vector2( 0.5+r1+ gtreeedge[0].x, gtreeedge[0].y );

   TCoordinateNode( Triangles.coord ).SetPoint( Vertices );
   TTextureCoordinateNode( Triangles.TexCoord ).SetPoint( TexCoords );
   Vertices.Free;
   TexCoords.Free;

   Shape := TShapeNode.Create;
   Shape.Geometry := Triangles;
   addtexture( Shape, 'castle-data:/testtreetexture.png' );

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
   g.ReceiveShadowVolumes := false;
   result := g;
 end;


{ lod levels
    lowest
    1 color triangle billboard, no shadows
    1 texture triangle billboard, no shadows
    texture triangle fan strip billboard, no shadows
    texture triangle fan strip billboard, shadows
    4 texture triangle fan strips crossed, shadows
    extreme low lod solid model, shadows


  }




function TTreeBuilder.BuildGraphics( aowner : TComponent;
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
  { basic triangular 'pine' tree edge shape }
  gtreeedge := [vector2(1/28, 0),   { trunk base }
                vector2(1/28, 0.14), { trunk top }
                vector2(0.33, 0.33),  { widest part of folliage }
                vector2(0, 1 )];     { foliage top }
  GTreeBuilder := TTreeBuilder.create;
finalization
  GTreeBuilder.Free;
end.

