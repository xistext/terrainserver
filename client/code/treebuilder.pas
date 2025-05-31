unit TreeBuilder;

{ build Tree Model based on LOD and class }

interface

uses
   Classes, SysUtils, Math,
   CastleVectors, CastleTransform, CastleBehaviors, CastleScene, CastleColors, CastleRenderOptions,
   x3dNodes, x3dtools, baselight,
   TerrainObjects;




type TTreeShapeEdge = array of TVector2;

     TTileObj_BuildRec = record
        objtype       : ttileobj_type;
        worldposition : tvector3;
        worldsize     : single;
        LOD           : integer;
      end;

     TTileObj_BuildList = array of TTileObj_BuildRec;

     TBuilder_TileObj = class

        function BuildGraphics( aowner : TComponent;
                                var pos : TVector3;
                                objsize : single;
                                LOD : integer = 1 ) : TCastleTransform; virtual; abstract;
        function BuildSceneFromX3DRoot( aowner : TComponent;
                                        root   : TX3dRootNode ) : TCastleScene;
      end;


     TTreeBuilder = class( TBuilder_TileObj )

      function BuildGraphicsList( aowner    : TComponent;
                                  buildlist : TTileObj_BuildList ) : TCastleTransform;


      function BuildGraphics( aowner : TComponent;
                              var pos : TVector3;
                              objsize : single;
                              LOD : integer = 1 ) : TCastleTransform; override;
      function buildcolorline( count : integer;
                               color : TCastleColorRGB;
                               var Lines : TCoordinateNode;
                               LineWidth : single = 1;
                               LineType  : TLineType = ltSolid ) : TShapeNode;
        private
        function initColorTriangleBillboard( aowner : TComponent;
                                             texurl : string;
                                             height : single ) : TCastleTransform;
        function initTriangleFanBillboard( aowner : TComponent;
                                           texurl : string;
                                           height : single ) : TCastleTransform;
        function buildRotatedEdgeSolid( aowner : TComponent;
                                        texurl : string;
                                        const pos : tvector3;
                                        height : single ) : TShapeNode;
      end;


const GTreeBuilder : TTreeBuilder = nil;

implementation

function TBuilder_TileObj.BuildSceneFromX3DRoot( aowner : TComponent;
                                                 root   : TX3dRootNode ) : TCastleScene;
 begin
   result := TCastleScene.Create(aowner);
   result.RenderOptions.WholeSceneManifold := true;
   result.Load( Root, true );
   result.ReceiveShadowVolumes := false;
   result.CastShadowVolumes := false;
   result.CastShadows := true;
   result.RenderOptions.Blending := false;
 end;

function TTreeBuilder.initColorTriangleBillboard( aowner : TComponent;
                                                  texurl : string;
                                                  height : single ) : TCastleTransform;
 var b : TCastleBillBoard;
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

   result := BuildSceneFromX3dRoot( aowner, Root );
   b  := TCastleBillboard.Create( result );
   b.AxisOfRotation := vector3( 0, 1, 0 );
   result.AddBehavior( b );
 end;

var gtreeedge : TTreeShapeEdge;

function TTreeBuilder.initTriangleFanBillboard( aowner : TComponent;
                                                texurl : string;
                                                height : single ) : TCastleTransform;
 var b : TCastleBillBoard;
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

   result := buildSceneFromX3dRoot( aowner, root );
   b  := TCastleBillboard.Create( result );
   b.AxisOfRotation := vector3( 0, 1, 0 );
   result.AddBehavior( b );
 end;

function TTreeBuilder.buildcolorline( count : integer;
                                      color : TCastleColorRGB;
                                      var Lines : TCoordinateNode;
                                      LineWidth : single = 1;
                                      LineType  : TLineType = ltSolid ) : TShapeNode;
 var Material: TUnlitMaterialNode;
     Appearance: TAppearanceNode;
     LineSet : TLineSetNode;
     LineProperties : TLinePropertiesNode;
 begin
   Material := makeUnlitMaterial( color );
   Material.Transparency := 0.5;

   LineProperties := TLinePropertiesNode.Create;
   LineProperties.LinewidthScaleFactor := LineWidth;
   LineProperties.LineType := LineType;

   Appearance := TAppearanceNode.Create;
   Appearance.Material := Material;
   Appearance.LineProperties := LineProperties;

   LineSet := TLineSetNode.CreateWithShape(Result);
   LineSet.mode := lmStrip;
   LineSet.SetVertexCount(count);

   Result.Appearance := Appearance;
   Lines := TCoordinateNode.Create;
   Lines.FdPoint.Items.Count := count;
   LineSet.Coord := Lines;
 end;


function TTreeBuilder.buildRotatedEdgeSolid( aowner : TComponent;
                                             texurl : string;
                                             const pos : tvector3;
                                             height : single ) : TShapeNode;
var Triangles : TTriangleFanSetNode;
    TexCoords : TVector2List;
    Vertices : TVector3List;
    Root : TX3dRootNode;
    Shape : TShapeNode;
    h, sn, cn : single;
    r1, a, d : single;
    i : integer;
    Lines : TCoordinateNode;
    trunkshape : TShapeNode;
 const sides : integer = 5;
 begin
   Triangles := TTriangleFanSetNode.Create;
   Triangles.Solid := false;
   Triangles.SetFanCount([2+sides]);
   Triangles.Coord := TCoordinateNode.Create;
   Triangles.TexCoord := TTextureCoordinateNode.Create;
   TexCoords := TVector2List.Create;
   TexCoords.Count := 2 + sides;
   Vertices := TVector3List.Create;
   Vertices.Count := 2 + sides;
   h := height;
   r1 := height / 2;

   Vertices[0] := vector3( pos.x, pos.y + h, pos.z ); // center of triangle fan
   a := 0;
   d := 2 * Pi / sides;
   for i := 0 to sides - 1 do
    begin
      sincos( a, sn, cn );
      Vertices[i+1] := Vector3( pos.x + r1 * sn, pos.y, pos.z + r1 * cn );
      a := a + d;
    end;
   Vertices[sides+1] := Vertices[1];

   TexCoords.Items[0] := vector2( 0.5, 1 ); // center of trangle fan
   a := 0;
   d := 1 / sides;
   for i := 0 to sides - 1 do
    begin
      TexCoords.Items[i+1] := vector2( a, 0.15 );
      a := a + d;
    end;
   TexCoords[sides+1] := TexCoords[1];

   TCoordinateNode( Triangles.coord ).SetPoint( Vertices );
   TTextureCoordinateNode( Triangles.TexCoord ).SetPoint( TexCoords );
   Vertices.Free;
   TexCoords.Free;

   Shape := TShapeNode.Create;
   Shape.Geometry := Triangles;
   addtexture( Shape, 'castle-data:/testtree.png' );
   Result := Shape;
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



const clonetrees : boolean = false;
      prototree : TCastleTransform = nil;

function TTreeBuilder.BuildGraphics( aowner : TComponent;
                                     var pos : TVector3;
                                     objsize : single;
                                     LOD : integer = 1 ) : TCastleTransform;
 var shape : TShapeNode;
     root : TX3drootnode;
 begin
//   result := initPlaneBillboard( aowner, 'castle-data:/testtree.png', objsize );
//   result := initColorTriangleBillboard( aowner, 'castle-data:/testtree.png', objsize );
//   result := initTriangleFanBillboard( aowner, 'castle-data:/testtree.png', objsize );
(*   if clonetrees then
    begin
      if not assigned( prototree ) then
          prototree := buildRotatedEdgeSolid( aowner, 'castle-data:/testtree.png', 1 );
      g := TCastleTransformReference.Create( aowner );
      TCastleTransformReference( g ).reference := prototree;
    end
   else*)
(*   shape := buildRotatedEdgeSolid( aowner, 'castle-data:/testtree.png', vector3( 0, 0, 0 ));
   Root := TX3DRootNode.Create;
   Root.AddChildren( shape );

   result := buildSceneFromX3dRoot( aowner, root );*)
 end;

function TTreeBuilder.BuildGraphicsList( aowner    : TComponent;
                                         buildlist : TTileObj_BuildList ) : TCastleTransform;
 var i, c : integer;
     shape : TShapeNode;
     BuildRec : TTileObj_BuildRec;
     Root : TX3dRootNode;
     Light : TDirectionalLightNode;
 begin
   c := length( buildlist );
   if c > 0 then
    begin
      Root := TX3DRootNode.Create;
      for i := 0 to c - 1 do
       begin
         BuildRec := buildlist[i];
         shape := buildRotatedEdgeSolid( aowner, 'castle-data:/testtree.png', buildrec.WorldPosition, buildrec.WorldSize );
         shape.Appearance.ShadowCaster := true ;
         Root.AddChildren( Shape );
       end;
      Light := TDirectionalLightNode.Create;
      Light.direction := GLightDirection;
      Light.Intensity := 10;
      Root.AddChildren( Light );
      result := buildSceneFromX3dRoot( aowner, root );
    end;

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

