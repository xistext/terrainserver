unit basemesh;

interface

uses
  Classes, SysUtils,
  CastleUtils, CastleVectors, CastleTransform, CastleScene, CastleTriangles,
  x3dNodes, debug;

type

TAbstractMesh = class( TCastleScene )
   public

   constructor create( aowner : TComponent ); override;

   procedure InitializeData; dynamic;
   procedure UpdateMeshProperties; dynamic;

   function offset : TVector2; dynamic;

   protected

   fCoordinateNode : TCoordinateNode;  { maintain refererence to CoordinateNode }

   function initindexes : TInt32List;
   function inittriangles : TIndexedTriangleSetNode;

   function InitAppearance : TAppearanceNode; dynamic;
   function InitMaterial   : TPhysicalMaterialNode; dynamic;

   procedure setGridCount( iGridCount : integer ); dynamic; abstract;
   function getGridCount : integer; dynamic; abstract;

   procedure setGridStep( iGridStep : single ); dynamic; abstract;
   function getGridStep : single; dynamic; abstract;

   function getcellcount : integer;

   function CoordinateNode : TCoordinateNode;
   public

   procedure InitVertices; dynamic; abstract;

   property GridCount : integer read getGridCount write setGridCount;
   property GridStep  : single read getGridStep write setGridStep;
   property CellCount : integer read getCellCount;

   procedure UpdateGraphics; virtual; abstract;

   function ElevationAtPos( Pos : TVector2;
                            var Elev : single ) : boolean;

   private

   function ElevationAtPos_InternalRayCollision( pos  : TVector2;
                                                 var Elev : single ) : boolean;

 end;

TAbstractTextureMesh = class( TAbstractMesh )
   public

   procedure InitializeData; override;

   procedure InitVertices; override;

end;



TBaseMesh = class( TAbstractMesh )
   public

   constructor create( aowner : TComponent ); override;

   private

   fGridCount  : integer; { vertex count }
   fGridStep   : single;

   procedure setGridCount( iGridCount : integer ); override;
   function getGridCount : integer; override;

   procedure setGridStep( iGridStep : single ); override;
   function getGridStep : single; override;

end;

TTextureMesh = class( TBaseMesh )
   public

   procedure InitializeData; override;

   function inittexture( TextureUrl : string ) : TImageTextureNode;

   procedure InitVertices; override;

   protected

   TexCoordNode   : TTextureCoordinateNode;

end;

implementation

constructor TAbstractMesh.create( aowner : TComponent );
 begin
   inherited create( aowner );
   fCoordinateNode := nil;
 end;

function TAbstractMesh.getcellcount : integer;
 begin
   result := GridCount - 1;
 end;

function TAbstractMesh.offset : TVector2;
 begin
   result := vector2(0,0);
 end;

function TAbstractMesh.initindexes : TInt32List;
 var X, Z, c : integer;
 begin
   Result := TInt32List.Create;
   c := GridCount;
   for X:= 1 to c - 1 do for Z := 1 to c - 1 do with Result do
    begin
      { triangle1 }
      Add( c * Z + X );
      Add( c * ( Z - 1 ) + X );
      Add( c * ( Z - 1 ) + X - 1 );
      { triangle2 }
      Add( c * Z + X );
      Add( c * ( Z - 1 ) + X - 1 );
      Add( c * Z + X - 1 );
    end;
 end;

function TAbstractMesh.CoordinateNode : TCoordinateNode;
 var IndexedTriangleSetNode : TIndexedTriangleSetNode;
 begin
   result := fCoordinateNode;
//   IndexedTriangleSetNode := TIndexedTriangleSetNode( rootnode.FindNode( TIndexedTriangleSetNode, false ));
//   assert( result = TCoordinateNode( IndexedTriangleSetNode.Coord ));
 end;


function TAbstractMesh.ElevationAtPos_InternalRayCollision( pos  : TVector2;
                                                            var Elev : single ) : boolean;
 var Collision : TRayCollision;
     i : integer;
 begin
   collision := InternalRayCollision( vector3( pos.x, 100, pos.y ), vector3( 0, -1, 0 ));
   result := assigned( collision );
   if result then
     begin
       for i := 0 to collision.count - 1 do with collision[i] do
        begin
          if item = self then
           begin
             elev := Point.Y;
             break;
           end;
        end;
       collision.free;
    end;
 end;

function TAbstractMesh.ElevationAtPos( Pos : TVector2;
                                       var Elev : single ) : boolean;
   function checkpoint( const p : tvector3 ) : boolean;
    const zero : single = 0.01;
    begin
      result := (abs( p.x - pos.x )<=zero) and (abs( p.z -pos.y )<=zero);
      if result then
         elev := p.y;
    end;
   function checktriangle( const triangle : TTriangle3 ) : boolean;
    var intersection : tvector3;
    begin
      {why does this only work on the center tile???}
      result := IsPointWithinTriangle2D( Pos, triangle2( vector2( triangle[0].x, triangle[0].z ),
                                                  vector2( triangle[1].x, triangle[1].z ),
                                                  vector2( triangle[2].x, triangle[2].z )));
      if result then
       begin
         result := TryTriangleRayCollision( Intersection,
                                            Triangle,
                                            Triangle.Plane,
                                            vector3( pos.x, 100, pos.y ), vector3( 0, -1, 0 ));
         if result then
            elev := Intersection.y;
       end;
    end;
 var IndexedTriangleSetNode : TIndexedTriangleSetNode;
     c, ix : integer;
     p0, p1, p2 : tvector3;
     Coord : TCoordinateNode;
 begin
   elev := 0;
   if precisecollisions then
      result := ElevationAtPos_InternalRayCollision( Pos, Elev )
   else
    begin
      result := false;
      IndexedTriangleSetNode := TIndexedTriangleSetNode( rootnode.FindNode( TIndexedTriangleSetNode, false ));
      Coord := TCoordinateNode( IndexedTriangleSetNode.Coord );
      c := IndexedTriangleSetNode.FdIndex.Count;
      ix := 0;
      while ix < c do
       begin
         p0 := translation + Coord.FdPoint.Items[IndexedTriangleSetNode.FdIndex.Items[ix]];
         inc( ix );
         p1 := translation + Coord.FdPoint.Items[IndexedTriangleSetNode.FdIndex.Items[ix]];
         inc( ix );
         p2 := translation + Coord.FdPoint.Items[IndexedTriangleSetNode.FdIndex.Items[ix]];
         inc( ix );
         result := checkpoint( p0 ) or
                   checkpoint( p1 ) or
                   checkpoint( p2 ) or
                   checktriangle( Triangle3( p0, p1, p2 ));
         if result then
            exit;
       end;
    end;
 end;

function TAbstractMesh.InitAppearance : TAppearanceNode;
 begin
   result := TAppearanceNode.create;
     { make the material lit }
   result.Material := initMaterial;
 end;

function TAbstractMesh.initmaterial : TPhysicalMaterialNode;
 begin
   Result := TPhysicalMaterialNode.Create;
 end;

procedure TAbstractMesh.initializedata;
 begin
   fCoordinateNode := TCoordinateNode.Create;
 end;

function TAbstractMesh.inittriangles : TIndexedTriangleSetNode;
 var Indexes : TInt32List;
 begin
   Indexes := initindexes;
   Result := TIndexedTriangleSetNode.Create;
   Result.Coord := CoordinateNode;
   Result.SetIndex( Indexes );
   Indexes.Free;
 end;

procedure TAbstractMesh.UpdateMeshProperties;
 begin
 end;

//--------------------------------------

constructor TBaseMesh.create( aowner : TComponent );
 begin
   inherited create( aowner );
   fGridCount := 0;
   fGridStep := 0;
 end;

procedure TBaseMesh.setGridCount( iGridCount : integer );
 begin
   fGridCount := iGridCount;
 end;

function TBaseMesh.getGridCount : integer;
 begin
   result := fGridCount;
 end;

procedure TBaseMesh.setGridStep( iGridStep  : single );
 begin
   fGridStep := iGridStep;
 end;

function TBaseMesh.getGridStep : single;
 begin
   result := fGridStep;
 end;

//-------------------------------------

procedure TAbstractTextureMesh.initializedata;
 var Root : TX3DRootNode;
     Triangles : TIndexedTriangleSetNode;
     Appearance: TAppearanceNode;
     Shape : TShapeNode;
 begin
   inherited;

   Triangles := initTriangles;
   initvertices;

   Shape := TShapeNode.Create;
   Shape.Geometry := Triangles;

   Appearance := InitAppearance;
   Shape.Appearance := Appearance;

   Root := TX3DRootNode.Create;
   Root.AddChildren( Shape );

   Load(Root, true );
   UpdateMeshProperties;
 end;

procedure TAbstractTextureMesh.InitVertices;
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
   sz2 := gcount * Step * 0.5;
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
   CoordinateNode.SetPoint( Vertices );
   Vertices.Free;
 end;

//---------------------------------

procedure TTextureMesh.InitializeData;
 var Root : TX3DRootNode;
     Triangles : TIndexedTriangleSetNode;
     Appearance: TAppearanceNode;
     Shape : TShapeNode;
 begin
   inherited;
   TexCoordNode := TTextureCoordinateNode.Create;

   Triangles := initTriangles;
   initvertices;

   Shape := TShapeNode.Create;
   Shape.Geometry := Triangles;

   Appearance := InitAppearance;
   Shape.Appearance := Appearance;

   Root := TX3DRootNode.Create;
   Root.AddChildren( Shape );

   { texture }
   Triangles.TexCoord := TexCoordNode;

   Load(Root, true );
   UpdateMeshProperties;
 end;

function TTextureMesh.inittexture( TextureUrl : string ) : TImageTextureNode;
 begin
   Result := TImageTextureNode.Create;
   Result.SetUrl([TextureUrl]);
 end;

procedure TTextureMesh.InitVertices;
 var Vertices  : TVector3List;
     step, sz2 : single;
     i, j, vcount : integer;
     VertexPtr : ^TVector3;
     Vertex : TVector3;
     TexCoords : TVector2List;
 begin
   step := gridstep;
   Vertices := TVector3List.Create;
   vcount := fGridCount * fGridCount;
   Vertices.Count := vcount;
   VertexPtr := Vertices.Ptr(0);
   TexCoords := TVector2List.Create;
   TexCoords.Capacity := vcount;
   sz2 := CellCount * Step * 0.5;
   vertex.y := 0;
   vertex.z := -sz2;
   for i := 0 to fGridCount - 1 do
    begin
      Vertex.x := -sz2; { world x offset to align tiles }
      for j := 0 to fGridCount - 1 do
       begin
         VertexPtr^ := Vertex;
         vertex.x := vertex.x + step;
         TexCoords.Add(Vector2(0,0));
         inc( vertexptr );
       end;
      vertex.z := vertex.z + step;
    end;
   CoordinateNode.SetPoint( Vertices );
   TexCoordNode.SetPoint(TexCoords);
   Vertices.Free;
   TexCoords.Free;
 end;



end.
