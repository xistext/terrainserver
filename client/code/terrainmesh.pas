unit terrainmesh;

interface

uses
   Classes, SysUtils,  Generics.Collections,
   x3dnodes, x3dfields, CastleInternalGeometryArrays,
   castleclassutils, castlevectors, castlerenderoptions, castlecolors, CastleDownload,
   terrainparams,
   basemesh, watergrid, TerrainData, TerrainShader;


{
  sand, clay, dirt, soil, gravel, rocks, boulders }

type

TTerrainMesh = class; {forward}

TSplatPalette = array of TVector3;

TTextureLayer = class(TCastleComponent)
   public
   Terrain     : TTerrainMesh;
   TextureNode : TImageTextureNode;
   constructor CreateForTerrain( const ATerrain : TTerrainMesh;
                                 layer : integer;
                                 url : string;
                                 const Effect : TEffectNode );
   function GetTexture: String;

   procedure SetTexture(const Value: String);

 end;

TTerrainMesh = class( TAbstractTextureMesh )
     constructor create( aowner : TComponent );  override;
     constructor create2( aowner : TComponent;
                          iLinkedTile : TTerTile );
     function offset : TVector2; override;
     procedure UpdateSize;
//   procedure updategraphics; override;
     procedure updatefromgrid( TerrainGrid : TSingleGrid );
     procedure UpdateAppearance;
     function InitAppearance : TAppearanceNode; override;
     public
     LinkedTile : TTerTile;

     protected

     procedure setGridCount( iGridCount : integer ); override;
     function getGridCount : integer; override;

     function getGridStep : single; override;

     function BuildTerrainEffect: TEffectNode;

   end;

const GShaderId : integer = 0;
      GShowGrid : boolean = false;
      GShowContour : boolean = false;
//      GGridScale : integer = 1; { 5m }
      GGridScale : single = 5; { 5m }
      GContourScale : single = 5; {5m }
var  GSplatPalette : TSplatPalette;

function gettileshader( Tile : ttertile;
                        shaderid : integer ) : TTileShader;

implementation

const
  { URL of a white pixel texture, embedded PPM.
    See http://netpbm.sourceforge.net/doc/ppm.html . }
  WhitePixel = 'data:image/x-portable-pixmap,P3'#10 +
    '1 1'#10 +
    '255'#10 +
    '255 255 255';


function gettileshader( Tile : ttertile;
                        shaderid : integer ) : TTileShader;
 begin
     case shaderid of
       0 : result := TElevationShader.create('5m',1);
       1 : result := TElevationShader.create('1m',5);
    end;
 end;

//------------------------------

constructor TTextureLayer.CreateForTerrain( const ATerrain : TTerrainMesh;
                                            layer : integer;
                                            url : string;
                                            const Effect : TEffectNode );
 var TextureField : TSFNode;
 begin
   inherited create( ATerrain );
   Terrain := ATerrain;

   TextureNode := TImageTextureNode.Create;
   TextureNode.SetUrl( [url] );

   TextureField := TSFNode.Create(Effect, false, 'tex_'+IntToStr(layer), [], TextureNode );
   Effect.AddCustomField( TextureField );
 end;

function TTextureLayer.GetTexture: String;
begin
  if TextureNode.FdUrl.Count = 2 then // [image url, WhitePixel]
    Result := TextureNode.FdUrl.Items[0]
  else
    Result := '';
end;

procedure TTextureLayer.SetTexture(const Value: String);
begin
  if GetTexture() <> Value then
  begin
    if Value <> '' then
      TextureNode.SetUrl([Value, WhitePixel])
    else
      TextureNode.SetUrl([WhitePixel]);
    // TODO: Only this will properly update the shader uniform to use new texture
    Terrain.ChangedAll;
  end;
end;


//------------------------------

constructor TTerrainMesh.create( aowner : TComponent );
 begin
   inherited create( aowner );
   LinkedTile := nil;
   Pickable := true;
//   PreciseCollisions := true;
 end;

constructor TTerrainMesh.create2( aowner : TComponent;
                                  iLinkedTile : TTerTile );
var sz : single;
 begin
   inherited create( aowner );
   LinkedTile := iLinkedTile;
   if assigned( LinkedTile ) then with LinkedTile do
    begin
      UpdateSize;
      sz := GDefGridCellCount * GDefGridStep;
    end;
   RenderOptions.WireframeEffect := weSilhouette;
   pickable := true;
//   PreciseCollisions := true;
end;

function TTerrainMesh.offset : TVector2;
 var sz, sz2 : single;
 begin
   if assigned( LinkedTile ) then
    begin
      sz := LinkedTile.GetWorldSize;
      sz2 := sz/2;
      result := vector2( LinkedTile.Info.TileX * sz,
                         -sz2 + LinkedTile.Info.TileY * sz )
    end
   else
      result := inherited Offset;
 end;

procedure TTerrainMesh.UpdateSize;
 begin
   InitializeData;
 end;

procedure TTerrainMesh.setGridCount( iGridCount : integer );
 begin
   LinkedTile.Info.TileSz := iGridCount;
 end;

function TTerrainMesh.getGridCount : integer;
begin
  result := LinkedTile.Info.TileSz;
end;

function TTerrainMesh.getGridStep : single;
begin
  Result := LinkedTile.GridStep;
end;

   function readstring( url : string ) : string;
    var MyTextReader : TTextReader;
        Line : string;
        L : integer;
    begin
      result := '';
      MyTextReader := TTextReader.Create( url );
      while not MyTextReader.eof do
       begin
         Line := MyTextReader.Readln;
         L := Length( Line );
        // if ( L > 0 ) and (( L = 1 ) or ( copy( Line, 1, 2 ) <> '//' )) then
            result := result + line + lineending;
       end;
      MyTextReader.Free;
    end;

type tpaletterec = packed record
        ix : byte;
        a : byte;
        r0, r1 : byte;
      end;

function encodesplatcell( r, g, b, a : byte;
                          t1, t1a : byte ) : integer;
 { encode 4 bit r,g,b,a into integer }
 begin
   assert(( r < 16 ) and ( g < 16 ) and ( b < 16 ) and ( a < 16 ));
   result := r + g shl 4 + b shl 8 + a shl 12 + t1 shl 16 + t1a shl 20;
 end;

function decodesplatcell( v : integer ) : integer;
 { encode 8 bit alpha into the paletteix }
 var r, g, b, a : integer;
 begin
   r := v shl 28 shr 28;
   g := v shl 24 shr 28;
   b := v shl 20 shr 28;
   a := v shl 16 shr 28;
   result := r +
             g * 16 +
             b * 256 +
             a * 4096;
(*   with tpaletterec( result ) do
    begin
      ix := paletteix;
      a := alpha;
      r0 := 0;
      r1 := 0;
    end;*)
 end;


function TTerrainMesh.BuildTerrainEffect : TEffectNode;
 var FragmentPart, VertexPart : TEffectPartNode;
     ix, iy : integer;
     SplatMap : TMFLong;
     TexImage : TImageTextureNode;
 begin
   Result := TEffectNode.Create;
   Result.Language := slGLSL;
   Result.UniformMissing := umIgnore;

   Result.AddCustomField(TSFVec4f.Create(Result, true, 'uv_scale', Vector4( 0.11, 0.26, 0.36, 0.36)));
   Result.AddCustomField(TSFVec4f.Create(Result, true, 'metallic', Vector4( 1, 1, 1, 1)));
   Result.AddCustomField(TSFVec4f.Create(Result, true, 'roughness', Vector4( 1, 1, 1, 1)));

   Result.AddCustomField(TSFBool.Create(Result, true, 'blur', true ));

   Result.AddCustomField(TSFFloat.Create(Result, true, 'height_1', 4));
   Result.AddCustomField(TSFFloat.Create(Result, true, 'height_2', 8));

   TexImage := TImageTextureNode.Create;
   TexImage.SetUrl( ['castle-data:/terrain/textures/island_sand2_d.jpg'] );
   Result.AddCustomField( TSFNode.Create(Result, false, 'tex_1', [], TexImage ));

   TexImage  := TImageTextureNode.Create;
   TexImage.SetUrl( ['castle-data:/terrain/textures/ground_mud2_d.jpg'] );
   Result.AddCustomField( TSFNode.Create(Result, false, 'tex_2', [], TexImage ));

   TexImage := TImageTextureNode.Create;
   TexImage.SetUrl( ['castle-data:/terrain/textures/moss_ground_d.jpg'] );
   Result.AddCustomField( TSFNode.Create(Result, false, 'tex_3', [], TexImage ));

   TexImage := TImageTextureNode.Create;
   TexImage.SetUrl( ['castle-data:/terrain/textures/mntn_green_d.jpg'] );
   Result.AddCustomField( TSFNode.Create(Result, false, 'tex_4', [], TexImage ));

   Result.AddCustomField(TSFInt32.Create(Result, true, 'splat_sz', 60));
   splatmap := TMFLong.Create( Result, true, 'splatmap', [] );
   splatmap.items.Count := 3600;
   for ix := 0 to 60 - 1 do
       for iy := 0 to 60 - 1 do
           splatmap.items[ix*60+iy] := encodesplatcell( random(8), random(16), random(8), random(8), random(4), random(16));
   Result.AddCustomField(splatmap);

   Result.AddCustomField(TSFFloat.Create(Result, true, 'grid_scale', ord( GShowGrid ) * GGridScale ));
   Result.AddCustomField(TSFFloat.Create(Result, true, 'contour_scale', ord( GShowContour ) * GContourScale ));

   Result.AddCustomField(TSFVec4f.Create(Result, true, 'grid_color', Vector4( 0.1,0.2,0.2,0.5 )));
   Result.AddCustomField(TSFVec4f.Create(Result, true, 'grid10_color', Vector4( 0.5,0.1,0.1,0.5 )));
   Result.AddCustomField(TSFVec4f.Create(Result, true, 'contour_color', Vector4( 0.38,0.19,0.0,0.9 )));

   Result.AddCustomField(TSFFloat.Create(Result, true, 'layers_influence', 1.0));
   Result.AddCustomField(TSFFloat.Create(Result, true, 'steep_emphasize', 5 ));


   { initialize 2 EffectPart nodes (one for vertex shader, one for fragment shader) }
   FragmentPart := TEffectPartNode.Create;
   FragmentPart.ShaderType := stFragment;
   FragmentPart.Contents := readstring('castle-data:/terrain/textures/terrain.fs');

   VertexPart := TEffectPartNode.Create;
   VertexPart.ShaderType := stVertex;
   VertexPart.Contents := readstring('castle-data:/terrain/textures/terrain.vs');

   Result.SetParts([FragmentPart, VertexPart]);
 end;

function TTerrainMesh.InitAppearance : TAppearanceNode;
 var Effect : TEffectNode;
 begin
   Result := inherited;

   Effect := BuildterrainEffect;
   Result.SetEffects([Effect]);
 end;

procedure buildvertexlistsfromgrid( grid : TSingleGrid;
                                    Vertices : TVector3List;
                                    shader : TTileShader );
var i, j, c, posy : integer;
    VertexPtr : ^TVector3;
    hptr : PSingle;
 begin
   VertexPtr := Vertices.Ptr(0);  { starting vertex pointer }
   posy := 0;
   c := grid.wh;
   for i := c - 1 downto 0 do
    begin
      hptr := grid.ptrix( posy );
      for j := c - 1 downto 0 do
       begin
         VertexPtr^.Y := hptr^;
         inc( VertexPtr );

         inc( hptr, grid.wh );
       end;
      inc( posy );
    end;
 end;

procedure TTerrainMesh.updatefromgrid( TerrainGrid : TSingleGrid );
var shader : TTileShader;
    coord : TCoordinateNode;
begin
  shader := gettileshader( LinkedTile, GShaderId );
  coord := CoordinateNode;
  buildvertexlistsfromgrid( TerrainGrid,
                            Coord.FdPoint.Items, shader );
  shader.free;
  Coord.FdPoint.changed; { trigger mesh to rebuild }
//  dirty := false;
end;

procedure TTerrainMesh.UpdateAppearance;
var Appearance : TAppearanceNode;
    EffectNode : TEffectNode;
begin
  Appearance := TAppearanceNode( rootnode.FindNode( TAppearanceNode, true ));

  EffectNode := TEffectNode( Appearance.fdEffects[0] );
  TSFFloat( EffectNode.Field( 'contour_scale' )).Value := ord( GShowContour ) * GContourScale;
  TSFFloat( EffectNode.Field( 'grid_scale' )).Value := ord( GShowGrid ) * GGridScale;
 // TSFFloat( EffectNode.Field( 'overlay_scale' )).Changed;

  self.ChangedAll( true );

//  dirty := false;
end;

procedure buildsplatpalette;
 var i : integer;
     v : single;
     ix : integer;
 begin
   { black .. white }
   setlength( gsplatpalette, 71 );
   ix := 0;
   for i := 0 to 10 do
    begin
      v := i * 0.1;
      gsplatpalette[ix] := vector3( v, v, v );
      inc( ix )
    end;
   { red }
   for i := 1 to 10 do
    begin
      v := i * 0.1;
      gsplatpalette[ix] := vector3( v, 0, 0 );
      inc( ix )
    end;
   { green }
   for i := 1 to 10 do
    begin
      v := i * 0.1;
      gsplatpalette[ix] := vector3( 0, v, 0 );
      inc( ix )
    end;
   { blue }
   for i := 1 to 10 do
    begin
      v := i * 0.1;
      gsplatpalette[ix] := vector3( 0, 0, v );
      inc( ix )
    end;
   { yellow }
   for i := 1 to 10 do
    begin
      v := i * 0.1;
      gsplatpalette[ix] := vector3( v, v, 0 );
      inc( ix )
    end;
   { cyan }
   for i := 1 to 10 do
    begin
      v := i * 0.1;
      gsplatpalette[ix] := vector3( 0, v, v );
      inc( ix )
    end;
   { magenta }
   for i := 1 to 10 do
    begin
      v := i * 0.1;
      gsplatpalette[ix] := vector3( v, 0, v );
      inc( ix )
    end;
 end;

initialization
  buildsplatpalette;
end.

