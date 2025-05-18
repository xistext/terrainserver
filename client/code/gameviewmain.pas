{ Terrain Client
  Based on the Castle Game Engine Indy Client/Server demo.
  Erik Johnson erik@edj.net

}

unit GameViewMain;

interface

uses Classes, SysUtils,
  { indy }
  idGlobal,
  { cge }
  {$ifdef OpenGLES} CastleGLES {$else} CastleGL {$endif},
  CastleUtils, CastleLog, CastleBehaviors,
  CastleVectors, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleClientServer, CastleTerrain, CastleScene,
  CastleViewport, CastleCameras, CastleTransform, CastleWindow, CastleImages,
  { terrain client }
  watergrid, WaterParams,
  TerServerCommon, TerrainData, TerrainParams, TerrainObjects,
  TerrainShader, TerrainMesh,
  BaseMesh, TerrainClient, watercolor, layermarkup,
  TreeBuilder;

const
  tool_none  = 0;
  tool_brush = 1;
  tool_dig   = 2;
  tool_pile  = 3;
  tool_globe = 4;
  tool_view  = 5;

  fogfactor : single = 0.0; { used to turn fog on/off }

type

  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    Fog1 : TCastleFog;

    LabelFps: TCastleLabel;
    EditHostname: TCastleEdit;
    EditPort: TCastleIntegerEdit;
    ButtonCreateClient: TCastleButton;
    EditSend: TCastleEdit;
    ButtonSend: TCastleButton;
    Viewport1 : TCastleViewport;
    MainNavigation : TCastleWalkNavigation;
    MainCamera : TCastleCamera;
    FPSLabel : TCastleLabel;

    ButtonContour : TCastleButton;
     LabelContourScale : TCastleLabel;
    ButtonGrid    : TCastleButton;
     LabelGridScale : TCastleLabel;
    ButtonWater   : TCastleButton;
    ButtonFog     : TCastleButton;

    ColorPreview : TCastleShape;
    RedSlider : TCastleIntegerSlider;
    GreenSlider : TCastleIntegerSlider;
    BlueSlider : TCastleIntegerSlider;
    AlphaSlider : TCastleIntegerSlider;
    AlphaSlider1 : TCastleIntegerSlider;
    AlphaSlider2 : TCastleIntegerSlider;
    AlphaSlider3 : TCastleIntegerSlider;
    AlphaSlider4 : TCastleIntegerSlider;
    ToolRadiusSlider : TCastleIntegerSlider;

    PointLight1 : TCastlePointLight;
    TerrainLayer : TCastleTransform;
    WaterLayer   : TCastleTransform;

    ButtonBrush : TCastleButton;
    ButtonDig   : TCastleButton;
    ButtonPile  : TCastleButton;
    ButtonGlobe : TCastleButton;
    ButtonView  : TCastleButton;
    ToolShape : TCastleShape;
    ToolPileIndicator : TCastleShape;
    ToolDigIndicator : TCastleShape;

    ColorPicker : TCastleShape;
    TexturePreview1 : TCastleImageControl;
    TexturePreview2 : TCastleImageControl;
    TexturePreview3 : TCastleImageControl;
    TexturePreview4 : TCastleImageControl;

    PositionLabel : TCastleLabel;
    Elevationlabel : TCastleLabel;
    AltitudeLabel : TCastleLabel;
    ElevationIndicator : TCastleShape;
    WaterIndicator     : TCastleShape;
    AltitudeIndicator : TCastleShape;

    WorldOptionsPanel : TCastleShape;
    ViewOptionsPanel : TCastleShape;
    ViewRadiusSlider : TCastleIntegerSlider;
    CheckboxShadows : TCastleCheckbox;
    SnowLineSlider   : TCastleFloatSlider;
    GridPlane : TCastleTransform;

  private
    connectiontimeout : single;
    connectionstatus : integer;
    FClient: TTerClient;
    procedure HandleConnected;
    procedure HandleDisconnected;
    procedure HandleMessageReceived(const AMessage: String);
    procedure HandleTileReceived( const msginfo : TMsgHeader;
                                  const tileinfo : TTileHeader;
                                  tilegrid : TSingleGrid;
                                  watergrid : TSingleGrid;
                                  floragrid : TSingleGrid );
    procedure HandleTreeReceived( const listinfo : TTileObjHeader;
                                  objlist :  TTileObj_RecList );

    procedure ClickCreateClient(Sender: TObject);
    procedure ClickLayer(Sender: TObject);
    procedure ClickSend(Sender: TObject);
    procedure ClickTool( Sender: TObject );
    procedure ColorSliderChange( sender : TObject );
    procedure ViewRadiusChange( sender : TObject );
    procedure SnowLineChange( sender : TObject );
    procedure ShadowsChange( sender : TObject );

    procedure ViewportPressed(const Sender: TCastleUserInterface;
                              const Event: TInputPressRelease; var Handled: Boolean);

  public
    activetool : integer;
    lockviewtoground : boolean;
    viewanchor : TPoint;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    procedure mousewheel( direction : integer );
    function Press(const Event: TInputPressRelease): boolean; override;
    function MoveAllowed(const Sender: TCastleNavigation;
                         const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
                         const Radius: Single; const BecauseOfGravity: Boolean): boolean;
    procedure HandleMotion(const Sender: TCastleUserInterface;
                           const Event: TInputMotion;
                           var Handled: Boolean);
    function HeightAboveTerrain(Pos: TVector3;
                                out Y: Single;
                                heighttype : byte = 0 ): boolean;
    procedure UseTool( var pos : tvector3 );
    procedure UpdateFPSLabel;
    procedure UpdatePositionIndicator;
    procedure UpdateViewAnchor( Pos : TVector2; ForceRebuild : boolean = false );
    procedure UpdateFog;
   end;

var
  ViewMain: TViewMain;

implementation //===============================================================

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
  ConnectionStatus := status_disconnected;
  ConnectionTimeout := 0;
  lockviewtoground := false;
  activetool := tool_none;
  InitializeLog;
end;

procedure TViewMain.Start;
 var
   MaxVertexUniformComponents: TGLint;
   MarkupLayer : TMarkupLayer;
begin
  inherited;
  GParentComponent := Viewport1.Items;
  Viewport1.OnMotion := {$ifdef FPC}@{$endif} HandleMotion;
  Viewport1.OnPress :=   {$ifdef FPC}@{$endif} ViewportPressed;
;
  ButtonCreateClient.OnClick := {$ifdef FPC}@{$endif} ClickCreateClient;
  ButtonGrid.OnClick := {$ifdef FPC}@{$endif} ClickLayer;
  ButtonWater.OnClick := {$ifdef FPC}@{$endif} ClickLayer;
  ButtonContour.OnClick := {$ifdef FPC}@{$endif} ClickLayer;
  ButtonFog.OnClick := {$ifdef FPC}@{$endif} ClickLayer;
  ButtonSend.OnClick := {$ifdef FPC}@{$endif} ClickSend;
  RedSlider.OnChange := {$ifdef FPC}@{$endif} ColorSliderChange;
  BlueSlider.OnChange := {$ifdef FPC}@{$endif} ColorSliderChange;
  GreenSlider.OnChange := {$ifdef FPC}@{$endif} ColorSliderChange;
  AlphaSlider.OnChange := {$ifdef FPC}@{$endif} ColorSliderChange;
  AlphaSlider1.OnChange :=  {$ifdef FPC}@{$endif} ColorSliderChange;
  AlphaSlider2.OnChange :=  {$ifdef FPC}@{$endif} ColorSliderChange;
  AlphaSlider3.OnChange :=  {$ifdef FPC}@{$endif} ColorSliderChange;
  AlphaSlider4.OnChange :=  {$ifdef FPC}@{$endif} ColorSliderChange;
  ToolRadiusSlider.OnChange :=  {$ifdef FPC}@{$endif} ColorSliderChange;
  ViewRadiusSlider.OnChange := {$ifdef FPC}@{$endif} ViewRadiusChange;
  CheckboxShadows.OnChange := {$ifdef FPC}@{$endif} ShadowsChange;
  SnowLineSlider.OnChange := {$ifdef FPC}@{$endif} SnowLineChange;
  ButtonBrush.OnClick := {$ifdef FPC}@{$endif} ClickTool;
  ButtonDig.OnClick := {$ifdef FPC}@{$endif} ClickTool;
  ButtonPile.OnClick := {$ifdef FPC}@{$endif} ClickTool;
  ButtonGlobe.OnClick := {$ifdef FPC}@{$endif} ClickTool;
  ButtonView.OnClick := {$ifdef FPC}@{$endif} ClickTool;

  RedSlider.Value := trunc(ColorPreview.Color.X *15);
  GreenSlider.Value := trunc(ColorPreview.Color.Y *15);
  BlueSlider.Value := trunc(ColorPreview.Color.Z *15);
  AlphaSlider.Value := trunc(ColorPreview.Color.W *15);

  ClickCreateClient( self );
  MainNavigation.Input_Jump.Assign(keyNone);
  MainNavigation.Input_Crouch.Assign(keyNone);
  MainNavigation.OnMoveAllowed := {$ifdef FPC}@{$endif} MoveAllowed;
  LabelContourScale.Caption := '';
  LabelGridScale.Caption := '';

  SnowLineSlider.Value := defaultSnowLine;

  MarkupLayer := TMarkupLayer.Create(Viewport1);
  MarkupLayer.terrainheight := {$ifdef fpc}@{$endif}HeightAboveTerrain;
  TerrainLayer.Add(MarkupLayer);
  GMarkupLayer := MarkupLayer;

  TexturePreview1.Color := vector4(1,1,1,0.5);
  TexturePreview1.AlphaChannel := acAuto;

  TexturePreview2.Color := vector4(1,1,1,0.5);
  TexturePreview2.AlphaChannel := acAuto;

  ColorPicker.Translation := vector2( 80, 0 );
  ColorPicker.Exists := false;
  ColorSliderChange( self );
  UpdatePositionIndicator;

  WorldOptionsPanel.Translation := ColorPicker.Translation;
  WorldOptionsPanel.Exists := false;
  ViewOptionsPanel.Translation := ColorPicker.Translation;
  ViewOptionsPanel.Exists := false;

  glGetIntegerv(GL_MAX_VERTEX_UNIFORM_COMPONENTS, @MaxVertexUniformComponents);
  WritelnLog('GL_MAX_VERTEX_UNIFORM_COMPONENTS: %d', [MaxVertexUniformComponents]);

  UpdateFog;
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(FClient);
  inherited;
end;

procedure TViewMain.UpdatePositionIndicator;
 var terrainh, waterh : single;
     agl : single;
 begin
   terrainh := 0;
   waterh := 0;
   TerrainHeight( MainCamera.Translation, terrainh );
   GTileList.WaterAtPos( Vector2( MainCamera.Translation.X, MainCamera.Translation.Z ), waterh );
   agl := MainCamera.Translation.Y - terrainh;
   PositionLabel.Caption := Format( '%f x %f', [MainCamera.Translation.X, MainCamera.Translation.Z] );
   ElevationLabel.Caption := Format( '%f', [TerrainH] );
   AltitudeLabel.Caption := Format( '%f', [MainCamera.Translation.Y] );
   ElevationIndicator.Height := 4 + TerrainH * 2;
   AltitudeIndicator.Translation := vector2( 0, ElevationIndicator.Height - 2 + agl*2 );
   WaterIndicator.Height := waterh * 2;
   WaterIndicator.Translation := vector2( 0, ElevationIndicator.Translation.Y + ElevationIndicator.Height );
 end;

procedure TViewMain.UpdateFPSLabel;
 var fps : single;
 begin
   fps := Container.Fps.RealFps;
   FPSLabel.Caption := Format('%2.0ffps', [fps]);
   case trunc( fps / 30 ) of
      0 : FPSLabel.Color := Vector4( 1, 0, 0, 0.5 );
      1 : FPSLabel.Color := Vector4( 1, 1, 0, 0.5 );
    else
       FPSLabel.Color := vector4( 0, 1, 0, 0.5 );
    end;
 end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
 begin
  inherited;
  if connectionstatus = status_connecting then
   begin
     connectiontimeout := connectiontimeout + secondspassed;
     if connectiontimeout > 1 then
      begin
//        infowrite( 'Terrain server at '+GDefaulthost+':'+inttostr(GDefaultPort)+' not found.' );
        if FClient <> nil then
        begin
          connectionstatus := status_disconnected;
          SetCreateClientMode( connectionstatus, ButtonCreateClient );
          FreeAndNil(FClient);
        end
      end;
   end;
  UpdateFPSlabel;

  MainNavigation.MouseLook := {( GActiveDrag = self ) and} ( buttonMiddle in Container.MousePressed );
end;

procedure TViewMain.HandleConnected;
begin
//  InfoWrite('Connected to terrain server.');
  UpdateViewAnchor( vector2( 0, 0 ), true );
  connectionstatus := status_connected;
  SetCreateClientMode( connectionstatus, ButtonCreateClient );
  ButtonSend.Enabled := FClient <> nil;
end;

procedure TViewMain.HandleDisconnected;
begin
//  InfoWrite('Disconnected from terrain server.');
  ButtonSend.Enabled := false;
  connectionstatus := status_disconnected;
  SetCreateClientMode( connectionstatus, ButtonCreateClient );
  if assigned( fclient ) then
     FreeAndNil( fClient );
end;

procedure TViewMain.HandleMessageReceived(const AMessage: String);
begin
//  InfoWrite('Received message: ' + AMessage);
end;

procedure TViewMain.ClickCreateClient(Sender: TObject);
begin
  case connectionstatus of
    status_disconnected :
    begin
//      InfoWrite( 'Connecting to terrain server at '+GDefaultHost + ':' + IntToStr( GDefaultPort ) +'...');
      FClient := TTerClient.Create;
      FClient.Hostname := GDefaultHost;
      FClient.Port := GDefaultPort;

      FClient.OnConnected := {$ifdef FPC}@{$endif} HandleConnected;
      FClient.OnDisconnected := {$ifdef FPC}@{$endif} HandleDisconnected;
      FClient.OnMessageReceived := {$ifdef FPC}@{$endif} HandleMessageReceived;
      FClient.fOnTileReceived :=  {$ifdef FPC}@{$endif}HandleTileReceived;
      FClient.fOnTreeReceived :=  {$ifdef FPC}@{$endif}HandleTreeReceived;
      connectiontimeout := 0;
      connectionstatus := status_connecting;
      FClient.Connect;
    end;
    status_connected : if FClient <> nil then
     begin
       connectionstatus := status_disconnected;
       FClient.Disconnect;
       FreeAndNil(FClient);
     end;
    status_connecting : if FClient <> nil then
     begin
       connectionstatus := status_disconnected;
       FreeAndNil(FClient);
     end
   end;
  SetCreateClientMode( connectionstatus, ButtonCreateClient );
end;

 procedure RemoveWaterMesh( Layer : TCastleTransform;
                            Tile : TTerTile );
  var g : TCastleTransform;
  begin
    g := Tile.WaterGraphics;
    if assigned( g ) then
     begin
       Layer.Remove( g );
       FreeAndNil( g );
     end;
  end;

 procedure RemoveTerrainMesh( Layer : TCastleTransform;
                              Tile : TTerTile );
  var g : TCastleTransform;
  begin
    g := Tile.TerrainGraphics;
    if assigned( g ) then
     begin
       Layer.Remove( g );
       FreeAndNil( g );
     end;
  end;


 procedure CreateWaterMesh( Layer : TCastleTransform;
                            Tile : TTerTile );
  begin
    RemoveWaterMesh( Layer, Tile );
    Tile.WaterGraphics := TWaterMesh.Create2( Layer, Tile );
    Layer.Add( Tile.WaterGraphics );
  end;

 procedure CreateTerrainMesh( Layer : TCastleTransform;
                              Tile : TTerTile );
  begin
    RemoveTerrainMesh( Layer, Tile );
    Tile.TerrainGraphics := TTerrainMesh.Create2( Layer, Tile );
    Layer.Add( Tile.TerrainGraphics );
  end;


type TTexGrid = array of tvector2;

function CalculateWater( Tile : TTerTile ) : TTexGrid;
 { calculates water heights and tex map }
 var x, y : integer;
     h : single;
     waterh, terrainh, florah : psingle;
     watergrid : TSingleGrid;
 begin
   watergrid := Tile.WaterGrid;
   waterh := watergrid.ptrix(0);
   terrainh := Tile.TerrainGrid.ptrix(0);
   florah := Tile.FloraGrid.ptrix(0);
   result := [];
   setlength( result, watergrid.wxh );
   for x := 0 to watergrid.wh - 1 do for y := 0 to watergrid.wh - 1 do
    begin
      h := waterh^;
      result[ y * watergrid.wh + x ] := CalcDepthAndTexture( h, florah^, terrainh^, true );
      waterh^ := terrainh^ + h;
      { calculate water texture index }
      inc( waterh );
      inc( terrainh );
      inc( florah );
    end;
 end;

 procedure TViewMain.HandleTreeReceived( const listinfo : TTileObjHeader;
                                         objlist : TTileObj_RecList );
  var tile : TTerTile;
      ix : integer;
      tilelist : TTileObjList;
  begin
    if GTileList.findtile( listinfo.TileX, listinfo.tiley, ix ) then
     begin
       tile := TTerTile( GTileList.at( ix ));



     end;
  end;

procedure TViewMain.HandleTileReceived( const msginfo : TMsgHeader;
                                        const tileinfo : TTileHeader;
                                        tilegrid : TSingleGrid;
                                        watergrid : TSingleGrid;
                                        floragrid : TSingleGrid );
 var tile : TTerTile;
     TexGrid : TTexGrid;
 begin
   Tile := GTileList.GetInitTile( tileinfo );
   if assigned( tile ) then case msginfo.msgtype of
     msg_water : begin  { obsolete in favor of msg_water2? }
                    { calculate water elevations }
                    if ( tilegrid.wh <> tile.TerrainGrid.wh ) or
                       ( tilegrid.wh <> tile.FloraGrid.wh ) or
                       ( tilegrid.wh <> tile.WaterGrid.wh ) then
                     begin
                       { water lod mismatch, currently ignores the message, the next one should be the right LOD }
                     end
                    else
                     begin
                       Tile.SetWaterGrid( TileGrid );
                       TileGrid := nil;
                       TexGrid := CalculateWater( Tile );
                       if ( not assigned( Tile.WaterGraphics )) or
                          ( Tile.Info.TileSz <> tileInfo.TileSz ) then
                        begin
                          Tile.Info := TileInfo;
                          CreateWaterMesh( WaterLayer, Tile );
                        end;
                       { update tile graphics }
                       TTerrainMesh( Tile.WaterGraphics ).UpdateFromGrid( Tile.WaterGrid );
                       TTerrainMesh( Tile.WaterGraphics ).UpdateVerticesTexture(TexGrid);
                     end;
                  end;
     msg_tile : begin
                  if not assigned( Tile.TerrainGraphics ) then
                   begin
                     Tile.Info := TileInfo;
                     CreateTerrainMesh( TerrainLayer, Tile );
                     CreateWaterMesh( WaterLayer, Tile );
                   end
                  else
                   begin
                     if ( Tile.Info.TileSz <> tileInfo.TileSz ) then
                      begin
                        Tile.Info := TileInfo;
                        CreateTerrainMesh( TerrainLayer, Tile );
                        CreateWaterMesh( WaterLayer, Tile );
                      end;
                   end;
                  { update tile graphics }
                  TTerrainMesh( Tile.TerrainGraphics ).UpdateFromGrid( TileGrid );
                  Tile.SetTerrainGrid( TileGrid );
                  TileGrid := nil;
                end;

      msg_LODUpdate : begin { LOD change.  Message has all effected layers }
                     assert( assigned( tilegrid ) and assigned( watergrid ) and assigned( floragrid ));
                     Tile.SetTerrainGrid( TileGrid );
                     Tile.SetWaterGrid( WaterGrid );
                     Tile.SetFloraGrid( FloraGrid );

                     { calculate water elevations }
                     TexGrid := CalculateWater( Tile );

                     { build/update water and terrain as needed }
                     if ( Tile.Info.TileSz <> tileinfo.tilesz ) then
                      begin
                        Tile.Info := TileInfo;
                        CreateTerrainMesh( TerrainLayer, Tile );
                        TTerrainMesh( Tile.TerrainGraphics ).UpdateFromGrid( tilegrid );
                        CreateWaterMesh( WaterLayer, Tile );
                      end
                     else
                      begin
                        if not assigned( tile.terraingraphics ) then
                           CreateTerrainMesh( TerrainLayer, Tile );
                        if not assigned( tile.watergraphics ) then
                           CreateWaterMesh( WaterLayer, Tile );
                      end;

                     TTerrainMesh( Tile.TerrainGraphics ).UpdateFromGrid( TileGrid );
                     TTerrainMesh( Tile.WaterGraphics ).UpdateFromGrid( waterGrid );
                     TTerrainMesh( Tile.WaterGraphics ).UpdateVerticesTexture( texgrid );
                     TileGrid := nil;
                     WaterGrid := nil;
                     FloraGrid := nil;
                   end;
      msg_splat : begin
                    if assigned( tile.TerrainGraphics ) then
                       TTerrainMesh( Tile.TerrainGraphics ).UpdateSplatmap( tilegrid )
                  end;
      msg_flora : begin
                    tile.SetFloraGrid( tilegrid );
                    tilegrid := nil;
                  end;
    end;
   { free the sent data when finished }
   if assigned( tilegrid ) then
      TileGrid.Free;
   if assigned( watergrid ) then
      watergrid.Free;
   if assigned( floragrid ) then
      floragrid.Free;

 end;


procedure TViewMain.ClickLayer(Sender: TObject);
 var button : TCastleButton;
     tile : TTerTile;
     i : integer;
     updateappearance : boolean;
 begin
   button := TCastleButton( Sender );
   Button.Pressed := not Button.Pressed;
   updateappearance := true;
   if button = buttongrid then
    begin
      GShowGrid := Button.Pressed;
      if GShowGrid then
       begin
         LabelGridScale.Caption := FormatFloat( '0.#', 1/GGridScale * 5 )+'m';
         Button.CustomBackground := false;
       end
      else
       begin
         LabelGridScale.Caption := '';
         Button.CustomBackground := true;
       end;
    end
   else
   if button = buttoncontour then
    begin
      GShowContour := Button.Pressed;
      if GShowContour then
       begin
         LabelContourScale.Caption := FormatFloat( '0.#', 1/GContourScale * 5 )+'m';
         Button.CustomBackground := false;
       end
      else
       begin
         LabelContourScale.Caption := '';
         Button.CustomBackground := true;
       end;
    end
   else
   if button = buttonwater then
    begin
      updateappearance := false;
      WaterLayer.Visible := Button.Pressed;
      Button.CustomBackground := not WaterLayer.Visible;
    end
   else
   if button = buttonfog then
    begin
      updateappearance := false;
      if Button.Pressed then
         fogfactor := 1
      else
         fogfactor := 0;
      UpdateFog;
    end;

  if updateappearance then for i := 0 to gtilelist.Count - 1 do
   begin
     Tile := TTerTile( gTileList.At( i ));
     if assigned( Tile.TerrainGraphics ) then
        TTerrainMesh( Tile.TerrainGraphics ).UpdateAppearance;
   end;
 end;

procedure TViewMain.ClickTool( Sender:Tobject );
 var thisbutton : TCastleButton;
 begin
   thisbutton := TCastleButton( Sender );
   thisbutton.pressed := not thisbutton.pressed;
   if thisbutton.pressed then
    begin
      if thisbutton <> buttondig then
         buttondig.pressed := false;
      if thisbutton <> buttonbrush then
         buttonbrush.pressed := false;
      if thisbutton <> buttonpile then
         buttonpile.Pressed := false;
      if thisbutton <> buttonglobe then
         buttonglobe.Pressed := false;
      if thisbutton <> buttonview then
         buttonview.Pressed := false;
    end;
   activetool := tool_none;
   if buttonbrush.pressed then
      activetool := tool_brush
   else
   if buttondig.pressed then
      activetool := tool_dig
   else
   if buttonpile.pressed then
      activetool := tool_pile
   else
   if buttonglobe.pressed then
      activetool := tool_globe
   else
   if buttonview.pressed then
      activetool := tool_view;
   ColorPicker.Exists := activetool = tool_brush;
   WorldOptionsPanel.Exists := activetool = tool_globe;
   ViewOptionsPanel.Exists := activetool = tool_view;
   ColorSliderChange( self );
 end;

procedure TViewMain.ClickSend(Sender: TObject);
begin
  FClient.Send(EditSend.Text);
end;

procedure TViewMain.ColorSliderChange( sender : TObject );
 var factor : single;
 begin
   factor := 1/15;
   ColorPreview.Color := vector4( RedSlider.Value * factor, GreenSlider.Value * factor, BlueSlider.Value * factor, AlphaSlider.Value * factor );
   TexturePreview1.Color := vector4( 1, 1, 1, AlphaSlider1.Value * factor );
   TexturePreview2.Color := vector4( 1, 1, 1, AlphaSlider2.Value * factor );
   TexturePreview3.Color := vector4( 1, 1, 1, AlphaSlider3.Value * factor );
   TexturePreview4.Color := vector4( 1, 1, 1, AlphaSlider4.Value * factor );

   if activetool = tool_brush then
      ToolShape.Color := ColorPreview.Color // ?how to combine textures?
   else
      ToolShape.Color := vector4( 0, 0, 0.12, 0.62 );
   ToolShape.Width := ToolRadiusSlider.Value * 5;
   ToolShape.Height := ToolRadiusSlider.Value * 2.5;
   ToolPileIndicator.Width := ToolShape.Width;
   ToolPileIndicator.Height := ToolShape.Height;
   ToolDigIndicator.Width := ToolShape.Width;
   ToolDigIndicator.Height := ToolShape.Height;
   ToolPileIndicator.Exists := activetool = tool_pile;
   ToolDigIndicator.Exists := activetool = tool_dig;
 end;


  type tremovetilerecord = record
          ViewMain : TViewMain;
          cpt    : tpoint;
          radius  : integer;
        end;

  procedure removedistanttile( atile : ttertile;
                               var doremove : boolean;
                               data : pointer );
   var d : integer;
       g : TCastleTransform;
   begin
     with tremovetilerecord( data^ ) do
      begin
        d := atile.tiledist( cpt );
        doremove := d > radius;
        if doremove then
         begin
           g := atile.terraingraphics;
           if assigned( g ) then
            begin
              ViewMain.TerrainLayer.Remove( g );
              g.free;
              atile.terraingraphics := nil;
            end;
           g := atile.watergraphics;
           if assigned( g ) then
            begin
              ViewMain.TerrainLayer.Remove( g );
              g.free;
              atile.watergraphics := nil;
            end;
         end;
      end;
   end;

procedure TViewMain.UpdateViewAnchor( Pos : TVector2; ForceRebuild : boolean = false );
 { rebuilds terrain around you if you move into a different tile or if forced }
 var cmd : string;
     radius : integer; { radius in tile index units }
     removerec : tremovetilerecord;
     newanchor : tpoint;
 begin
   newanchor := GTileList.CalculateTileOffset( pos );
   if forcerebuild or ( viewanchor.X <> newanchor.x ) or ( viewanchor.Y <> newanchor.y ) then
    begin
      viewanchor := newanchor;

//      GridPlane.Translation := vector3( viewanchor.X * 60 - 30, 0, viewanchor.Y * 60 - 30 );

      radius := ViewRadiusSlider.Value;
      cmd := 'build '+IntToStr( viewanchor.x )+','+inttostr(viewanchor.y)+','+inttostr(radius);
      FClient.Send(cmd);

      { remove distant tiles }
      removerec.ViewMain := self;
      removerec.radius := radius;
      removerec.cpt := viewanchor;
      GTileList.iteratetiles( {$ifdef fpc}@{$endif} removedistanttile, @removerec );
    end;
 end;

procedure TViewMain.UpdateFog;
 begin
   if fogfactor > 0 then
      Fog1.VisibilityRange := ( 60 * ViewRadiusSlider.Value + MainCamera.translation.y )
   else
      Fog1.VisibilityRange := 1000; { essentially 'no fog' }
 end;

procedure TViewMain.ViewRadiusChange( sender : TObject );
 begin
   UpdateFog;
   updateviewanchor( vector2( MainCamera.Translation.x, MainCamera.Translation.z ), true );
 end;

procedure TViewMain.SnowLineChange( sender : TObject );
 var cmd : string;
 begin
   DefaultSnowline := SnowLineSlider.Value;
   cmd := 'snowline '+FloatToStr( DefaultSnowline );
   FClient.Send(cmd);
 end;

procedure TViewMain.ShadowsChange( sender : TObject );
 begin
   PointLight1.Shadows := CheckBoxShadows.Checked;
 end;

function TViewMain.MoveAllowed(const Sender: TCastleNavigation;
                               const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
                               const Radius: Single; const BecauseOfGravity: Boolean): boolean;
 var terrainh : single;
 begin
   TerrainHeight( ProposedNewPos, TerrainH );
   NewPos := ProposedNewPos;
   lockviewtoground := ( NewPos.Y - radius < Terrainh ) or lockviewtoground;
   if lockviewtoground then
    begin
      NewPos := Vector3(ProposedNewPos.X, TerrainH + radius, ProposedNewPos.Z );
      MainNavigation.MoveHorizontalSpeed := sqrt(radius);
    end;
   UpdatePositionIndicator;
   UpdateViewAnchor( vector2( ProposedNewPos.X, ProposedNewPos.Z ));
   result := true;
 end;

function TViewMain.HeightAboveTerrain(Pos: TVector3;
                                      out Y: Single;
                                      heighttype : byte = 0 ): boolean;
 begin
   TerrainHeight( MainCamera.Translation, Y );
   result := true
 end;

procedure TViewMain.UseTool( var pos : tvector3 );
 var params : string;
     g : TCastleTransform;
 begin
   params := FormatFloat( '0.###', pos.x )+','+FormatFloat( '0.###', pos.z );
   case activetool of
      tool_none : begin
                    { place a test tree billboard }
                    g := GTreeBuilder.BuildTree( Viewport1, pos, 1 );
                    Viewport1.Items.Add( g );
                  end;
      tool_brush : fClient.Send( 'paint '+params+','+
                                  inttostr( encodesplatcell( RedSlider.Value, GreenSlider.Value, BlueSlider.Value, AlphaSlider.Value, 0, AlphaSlider1.Value ))+','+
                                  inttostr( toolradiusslider.value )
                                  );
      tool_dig : begin
                   fClient.Send( 'dig '+params+','+
                                  inttostr( toolradiusslider.value ) );
                   fClient.Send( 'paint '+params +','+inttostr( freshdigcolor )+','+
                                  inttostr( toolradiusslider.value ));
                 end;
      tool_pile : fClient.Send( 'pile '+params+','+
                                  inttostr( toolradiusslider.value ) );
    end;
 end;

procedure TViewMain.HandleMotion(const Sender: TCastleUserInterface;
                                 const Event: TInputMotion;
                                 var Handled: Boolean);
 var MouseRayOrigin, MouseRayDirection : TVector3;
     i : integer;
     pos : TVector3;
     thistile : ttertile;
     MousePosition : TVector2;
     Neighbors : TTileNeighbors;
 begin
   if buttonleft in event.Pressed then
    begin
      if GTileList.findtileatlocation( vector2( MainCamera.translation.x, MainCamera.translation.z ), thistile ) and
         assigned( thistile.TerrainGraphics ) then
       begin
         MousePosition := Container.MousePosition;
//         if Viewport1.MousePosition(MousePosition) then
          begin
            Viewport1.PositionToRay(MousePosition, true, MouseRayOrigin, MouseRayDirection );

            if TTerrainMesh( thistile.TerrainGraphics ).raycast2( MouseRayOrigin, MouseRayDirection, Pos ) then
               UseTool( Pos )
            else
            begin
              Neighbors := thistile.GetNeighbors;
              for i := 0 to 7 do
               begin
                 if assigned( Neighbors[i] ) and assigned( Neighbors[i].terraingraphics ) and
                    TTerrainMesh( Neighbors[i].terraingraphics ).raycast2( MouseRayOrigin, MouseRayDirection, Pos ) then
                  begin
                    UseTool( Pos );
                    break;


                  end;
               end;

            end;
          end;
       end;
    end;
 end;


procedure TViewMain.ViewportPressed(const Sender: TCastleUserInterface;
                                    const Event: TInputPressRelease; var Handled: Boolean);
 var MouseRayOrigin, MouseRayDirection : TVector3;
     i : integer;
     pos : TVector3;
     thistile : ttertile;
     MousePosition : TVector2;
     Neighbors : TTileNeighbors;
 begin
   if ( event.EventType = itMouseButton ) and ( event.IsMouseButton( ButtonLeft )) then
    begin
      if GTileList.findtileatlocation( vector2( MainCamera.translation.x, MainCamera.translation.z ), thistile ) and
         assigned( thistile.TerrainGraphics ) then
       begin
         MousePosition := Container.MousePosition;
//         if Viewport1.MousePosition(MousePosition) then
          begin
            Viewport1.PositionToRay(MousePosition, true, MouseRayOrigin, MouseRayDirection );

            if TTerrainMesh( thistile.TerrainGraphics ).raycast2( MouseRayOrigin, MouseRayDirection, Pos ) then
               UseTool( Pos )
            else
            begin
              Neighbors := thistile.GetNeighbors;
              for i := 0 to 7 do
               begin
                 if assigned( Neighbors[i] ) and assigned( Neighbors[i].terraingraphics ) and
                    TTerrainMesh( Neighbors[i].terraingraphics ).raycast2( MouseRayOrigin, MouseRayDirection, Pos ) then
                  begin
                    UseTool( Pos );
                    break;


                  end;
               end;

            end;
          end;
       end;
    end;
 end;





procedure TViewMain.Mousewheel( direction : integer );
 var h, amt : single;
     delta : single;
     pos : TVector3;
     TerrainH : single;
 begin
(*   if altdown or ( ToolEvent = GDefaultTool ) or not ToolEvent.mousewheel( direction ) then
    begin*)
      h := MainCamera.translation.y;
      pos := Vector3( MainCamera.Translation.X, h, MainCamera.Translation.Z );
      TerrainHeight( Pos, TerrainH );
      amt := h - TerrainH;
      if amt < 1 then
         amt := 1;
      { if no tools are taking mousewheel then use it to change view height }
      delta := direction/3 * amt;
      h := h + delta;
      if h - MainNavigation.radius < TerrainH then
       begin
         lockviewtoground := true;
         h := MainNavigation.radius + TerrainH;
         MainNavigation.MoveHorizontalSpeed := 0.1;
       end
      else
         lockviewtoground := false;
      pos.Y := h;
      MainNavigation.MoveHorizontalSpeed := sqrt(amt);
    MainCamera.Translation := pos;
    UpdatePositionIndicator;
    UpdateFog;
 end;

function TViewMain.Press(const Event: TInputPressRelease): boolean;

  function checkkey( key : TKey; var mainresult : boolean ) : boolean;
   begin
     result := Event.isKey( key );
     mainresult := mainresult or result;
   end;
  procedure checkmousewheel;
   begin
     if Event.IsMouseWheel( mwUp ) then
      begin
        mousewheel( 1 );
        result := true;
      end
     else
     if Event.IsMouseWheel( mwDown ) then
      begin
        mousewheel( -1 );
        Result := true;
      end;
   end;
begin
  Result := false;
//  if ( gactivedrag <> self ) and ( gactivedrag <> nil ) then exit;
  Result := inherited;
  if Result then Exit;

  case event.eventtype of
    itMouseWheel : checkmousewheel;
    itMouseButton : begin
                      (*GActiveDrag := self;
                      if Event.IsMouseButton(buttonRight) then
                       begin
                        // HitDispatch( true );
                         RightButtondown := true;
                         MouseDragStart := Container.MousePosition;
                         Result := true;
                       end;
                      if Event.IsMouseButton(buttonLeft) then
                       begin
                         if not UseCurrToolOnTerrain then HitDispatch( false );
                         Result := true;
                       end;*)
                    end;
    itKey : begin
(*              if checkkey(keyTab,result) then
                 NextTimeSpeed;
              if checkkey(keySpace,result) then
                 PauseTime;
              if checkkey(key5,result) then
                 SelectionPanel.exists := false;
              if checkkey(key1,result) then
                 SelectionView.Camera := MapCamera;
              if checkkey(keyF11,result) then
                 Application.MainWindow.FullScreen := not Application.MainWindow.FullScreen;
              if checkkey(keyF12,result) then
                 Container.SaveScreenToDefaultFile;
              if checkkey(keyEscape,result) then
               begin
                 if ToolEvent <> GDefaultTool then { cancel any active tool }
                  begin
                    ToolEvent.Free;
                    ToolEvent := GDefaultTool;
                  end
                 else
                    ClosePanels;
               end;
              if checkkey(keyF10,result) then { toggle terrain wirefram }
                 ToggleTerrainWireframe;
              if checkkey(keyF11,result) then { toggle treelayer }
                 trees.Exists := not trees.exists;
              if checkkey(keyF5,result) then
                 Quicksave;
              if checkkey(keyDelete,result) then
                 DeleteSelection;
              if checkkey(keyB,result) then
               begin
                 form := TBuildingPlanner.create( MainViewport );
        {         formx := container.pixelswidth - GFormCount * defaultFormwidth;
                 form.Translation := vector2( formx, defaultformy );}
                 MainViewport.InsertFront( form );
               end;                               *)
            end;
    end;
 end;


end.
