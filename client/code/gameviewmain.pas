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
  CastleUtils,
  CastleVectors, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleClientServer, CastleTerrain, CastleScene,
  CastleViewport, CastleCameras, CastleTransform, CastleWindow, CastleImages,
  { terrain client }
  TerServerCommon, TerrainData, TerrainParams, TerrainShader, TerrainMesh,
  watergrid, BaseMesh, TerrainClient, watercolor, layermarkup,
  debug;

const
  tool_none  = 0;
  tool_brush = 1;
  tool_dig   = 2;
  tool_pile  = 3;

type

  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
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

    TerrainLayer : TCastleTransform;
    WaterLayer   : TCastleTransform;

    ButtonBrush : TCastleButton;
    ButtonDig   : TCastleButton;
    ButtonPile  : TCastleButton;
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
    AltitudeIndicator : TCastleShape;

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
                                  floragrid : TSingleGrid;
                                  texgrid : TTexPoints );
    procedure ClickCreateClient(Sender: TObject);
    procedure ClickLayer(Sender: TObject);
    procedure ClickSend(Sender: TObject);
    procedure ClickTool( Sender: TObject );
    procedure ColorSliderChange( sender : TObject );

  public
    activetool : integer;
    lockviewtoground : boolean;
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
    procedure UseTool( const pos : tvector3 );
    procedure UpdatePositionIndicator;
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
end;

procedure TViewMain.Start;
 var
   MaxVertexUniformComponents: TGLint;
   MarkupLayer : TMarkupLayer;
begin
  inherited;
  GParentComponent := Viewport1.Items;
  ButtonCreateClient.OnClick := {$ifdef FPC}@{$endif} ClickCreateClient;
  ButtonGrid.OnClick := {$ifdef FPC}@{$endif} ClickLayer;
  ButtonWater.OnClick := {$ifdef FPC}@{$endif} ClickLayer;
  ButtonContour.OnClick := {$ifdef FPC}@{$endif} ClickLayer;
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

  RedSlider.Value := trunc(ColorPreview.Color.X *15);
  GreenSlider.Value := trunc(ColorPreview.Color.Y *15);
  BlueSlider.Value := trunc(ColorPreview.Color.Z *15);
  AlphaSlider.Value := trunc(ColorPreview.Color.W *15);

  Viewport1.OnMotion := {$ifdef FPC}@{$endif} HandleMotion;

  ClickCreateClient( self );
  MainNavigation.Input_Jump.Assign(keyNone);
  MainNavigation.Input_Crouch.Assign(keyNone);
  MainNavigation.OnMoveAllowed := {$ifdef FPC}@{$endif} MoveAllowed;
  LabelContourScale.Caption := '';
  LabelGridScale.Caption := '';

  ButtonBrush.OnClick := {$ifdef FPC}@{$endif} ClickTool;
  ButtonDig.OnClick := {$ifdef FPC}@{$endif} ClickTool;
  ButtonPile.OnClick := {$ifdef FPC}@{$endif} ClickTool;

  MarkupLayer := TMarkupLayer.Create(Viewport1);
  MarkupLayer.terrainheight := {$ifdef fpc}@{$endif}HeightAboveTerrain;
  TerrainLayer.Add(MarkupLayer);
  GMarkupLayer := MarkupLayer;

  TexturePreview1.Color := vector4(1,1,1,0.5);
  TexturePreview1.AlphaChannel := acAuto;

  TexturePreview2.Color := vector4(1,1,1,0.5);
  TexturePreview2.AlphaChannel := acAuto;

  ColorPicker.Exists := false;
  ColorSliderChange( self );
  UpdatePositionIndicator;

  glGetIntegerv(GL_MAX_VERTEX_UNIFORM_COMPONENTS, @MaxVertexUniformComponents);
  dbgwriteln(format('GL_MAX_VERTEX_UNIFORM_COMPONENTS: %d', [MaxVertexUniformComponents]));
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(FClient);
  inherited;
end;

procedure TViewMain.UpdatePositionIndicator;
 var terrainh : single;
     agl : single;
 begin
   TerrainHeight( MainCamera.Translation, terrainh );
   agl := MainCamera.Translation.Y - terrainh;
   PositionLabel.Caption := Format( '%f x %)', [MainCamera.Translation.X, MainCamera.Translation.Z] );
   ElevationLabel.Caption := Format( '%f', [TerrainH] );
   AltitudeLabel.Caption := Format( '%f', [MainCamera.Translation.Y] );
   ElevationIndicator.Height := 8 + TerrainH;
   AltitudeIndicator.Translation := vector2( 0, ElevationIndicator.Height - 2 + agl );
 end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
 var fps : single;
 begin
  inherited;
  if connectionstatus = status_connecting then
   begin
     connectiontimeout := connectiontimeout + secondspassed;
     if connectiontimeout > 1 then
      begin
        dbgwriteln( 'Terrain server at '+GDefaulthost+':'+inttostr(GDefaultPort)+' not found.' );
        if FClient <> nil then
        begin
          connectionstatus := status_disconnected;
          SetCreateClientMode( connectionstatus, ButtonCreateClient );
          FreeAndNil(FClient);
        end
      end;
   end;
  MainNavigation.MouseLook := {( GActiveDrag = self ) and} ( buttonMiddle in Container.MousePressed );
  fps := Container.Fps.RealFps;
  FPSLabel.Caption := Format('%2.0ffps', [fps]);
  if fps >= 60 then
     FPSLabel.Color := vector4( 0, 1, 0, 1  )
  else
  if fps >= 30 then
     FPSLabel.Color := Vector4( 1, 1, 0, 1 )
  else
     FPSLabel.Color := Vector4( 1, 0, 0, 1 )
end;

procedure TViewMain.HandleConnected;
begin
  DbgWriteln('Connected to terrain server.');
  ClickSend( self );
  connectionstatus := status_connected;
  SetCreateClientMode( connectionstatus, ButtonCreateClient );
  ButtonSend.Enabled := FClient <> nil;
end;

procedure TViewMain.HandleDisconnected;
begin
  DbgWriteln('Disconnected from terrain server.');
  ButtonSend.Enabled := false;
  connectionstatus := status_disconnected;
  SetCreateClientMode( connectionstatus, ButtonCreateClient );
  if assigned( fclient ) then
     FreeAndNil( fClient );
end;

procedure TViewMain.HandleMessageReceived(const AMessage: String);
begin
  DbgWriteln('Received message: ' + AMessage);
end;

procedure TViewMain.ClickCreateClient(Sender: TObject);
begin
  case connectionstatus of
    status_disconnected :
    begin
      DBGWriteln( 'Connecting to terrain server at '+GDefaultHost + ':' + IntToStr( GDefaultPort ) +'...');
      FClient := TTerClient.Create;
      FClient.Hostname := GDefaultHost;
      FClient.Port := GDefaultPort;

      FClient.OnConnected := {$ifdef FPC}@{$endif} HandleConnected;
      FClient.OnDisconnected := {$ifdef FPC}@{$endif} HandleDisconnected;
      FClient.OnMessageReceived := {$ifdef FPC}@{$endif} HandleMessageReceived;
      FClient.fOnTileReceived :=  {$ifdef FPC}@{$endif}HandleTileReceived;
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

 procedure CreateWaterMesh( Layer : TCastleTransform;
                            Tile : TTerTile );
  begin
    if assigned( Tile.WaterGraphics ) then
     begin
       Layer.Remove( Tile.WaterGraphics );
       Tile.WaterGraphics.Free;
     end;
    Tile.WaterGraphics := TWaterMesh.Create2( Layer, Tile );
    Layer.Add( Tile.WaterGraphics );
  end;

 procedure CreateTerrainMesh( Layer : TCastleTransform;
                              Tile : TTerTile );
  begin
    if assigned( Tile.TerrainGraphics ) then
     begin
       Layer.Remove( Tile.TerrainGraphics );
       Tile.TerrainGraphics.Free;
     end;
    Tile.TerrainGraphics := TTerrainMesh.Create2( Layer, Tile );
    Layer.Add( Tile.TerrainGraphics );
  end;

procedure TViewMain.HandleTileReceived( const msginfo : TMsgHeader;
                                        const tileinfo : TTileHeader;
                                        tilegrid : TSingleGrid;
                                        watergrid : TSingleGrid;
                                        floragrid : TSingleGrid;
                                        texgrid : TTexPoints );
 var tile : TTerTile;
     x,y : integer;
     waterh, terrainh, florah : psingle;
     h : single;
 begin
   Tile := GTileList.GetInitTile( tileinfo );
   if assigned( tile ) then case msginfo.msgtype of
     msg_water : begin  { obsolete in favor of msg_water2? }
                    if ( not assigned( Tile.WaterGraphics )) or
                       ( Tile.Info.TileSz <> tileInfo.TileSz ) then
                     begin
                       Tile.Info := TileInfo;
                       CreateWaterMesh( WaterLayer, Tile );
                     end;
                    { update tile graphics }
                    TTerrainMesh( Tile.WaterGraphics ).UpdateFromGrid( TileGrid );
                    TTerrainMesh( Tile.WaterGraphics ).UpdateVerticesTexture( texgrid );
                  end;
     msg_tile : begin
                  if ( not assigned( Tile.TerrainGraphics )) or
                     ( Tile.Info.TileSz <> tileInfo.TileSz ) then
                   begin
                     Tile.Info := TileInfo;
                     CreateTerrainMesh( TerrainLayer, Tile );
                   end;
                  { update tile graphics }
                  TTerrainMesh( Tile.TerrainGraphics ).UpdateFromGrid( TileGrid );
                end;
      msg_water2 : begin
                     assert( assigned( tilegrid ) and assigned( watergrid ) and assigned( floragrid ));
                     waterh := watergrid.ptrix(0);
                     terrainh := tilegrid.ptrix(0);
                     florah := floragrid.ptrix(0);
                     setlength( texgrid, watergrid.wxh );
                     assert( watergrid.wxh = tilegrid.wxh );
                     for x := 0 to watergrid.wh - 1 do
                      for y := 0 to watergrid.wh - 1 do
                         begin
                           h := waterh^;
                           texgrid[ y * watergrid.wh + x ] := CalcDepthAndTexture( h, florah^, terrainh^, true );
                           waterh^ := terrainh^ + h;
                           { calculate water texture index }
                           inc( waterh );
                           inc( terrainh );
                           inc( florah );
                         end;
                     if ( not assigned( Tile.WaterGraphics )) or
                       ( Tile.Info.TileSz <> tileInfo.TileSz ) then
                      begin
                        Tile.Info := TileInfo;
                        CreateWaterMesh( WaterLayer, Tile );
                      end;
                     TTerrainMesh( Tile.WaterGraphics ).UpdateFromGrid( waterGrid );
                     TTerrainMesh( Tile.WaterGraphics ).UpdateVerticesTexture( texgrid );
                   end;
      msg_splat : begin
                    if assigned( tile.TerrainGraphics ) then
                       TTerrainMesh( Tile.TerrainGraphics ).UpdateSplatmap( tilegrid )
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
    end;
   activetool := tool_none;
   if buttonbrush.pressed then
      activetool := tool_brush
   else
   if buttondig.pressed then
      activetool := tool_dig
   else
   if buttonpile.pressed then
      activetool := tool_pile;
   ColorPicker.Exists := activetool = tool_brush;
   ColorPicker.Translation := vector2( 80, 0 );
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
   result := true;
 end;

function TViewMain.HeightAboveTerrain(Pos: TVector3;
                                      out Y: Single;
                                      heighttype : byte = 0 ): boolean;
 begin
   TerrainHeight( MainCamera.Translation, Y );
   result := true
 end;

procedure TViewMain.UseTool( const pos : tvector3 );
 var params : string;
 begin
   params := FormatFloat( '0.###', pos.x )+','+FormatFloat( '0.###', pos.z );
   case activetool of
      tool_none : exit;
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
