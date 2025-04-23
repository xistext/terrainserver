{
  Copyright 2018-2024 Benedikt Magnus, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  idGlobal,
  CastleVectors, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleClientServer, CastleTerrain, CastleScene,
  CastleViewport, CastleCameras, CastleTransform,
  TerServerCommon, TerrainData, TerrainParams, TerrainShader, TerrainMesh,
  watergrid, BaseMesh, TerrainClient,
  debug;

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
    PosLabel : TCastleLabel;
    ButtonContour : TCastleButton;
    ButtonGrid    : TCastleButton;
    LabelGridScale : TCastleLabel;
    LabelContourScale : TCastleLabel;

    ColorPreview : TCastleShape;
    RedSlider : TCastleIntegerSlider;
    GreenSlider : TCastleIntegerSlider;
    BlueSlider : TCastleIntegerSlider;
    AlphaSlider : TCastleIntegerSlider;

  private
    FClient: TTerClient;
    procedure HandleConnected;
    procedure HandleDisconnected;
    procedure HandleMessageReceived(const AMessage: String);
    procedure HandleTileReceived( const msginfo : TMsgHeader;
                                  const tileinfo : TTileHeader;
                                  tilegrid : TSingleGrid );
    procedure ClickCreateClient(Sender: TObject);
    procedure ClickTest(Sender: TObject);
    procedure ClickSend(Sender: TObject);
    procedure ColorSliderChange( sender : TObject );
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    procedure mousewheel( direction : integer );
    function Press(const Event: TInputPressRelease): boolean; override;
    procedure TerrainHeight( const pos : tvector3; var h : single );
    function MoveAllowed(const Sender: TCastleNavigation;
                         const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
                         const Radius: Single; const BecauseOfGravity: Boolean): boolean;
    procedure HandleMotion(const Sender: TCastleUserInterface; const Event: TInputMotion; var Handled: Boolean);
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  GParentComponent := Viewport1.Items;
  ButtonCreateClient.OnClick := {$ifdef FPC}@{$endif} ClickCreateClient;
  ButtonGrid.OnClick := {$ifdef FPC}@{$endif} ClickTest;
  ButtonContour.OnClick := {$ifdef FPC}@{$endif} ClickTest;
  ButtonSend.OnClick := {$ifdef FPC}@{$endif} ClickSend;

  RedSlider.OnChange := {$ifdef FPC}@{$endif} ColorSliderChange;
  BlueSlider.OnChange := {$ifdef FPC}@{$endif} ColorSliderChange;
  GreenSlider.OnChange := {$ifdef FPC}@{$endif} ColorSliderChange;
  AlphaSlider.OnChange := {$ifdef FPC}@{$endif} ColorSliderChange;

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
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(FClient);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var terrainh : single;
    agl : single;
begin
  inherited;
  MainNavigation.MouseLook := {( GActiveDrag = self ) and} ( buttonMiddle in Container.MousePressed );
  TerrainHeight( MainCamera.Translation, terrainh );
  agl := MainCamera.Translation.Y - terrainh;
  PosLabel.Caption := Format( '(%f, %f) %f/%f', [MainCamera.Translation.X, MainCamera.Translation.Z, agl, TerrainH] );
end;

procedure TViewMain.HandleConnected;
begin
  DbgWriteln('Connected to server');
  ClickSend( self );
  ButtonCreateClient.Caption := 'Disconnect';
  ButtonSend.Enabled := FClient <> nil;
end;

procedure TViewMain.HandleDisconnected;
begin
  DbgWriteln('Disconnected from server');
  ButtonCreateClient.Caption := 'Connect';
  ButtonSend.Enabled := false;
  if assigned( fclient ) then
     FreeAndNil( fClient );
end;

procedure TViewMain.HandleMessageReceived(const AMessage: String);
begin
  DbgWriteln('Received message: ' + AMessage);
end;

procedure TViewMain.ClickCreateClient(Sender: TObject);
begin
  if FClient <> nil then
  begin
    FClient.Disconnect;
    FreeAndNil(FClient);
  end
  else
   begin
     FClient := TTerClient.Create;
     FClient.Hostname := 'localhost';
     FClient.Port := 10244;

     FClient.OnConnected := {$ifdef FPC}@{$endif} HandleConnected;
     FClient.OnDisconnected := {$ifdef FPC}@{$endif} HandleDisconnected;
     FClient.OnMessageReceived := {$ifdef FPC}@{$endif} HandleMessageReceived;
     FClient.fOnTileReceived :=  {$ifdef FPC}@{$endif}HandleTileReceived;
     try
       FClient.Connect;
     except
       FreeAndNil( FClient );
     end;
   end;
end;


procedure TViewMain.HandleTileReceived( const msginfo : TMsgHeader;
                                        const tileinfo : TTileHeader;
                                        tilegrid : TSingleGrid );
 var tile : TTerTile;
     tilemesh : TTerrainMesh;
 begin
   Tile := GTileList.GetInitTile( tileinfo );
   if assigned( Tile.Graphics ) then
    begin
      Viewport1.Items.Remove( Tile.Graphics );
      Tile.Graphics.Free;
    end;
   { update tile graphics }
   tilemesh := TTerrainMesh.create2( Viewport1, Tile );
   tilemesh.UpdateFromGrid( TileGrid );
   TileGrid.Free;

   Tile.Graphics := tilemesh;
   Viewport1.Items.Add( tilemesh );
 end;


procedure TViewMain.ClickTest(Sender: TObject);
 var button : TCastleButton;
 var tile : TTerTile;
var i : integer;
   shader : TTileShader;
 begin
   button := TCastleButton( Sender );
   Button.Pressed := not Button.Pressed;
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
    end;
  { test }
  for i := 0 to gtilelist.Count - 1 do
   begin
     Tile := TTerTile( gTileList.At( i ));
     if assigned( Tile.graphics ) then
        TTerrainMesh( Tile.graphics ).UpdateAppearance;
   end;
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

 end;

procedure TViewMain.TerrainHeight( const pos : tvector3; var h : single );
 var atile : ttertile;
     pos2 : tvector2;
 begin
   h := -1;
   pos2 := Vector2(Pos.X,Pos.Z);
   if gtilelist.findtileatlocation( pos2, atile ) and assigned( atile.Graphics ) then
    begin
//      dbgwriteln( inttostr( atile.Info.TileX )+','+inttostr( atile.Info.TileY ));
      TTerrainMesh( atile.graphics ).Elevationatpos( pos2, h );
    end;
 (*
   if assigned( atile ) then
      Y := aTile.Data.Height( Pos2, Vector2(0,0));
   else
    begin
      {!!! if no tile then use the tterrainnoise }
    end;     *)
 end;

function TViewMain.MoveAllowed(const Sender: TCastleNavigation;
                               const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
                               const Radius: Single; const BecauseOfGravity: Boolean): boolean;
 var h : single;
     terrainh : single;
 begin
   h := ProposedNewPos.Y;
   TerrainHeight( ProposedNewPos, TerrainH );
   NewPos := ProposedNewPos;
   if h - radius < Terrainh then
      NewPos := Vector3(ProposedNewPos.X, TerrainH + radius, ProposedNewPos.Z );
   result := true;
 end;

procedure TViewMain.HandleMotion(const Sender: TCastleUserInterface;
                                 const Event: TInputMotion;
                                 var Handled: Boolean);
 var rayitems : TRayCollision;
     node : TRayCollisionNode;
     c, i : integer;
     ItemHit : TCastleTransform;
     TerMesh : TTerrainMesh;
 begin
   rayitems := Viewport1.MouseRayHit;
   if assigned( rayitems ) then
     begin
      c := RayItems.Count;
      for i := c - 1 downto 0 do
       begin
         node := RayItems[i];
         ItemHit := node.Item;
         if ItemHit is TTerrainMesh then
          begin
            TerMesh := TTerrainMesh( ItemHit );

//             poslabel.Caption := Format( '(%f, %f) %f' , [node.Point.X, node.point.z, node.point.y] );

            break;
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
         h := MainNavigation.radius + TerrainH;
         MainNavigation.MoveHorizontalSpeed := 0.1;
       end;
      pos.Y := h;
      MainNavigation.MoveHorizontalSpeed := sqrt(amt);
//      ViewUpdated;
    MainCamera.Translation := pos;
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
