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
     TTerData = class( TCastleTerrainData )

       procedure DataModified;
       function Height(const Coord, TexCoord: TVector2): Single; override;

     end;


  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    EditHostname: TCastleEdit;
    EditPort: TCastleIntegerEdit;
    ButtonCreateClient: TCastleButton;
    ButtonChangeViewMode: TCastleButton;
    EditSend: TCastleEdit;
    ButtonSend: TCastleButton;
    Viewport1 : TCastleViewport;
    MainNavigation : TCastleWalkNavigation;
    MainCamera : TCastleCamera;
    PosLabel : TCastleLabel;
  private
    FClient: TTerClient;
    procedure HandleConnected;
    procedure HandleDisconnected;
    procedure HandleMessageReceived(const AMessage: String);
    procedure HandleTileReceived( const msginfo : TMsgHeader;
                                        tile  : TTerTile;
                                  const tilemesh : TTerrainMesh );
    procedure ClickCreateClient(Sender: TObject);
    procedure ClickTest(Sender: TObject);
    procedure ClickSend(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    procedure mousewheel( direction : integer );
    function Press(const Event: TInputPressRelease): boolean; override;
    procedure TerrainHeight( const pos : tvector3; var h : single );
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleUtils;

procedure TTerData.DataModified;
 begin
   DoChange;
 end;

function TTerData.Height(const Coord, TexCoord: TVector2): Single;
 begin
   result := 0;

 end;

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
  ButtonChangeViewMode.OnClick := {$ifdef FPC}@{$endif} ClickTest;
  ButtonSend.OnClick := {$ifdef FPC}@{$endif} ClickSend;
  ClickCreateClient( self );
  MainNavigation.Input_Jump.Assign(keyNone);
  MainNavigation.Input_Crouch.Assign(keyNone);
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
                                              tile : TTerTile;
                                        const tilemesh : TTerrainMesh );
 var c : TCastleCollider;
 begin
   if assigned( Tile.Graphics ) then
    begin
      Viewport1.Items.Remove( TTerrainMesh( Tile.Graphics ));
      TTerrainMesh( Tile.Graphics ).Free;
    end;
   { update tile graphics }
   Tile.Graphics := tilemesh;
(*         C := tilemesh.FindBehavior(TCastleCollider) as TCastleCollider;
       if C <> nil then
         C.InternalTransformChanged(tilemesh);*)

   Viewport1.Items.Add( tilemesh );
 end;


procedure TViewMain.ClickTest(Sender: TObject);
 var tile : TTerTile;

 var i : integer;
     shader : TTileShader;
     shadername : string;
begin
  gshaderid := ( gshaderid + 1 ) mod 7;
  { test }
  shadername := '';
  for i := 0 to gtilelist.Count - 1 do
   begin
     Tile := TTerTile( gTileList.At( i ));
     if assigned( Tile.graphics ) then
      begin
        shader := gettileshader( tile, gshaderid );
        shadername := shader.name;
        TTerrainMesh( Tile.graphics ).UpdateAppearance( shader );
        shader.free;
      end;
   end;
  ButtonChangeViewMode.Caption := shadername;
end;

procedure TViewMain.ClickSend(Sender: TObject);
begin
  FClient.Send(EditSend.Text);
end;

procedure TViewMain.TerrainHeight( const pos : tvector3; var h : single );
 var atile : ttertile;
 begin
   h := -1;
   if gtilelist.findtileatlocation( Vector2(Pos.X,Pos.Z), atile ) and assigned( atile.Graphics ) then
      TTerrainMesh( atile.graphics ).Elevationatpos( vector2( pos.x, pos.z ), h );
 (*
   if assigned( atile ) then
      Y := aTile.Data.Height( Pos2, Vector2(0,0));
   else
    begin
      {!!! if no tile then use the tterrainnoise }
    end;     *)
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
