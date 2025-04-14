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
  CastleKeysMouse, CastleClientServer, CastleTerrain,
  CastleViewport, CastleCameras, CastleTransform,
  TerServerCommon, TerrainData, TerrainParams,
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
    ButtonDestroyClient: TCastleButton;
    EditSend: TCastleEdit;
    ButtonSend: TCastleButton;
    Viewport1 : TCastleViewport;
    MainNavigation : TCastleWalkNavigation;
    MainCamera : TCastleCamera;
  private
    FClient: TTerClient;
    procedure HandleConnected;
    procedure HandleDisconnected;
    procedure HandleMessageReceived(const AMessage: String);
    procedure HandleTileReceived( const msginfo : TMsgHeader;
                                        tile  : TTerTile;
                                  const tilemesh : TTerrainMesh );
    procedure ClickCreateClient(Sender: TObject);
    procedure ClickDestroyClient(Sender: TObject);
    procedure ClickSend(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    procedure mousewheel( direction : integer );
    function Press(const Event: TInputPressRelease): boolean; override;
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
  ButtonCreateClient.OnClick := {$ifdef FPC}@{$endif} ClickCreateClient;
  ButtonDestroyClient.OnClick := {$ifdef FPC}@{$endif} ClickDestroyClient;
  ButtonSend.OnClick := {$ifdef FPC}@{$endif} ClickSend;
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(FClient);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  MainNavigation.MouseLook := {( GActiveDrag = self ) and} ( buttonMiddle in Container.MousePressed );
end;

procedure TViewMain.HandleConnected;
begin
  DbgWriteln('Connected to server');
  ButtonCreateClient.Enabled := FClient = nil;
  ButtonDestroyClient.Enabled := FClient <> nil;
  ButtonSend.Enabled := FClient <> nil;
end;

procedure TViewMain.HandleDisconnected;
begin
  DbgWriteln('Disconnected from server');
  ButtonCreateClient.Enabled := true;
  ButtonDestroyClient.Enabled := false;
  ButtonSend.Enabled := false;
end;

procedure TViewMain.HandleMessageReceived(const AMessage: String);
begin
  DbgWriteln('Received message: ' + AMessage);
end;

procedure TViewMain.HandleTileReceived( const msginfo : TMsgHeader;
                                              tile : TTerTile;
                                        const tilemesh : TTerrainMesh );
 var Mesh : TTerrainMesh;
 begin
   Mesh := TTerrainMesh( Tile.Graphics );
   if assigned( Mesh ) then
      Mesh.Free;
   { update tile graphics }
   Tile.Graphics := tilemesh;
   Viewport1.items.Add( tilemesh );
 end;

procedure TViewMain.ClickCreateClient(Sender: TObject);
begin
  FClient := TTerClient.Create;
  FClient.Hostname := 'localhost';
  FClient.Port := 10244;

  FClient.OnConnected := {$ifdef FPC}@{$endif} HandleConnected;
  FClient.OnDisconnected := {$ifdef FPC}@{$endif} HandleDisconnected;
  FClient.OnMessageReceived := {$ifdef FPC}@{$endif} HandleMessageReceived;
  FClient.fOnTileReceived :=  {$ifdef FPC}@{$endif}HandleTileReceived;

  MainNavigation.Input_Jump.Assign(keyNone);
  MainNavigation.Input_Crouch.Assign(keyNone);

  FClient.Connect;
end;

procedure TViewMain.ClickDestroyClient(Sender: TObject);
begin
  if FClient <> nil then
  begin
    FClient.Disconnect;
    FreeAndNil(FClient);
  end;
end;

procedure TViewMain.ClickSend(Sender: TObject);
begin
  FClient.Send(EditSend.Text);
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
      (*HeightAboveTerrain( Pos, TerrainH, ht_road );
      if h - MainNavigation.radius < TerrainH then
       begin
         pos.y := TerrainH + MainNavigation.radius + 0.01 ;
         MainNavigation.MoveHorizontalSpeed := 0.1;
       end
      else
       begin*)
         amt := h - TerrainH;
         { if no tools are taking mousewheel then use it to change view height }
         delta := direction/3 * amt;
         h := h + delta;
         //limitmax( h, MaxHeight );
         pos.Y := h;
         MainNavigation.MoveHorizontalSpeed := sqrt(amt);
(*       end;*)
      MainCamera.Translation := pos;
//      ViewUpdated;
(*    end;*)
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
