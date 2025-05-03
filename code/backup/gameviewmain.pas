{ Terrain Server

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
  CastleVectors, CastleWindow, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleClientServer, CastleNotifications,
  TerServerCommon, TerrainData, TerrainCommand,
  WaterFlow,
  liveTime,
  debug;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    EditPort: TCastleIntegerEdit;
    ButtonCreateServer: TCastleButton;
    ButtonDestroyServer: TCastleButton;
    EditSend: TCastleEdit;
    ButtonSend: TCastleButton;
    VerticalGroup2 : TCastleVerticalGroup;
    ConnectedIndicator : TCastleShape;
    ClientsLabel : TCastleLabel;
    TilesLabel : TCastleLabel;
    FlowLabel : TCastleLabel;
    ButtonFlow : TCastleButton;
    ButtonDeleteTiles : TCastleButton;
    TexturePreview : TCastleImageControl;
  private
    FServer: TCastleTCPServer;
    procedure HandleConnected(AClient: TClientConnection);
    procedure HandleDisconnected(AClient: TClientConnection);
    procedure HandleMessageReceived(const AMessage: String; AClient: TClientConnection);
    procedure ClickCreateServer(Sender: TObject);
    procedure ClickDestroyServer(Sender: TObject);
    procedure ClickSend(Sender: TObject);
    procedure ClickButtonFlow( Sender : TObject );
    procedure ClickButtonDeleteTiles( Sender : TObject );
    procedure SendStringToClient( AMessage : string;
                                  AClient : TClientConnection );

    procedure HandleCommandCallback( Msg : string );

  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    procedure Notification( Msg : string );
  end;

var
  ViewMain: TViewMain;

var lastclient : TClientConnection; { !kludge to test pushing water }

implementation

uses SysUtils,
  CastleUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
  TexturePreview.Color := vector4(0,0,0,0.5);
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonCreateServer.OnClick := {$ifdef FPC}@{$endif} ClickCreateServer;
  ButtonDestroyServer.OnClick := {$ifdef FPC}@{$endif} ClickDestroyServer;
  ButtonSend.OnClick := {$ifdef FPC}@{$endif} ClickSend;
  ButtonFlow.OnClick := {$ifdef FPC}@{$endif} ClickButtonFlow;
  ButtonDeleteTiles.OnClick := {$ifdef FPC}@{$endif} ClickButtonDeleteTiles;

  ClickCreateServer( self );
  ConnectedIndicator.exists := true;
  Container.UIScaling := usNone;
  EditPort.Enabled := false;
end;

procedure TViewMain.Stop;
begin
  EditPort.Enabled := false;
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
 var connected : boolean;
     task : TTaskItem;
     atile : ttertile;
     flowdelta : single;
     astr : string;
 const connectstatus : integer = -1;
       clientcount   : integer = -1;
 begin
  inherited;
  UpdateGameTime( SecondsPassed );
  { This virtual method is executed every frame (many times per second). }
  TilesLabel.Caption := IntToStr( GTileList.Count )+' tiles';
  connected := FServer <> nil;
  astr := '';

  if flowrunning and ( flowcounter > 0 ) then
   begin
     flowdelta := GameTime - FlowStartTime;
     if flowdelta > 0 then
       astr := Format( '%0f/sec', [flowcounter / flowdelta] );
   end
  else
     astr := 'stopped';
  FlowLabel.caption := astr;

  if ord( connected ) <> connectstatus then
   begin
     case connected of
       false : begin
             ConnectedIndicator.Color := vector4(0.5,0,0,1);
             ButtonCreateServer.Enabled := true;
             ButtonDestroyServer.Enabled := false;
             ButtonSend.Enabled := false;
           end;
       true : begin
             ConnectedIndicator.Color := vector4(0,0.5,0,1);
             ButtonCreateServer.Enabled := false;
             ButtonDestroyServer.Enabled := true;
             ButtonSend.Enabled := true;
           end;
       end;
     connectstatus := ord( connected );
   end;
  if connected  then
   begin
     if ( FServer.Connections <> clientcount ) then
      begin
        clientcount := FServer.Connections;
        ClientsLabel.Caption := IntToStr( clientcount ) + ' clients';
      end;
   end
  else
  if ClientCount <> 0 then
    begin
      clientcount := 0;
      ClientsLabel.Caption := IntToStr( clientcount ) + '0 clients';
    end;
  task := GTaskList.Pop;
  while assigned( task ) do
   begin
     task.RunTask;
//     Application.ProcessMessage( false, false );
     task := GTaskList.Pop;
   end;

  if assigned( lastclient.context ) and WaterFlowThreads[0].DirtyTileList.nexttile( atile  ) then
   begin
     waterArea( lastclient,
                atile.info.tilex, atile.info.tiley,
                0, {$ifdef fpc}@{$endif} HandleCommandCallback );
   end;


end;

procedure TViewMain.HandleConnected(AClient: TClientConnection);
begin
  LastClient := AClient;{!}
  Notification('Client connected');
end;

procedure TViewMain.HandleDisconnected(AClient: TClientConnection);
begin
  Notification('Client disconnected');
  GTaskList.removeclient( AClient ); { remove any tasks queued by the client }
end;

procedure TViewMain.HandleMessageReceived(const AMessage: String; AClient: TClientConnection);
begin
  Notification('Client cmd: ' + AMessage);
  { prcess command }
  GCmdList.executecommand( AClient, AMessage, {$ifdef FPC}@{$endif}HandleCommandCallback );
end;

procedure TViewMain.HandleCommandCallback( Msg : string );
 begin
(*   if msg <> '' then
     dbgwrite( Msg +'. ' );*)
   Application.ProcessMessage( false, false );
 end;

procedure TViewMain.SendStringToClient( AMessage : string;
                                        AClient : TClientConnection );
 var h : TMsgHeader;
 begin
   h.requestid := 0;
   h.msgtype := msg_string;
   h.msglen := 0;
   AClient.Send( h, sizeof( h ));
   FServer.SendToClient( AMessage, AClient );
 end;

procedure TViewMain.ClickCreateServer(Sender: TObject);
var filecount : integer;
begin
  FServer := TCastleTCPServer.Create;
  FServer.Port := EditPort.Value;

  FServer.OnConnected := {$ifdef FPC}@{$endif} HandleConnected;
  FServer.OnDisconnected := {$ifdef FPC}@{$endif} HandleDisconnected;
  FServer.OnMessageReceived := {$ifdef FPC}@{$endif} HandleMessageReceived;
  dbgwriteln( 'Reading tiles...' );
  FileCount := GTileList.ReadAllTerrainFiles( 'castle-data:/terrain' );

  FServer.Start;
  dbgwriteln( inttostr( filecount )+' tiles read' );

//
  Notification( 'Started' );
end;

procedure TViewMain.ClickDestroyServer(Sender: TObject);
begin
  if FServer <> nil then
  begin
    FServer.Stop;
    FreeAndNil(FServer);
    Notification( 'Stopped server.' );
  end;
end;

procedure TViewMain.ClickSend(Sender: TObject);
begin
  FServer.SendToAll(EditSend.Text);
end;

procedure TViewMain.ClickButtonFlow( Sender : TObject );
 begin
   ButtonFlow.Pressed := not ButtonFlow.Pressed;
   if ButtonFlow.Pressed then
    begin
      Notification( 'Start water flow threads.' );
      StartWaterFlowThreads;
    end
   else
    begin
      Notification( 'Stop water flow threads.' );
      StopWaterFlowThreads;
    end;
 end;

procedure TViewMain.ClickButtonDeleteTiles( Sender : TObject );
 var i : integer;
     atile : ttertile;
 begin
   if ButtonFlow.Pressed then
      ClickButtonFlow( self ); { turn off water flow }
   { delete all tiles on disk }
   for i := 0 to gtilelist.count - 1 do
    begin
      atile := ttertile( gtilelist.at( i ));
      atile.DeleteMyFiles;
    end;
   gtilelist.freeall;
   Notification('Cleared terrain.');
   assert( WaterFlowThreads[0].TaskList.Count = 0 );
 end;

procedure TViewMain.Notification( Msg : string );
 var alabel : TCastleLabel;
 begin
   alabel := TCastleLabel.Create( VerticalGroup2 );
   alabel.FontSize := 12;
   alabel.AutoSize := true;
   alabel.Color := Vector4(1,1,1,1);
   alabel.caption := FormatDateTime('yyyy.mm.dd hh:mm:ss', now )+ ' ' + Msg;
   VerticalGroup2.InsertBack( alabel );
 end;

end.
