{ Terrain Server
  This is based on the Castle Game Engine indy socket server demo.
  erik@edj.net
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  idGlobal,
  Windows, { for allocconsole to view debug }
  CastleVectors, CastleWindow, CastleComponentSerialize, CastleLog, CastleControls,
  CastleUIControls, CastleKeysMouse, CastleClientServer, CastleNotifications,
  ClientList,
  TerServerCommon, TerrainData, TerrainCommand,
  WaterFlow,
  liveTime;

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
    VerticalGroup2 : TCastleVerticalGroup;
    ConnectedIndicator : TCastleShape;
    ClientsLabel : TCastleLabel;
    TilesLabel : TCastleLabel;
    FlowLabel : TCastleLabel;
    ButtonFlow : TCastleButton;
    ButtonDeleteTiles : TCastleButton;
  private
    FServer: TCastleTCPServer;
    procedure HandleConnected(AClient: TClientConnection);
    procedure HandleDisconnected(AClient: TClientConnection);
    procedure HandleMessageReceived(const AMessage: String; AClient: TClientConnection);
    procedure ClickCreateServer(Sender: TObject);
    procedure ClickDestroyServer(Sender: TObject);
    procedure ClickButtonFlow( Sender : TObject );
    procedure ClickButtonDeleteTiles( Sender : TObject );
    procedure SendStringToClient( AMessage : string;
                                  AClient : TClientConnection );
    procedure HandleCommandCallback( Msg : string );
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;

    procedure StartServer;
    procedure StopServer;
    procedure StopServerAndExit;

    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    procedure ShowNotification( Msg : string );

    procedure CloseQuery(AContainer: TCastleContainer);
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
  Application.MainWindow.OnCloseQuery := {$ifdef FPC}@{$endif} CloseQuery;
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonCreateServer.OnClick := {$ifdef FPC}@{$endif} ClickCreateServer;
  ButtonDestroyServer.OnClick := {$ifdef FPC}@{$endif} ClickDestroyServer;
  ButtonFlow.OnClick := {$ifdef FPC}@{$endif} ClickButtonFlow;
  ButtonDeleteTiles.OnClick := {$ifdef FPC}@{$endif} ClickButtonDeleteTiles;
  InitializeLog;
  StartServer;
  ConnectedIndicator.exists := true;
  Container.UIScaling := usNone;
  EditPort.Enabled := false;
end;

   procedure updatewaterclient( Client : TTileClient; Tile : TTerTile; LOD : integer; data : pointer );
    begin
     buildwaterArea( Client,
                     tile.tilex, tile.tiley,
                     0, LOD, {$ifdef fpc}@{$endif}  TViewMain( data ).HandleCommandCallback );

    end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
 var connected : boolean;
     task : TTaskItem;
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
       astr := Format( '%0f', [flowcounter / flowdelta] );
   end
  else
     astr := 'off';
  FlowLabel.caption := astr;

  if ord( connected ) <> connectstatus then
   begin
     ButtonCreateServer.Enabled := not connected;
     ButtonDestroyServer.Enabled := connected;
     if connected then
        ConnectedIndicator.Color := vector4(0,0.5,0,1)
     else
        ConnectedIndicator.Color := vector4(0.5,0,0,1);
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
//     Application.ProcessMessage( false, false );!!! breathing here can cause stack overflow.  messages are also checked during command callback, so won't choke
     task := GTaskList.Pop;
   end;

  GClientList.IterateSubscriptions( {$ifdef FPC}@{$endif}updatewaterclient, self );
end;

procedure TViewMain.HandleConnected(AClient: TClientConnection);
begin
  GClientList.getsubscriber( AClient );
  ShowNotification('Client connected');
end;

procedure TViewMain.HandleDisconnected(AClient: TClientConnection);
var success : boolean;
    TileClient : TTileClient;
begin
  ShowNotification('Client disconnected');
  TileClient := GClientList.getsubscriber( AClient );
  GTaskList.removeclient( TileClient ); { remove any tasks queued by the client }
  success := GClientList.removesubscriber(AClient);
  assert( success );
end;

procedure TViewMain.HandleMessageReceived(const AMessage: String; AClient: TClientConnection);
var TileClient : TTileClient;
begin
  ShowNotification('Client cmd: ' + AMessage);
  { prcess command }
  TileClient :=  GClientList.getsubscriber( AClient );
  GCmdList.executecommand( TileClient, AMessage, {$ifdef FPC}@{$endif}HandleCommandCallback );
end;

procedure TViewMain.HandleCommandCallback( Msg : string );
 begin
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

procedure TViewMain.StartServer;
 var filecount : integer;
 begin
  FServer := TCastleTCPServer.Create;
  FServer.Port := EditPort.Value;
  FServer.OnConnected := {$ifdef FPC}@{$endif} HandleConnected;
  FServer.OnDisconnected := {$ifdef FPC}@{$endif} HandleDisconnected;
  FServer.OnMessageReceived := {$ifdef FPC}@{$endif} HandleMessageReceived;
  FileCount := GTileList.ReadAllTerrainFiles( 'castle-data:/terrain' );
  ShowNotification( inttostr( filecount )+' tiles read.' );
  FServer.Start;
  ShowNotification( 'Started server.' );
 end;

procedure TViewMain.StopServer;
 begin
   if FServer <> nil then
    begin
      if FServer.IsConnected then
         FServer.Stop;
      FreeAndNil(FServer);
      ShowNotification( 'Stopped server.' );
    end;
 end;

procedure TViewMain.ClickCreateServer(Sender: TObject);
begin
  StartServer;
end;

procedure TViewMain.ClickDestroyServer(Sender: TObject);
begin
  StopServer;
end;

procedure TViewMain.ClickButtonFlow( Sender : TObject );
 begin
   ButtonFlow.Pressed := not ButtonFlow.Pressed;
   if ButtonFlow.Pressed then
    begin
      ShowNotification( 'Start water flow threads.' );
      StartWaterFlowThreads;
    end
   else
    begin
      ShowNotification( 'Stop water flow threads.' );
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
   ShowNotification('Cleared terrain.');
   assert( WaterFlowThreads[0].TaskList.Count = 0 );
 end;

procedure TViewMain.ShowNotification( Msg : string );
 var alabel : TCastleLabel;
     item : TCastleUserInterface;
     line : string;
 begin
   alabel := TCastleLabel.Create( VerticalGroup2 );
   alabel.FontSize := 12;
   alabel.AutoSize := true;
   alabel.Color := Vector4(1,1,1,1);
   line := FormatDateTime('yyyy.mm.dd hh:mm:ss', now )+ ' ' + Msg;
   writelnlog( line );

   alabel.caption := line ;
   VerticalGroup2.InsertBack( alabel );
   if VerticalGroup2.ControlsCount > 100 then
    begin
      item := VerticalGroup2.Controls[VerticalGroup2.ControlsCount-1];
      VerticalGroup2.RemoveControl(item);
      item.free;
    end;
 end;

procedure TViewMain.StopServerAndExit;
 begin
  ShowNotification( 'Stop water flow threads.' );
  StopWaterFlowThreads;
//  Sleep(100); { give water flow thread chance to stop }
  StopServer;
  Application.MainWindow.Close;
 end;

procedure TViewMain.CloseQuery(AContainer: TCastleContainer);
 begin
   StopServerAndExit;
 end;

initialization
(* AllocConsole;
 {$ifdef fpc}
 IsConsole := True; // in System unit
 SysInitStdIO;      // in System unit
 {$endif}*)
end.
