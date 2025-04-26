unit TerrainClient;

interface

uses Classes, Generics.Collections,
     Collect, TerServerCommon, terrainparams, idGlobal,
     CastleClientServer, CastleTransform, CastleControls,
     CastleVectors, watergrid, CastleRenderOptions,
     TerrainData, BaseMesh, x3dnodes, TerrainShader,
     TerrainMesh,
     IDTCPClient;

type

     TTileRec = record
        msginfo : TMsgHeader;
        tileinfo : TTileHeader;
        tilegrid : TSingleGrid;
      end;

     TSynchronisedTileList = {$ifdef FPC}specialize{$endif} TThreadList<TTileRec>;

     TTileReceivedEvent = procedure( const msginfo : TMsgHeader;
                                     const tileinfo : TTileHeader;
                                           tilegrid : TSingleGrid ) of object;

     TTerClientThread = class( TCastleTCPClientThread )
        constructor Create(const AClient: TIdTCPClient;
                const AOnMessageReceived, AOnConnected, AOnDisconnected: TProcedureObject);
        destructor Destroy; override;

        function ProcessMessage( const msgheader : TmsgHeader;
                                 Buffer : TIdBytes ) : boolean; override;
        procedure SendTile( const msgheader : TMsgHeader;
                            const Buffer : TIdBytes; BufLen : dword );
        public
        FOnTileReceived: TProcedureObject;
        FTileList: TSynchronisedTileList;

       end;

     TTerClient = class( TCastleTCPClient )
        FOnTileReceived : TTileReceivedEvent;
        protected
        function CreateClientThread : TCastleTCPClientThread; override;
        procedure ClientOnTileReceived;
        property OnTileReceived: TTileReceivedEvent read FOnTileReceived write FOnTileReceived;
      end;

const GParentComponent : TComponent = nil;
      GDefaultHost     : string = 'localhost';
      GDefaultPort     : integer = 10244;

      status_disconnected = 0;
      status_connecting = 1;
      status_connected = 2;

procedure setCreateClientMode( mode : integer;
                               AButton : TCastleButton );

implementation

procedure setCreateClientMode( mode : integer;
                               AButton : TCastleButton );
 begin
   case mode of
     status_disconnected : begin
                             AButton.Caption := 'Connect';
                             AButton.Enabled := true;
                           end;
     status_connecting :   begin
                             AButton.Caption := 'Connecting...';
                             AButton.Enabled := false;
                           end;
     status_connected :    begin
                             AButton.Caption := 'Disconnect';
                             AButton.Enabled := true;
                           end;
     end;
 end;

//---------------------------------

constructor TTerClientThread.Create(const AClient: TIdTCPClient;
        const AOnMessageReceived, AOnConnected, AOnDisconnected: TProcedureObject);
 begin
   inherited Create(AClient, AOnMessageReceived, AOnConnected, AOnDisconnected);
   FTileList := TSynchronisedTileList.Create;
 end;

destructor TTerClientThread.Destroy;
 begin
   inherited;
   FTileList.Free;
 end;

function TTerClientThread.ProcessMessage( const msgheader : TmsgHeader;
                                          Buffer : TIdBytes ) : boolean;
 var MsgLen : dword;
 begin
   result := inherited ProcessMessage( msgheader, buffer );
   if not result then
    begin
       MsgLen := msgheader.msglen;
       if MsgLen > 0 then
        begin
          FClient.IOHandler.ReadBytes( Buffer, MsgLen, true );
          assert( length( buffer ) = MsgLen + SizeOf( msgheader ));

          case msgheader.msgtype of
              msg_Tile : sendtile( msgheader, buffer, MsgLen + SizeOf( msgheader ));
          end;
        end;
     end;
 end;

procedure TTerClientThread.SendTile( const msgheader : TMsgHeader;
                                     const Buffer : TIdBytes;
                                           BufLen : dword );
 var tilerec : TTileRec;
     hdsz : integer;
 begin
   hdsz := sizeof(tmsgheader) + sizeof(ttileheader);
   assert( BufLen > hdsz );
   BufLen := BufLen - hdsz;
   tilerec.msginfo := msgheader;
   Move( buffer[sizeof(tmsgheader)], tilerec.tileinfo, sizeof( ttileheader ));
   tilerec.tileGrid := TSingleGrid.createsize(tilerec.tileinfo.TileSz );
   Move( buffer[hdsz], tilerec.tileGrid.data^, BufLen );
   { make a new mesh if we can't figure out how to reuse existing mesh if same size }
   FTileList.Add( tilerec );
   Queue(FOnTileReceived);
 end;

//----------------------------------

function TTerClient.CreateClientThread : TCastleTCPClientThread;
 begin
   FClientThread := TTerClientThread.Create(FClient,
     {$ifdef FPC}@{$endif} ClientOnMessageReceived, FOnConnected, FOnDisconnected);
   TTerClientThread( FClientThread ).FOnTileReceived := {$ifdef FPC}@{$endif} ClientOnTileReceived;;
 end;

procedure TTerClient.ClientOnTileReceived;
 var TileRec : TTileRec;
 begin
   if assigned( FOnTileReceived ) then
    begin
      for TileRec in TTerClientThread( FClientThread ).fTileList.LockList do
       begin
         FOnTileReceived( tilerec.msginfo, tilerec.tileinfo, tilerec.tilegrid );

       end;
      TTerClientThread( FClientThread ).fTileList.Clear;
      TTerClientThread( FClientThread ).fTileList.UnlockList;
    end;
 end;

//-----------------------------





end.
