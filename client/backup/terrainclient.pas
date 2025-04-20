unit TerrainClient;

interface

uses Classes, Generics.Collections,
     Collect, TerServerCommon, terrainparams, idGlobal,
     CastleClientServer, CastleTransform,
     CastleVectors, watergrid, CastleRenderOptions,
     TerrainData, BaseMesh, x3dnodes, TerrainShader,
     TerrainMesh,
     IDTCPClient;

type

     TTileRec = record
        msginfo : TMsgHeader;
        tile     : TTerTile;
        tilemesh : TTerrainMesh;
      end;

     TSynchronisedTileList = {$ifdef FPC}specialize{$endif} TThreadList<TTileRec>;

     TTileReceivedEvent = procedure( const msginfo : TMsgHeader;
                                           tile : TTerTile;
                                     const tilemesh : TTerrainMesh ) of object;

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

implementation


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
var Body: TCastleRigidBody;
 var tilerec : TTileRec;
     hdsz : integer;
     agrid : tSingleGrid;
     tileinfo : TTileHeader;
     mycollider : TCastleMeshCollider;
 begin
   hdsz := sizeof(tmsgheader) + sizeof(ttileheader);
   assert( BufLen > hdsz );
   BufLen := BufLen - hdsz;
   tilerec.msginfo := msgheader;
   Move( buffer[sizeof(tmsgheader)], tileinfo, sizeof( ttileheader ));
   AGrid := TSingleGrid.createsize(tileinfo.TileSz );
   Move( buffer[hdsz], AGrid.Data^ , BufLen );

   tilerec.Tile := GTileList.GetInitTile( tileinfo );

   { make a new mesh if we can't figure out how to reuse existing mesh if same size }
   tilerec.tilemesh := TTerrainMesh.create2( GParentComponent, tilerec.Tile );
   tilerec.tilemesh.UpdateFromGrid( AGrid );
   AGrid.Free;
            (*
   Body := TCastleRigidBody.Create(tilerec.tilemesh);
   tilerec.tilemesh.AddBehavior(Body);


   MyCollider := TCastleMeshCollider.Create(tilerec.tilemesh);
   MyCollider.Mesh := tilerec.tilemesh;
   MyCollider.Restitution := 0.3;
   tilerec.tilemesh.AddBehavior(MyCollider);
              *)
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
         FOnTileReceived( tilerec.msginfo, tilerec.tile, tilerec.tilemesh );

       end;
      TTerClientThread( FClientThread ).fTileList.Clear;
      TTerClientThread( FClientThread ).fTileList.UnlockList;
    end;
 end;

//-----------------------------





end.
