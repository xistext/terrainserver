unit TerrainClient;

interface

uses Classes, Generics.Collections,
     Collect, TerServerCommon, terrainparams, idGlobal,
     CastleClientServer, CastleTransform, CastleControls,
     CastleVectors, watergrid, CastleRenderOptions,
     TerrainData, BaseMesh, x3dnodes, TerrainShader,
     TerrainMesh, TerrainObjects,
     IDTCPClient;

type
     TTexPoints = array of tvector2;

     TTileRec = record
        msginfo : TMsgHeader;
        tileinfo : TTileHeader;
        tilegrid : TSingleGrid;
        watergrid : TSingleGrid;
        floragrid : TSingleGrid;
      end;

     TTreesRec = record
        msgInfo  : TTileObjHeader;
        ObjList  : TTileObj_RecList;
      end;

     TSynchronisedTileList = {$ifdef FPC}specialize{$endif} TThreadList<TTileRec>;
     TSynchronisedTreeList = {$ifdef FPC}specialize{$endif} TThreadList<TTreesRec>;

     TTileReceivedEvent = procedure( const msginfo : TMsgHeader;
                                     const tileinfo : TTileHeader;
                                           tilegrid : TSingleGrid;
                                           watergrid : TSingleGrid;
                                           floragrid : TSingleGrid) of object;
     TTreeReceivedEvent = procedure( const listinfo : TTileObjHeader;
                                     objlist :     TTileObj_RecList) of object;

     TTerClientThread = class( TCastleTCPClientThread )
        constructor Create(const AClient: TIdTCPClient;
        const AOnMessageReceived, AOnConnected, AOnDisconnected: TProcedureObject);
        destructor Destroy; override;

        function ProcessMessage( const msgheader : TmsgHeader;
                                 Buffer : TIdBytes ) : boolean; override;
        procedure SendTile( const msgheader : TMsgHeader;
                            const Buffer : TIdBytes; BufLen : dword );
        procedure SendTrees( const msgheader : TMsgHeader;
                             const Buffer : TIdBytes; BufLen : dword );
        public
        FOnTileReceived: TProcedureObject;
        FOnTreeReceived: TProcedureObject;
        FTileList: TSynchronisedTileList;
        FTreeList: TSynchronisedTreeList;

       end;

     TTerClient = class( TCastleTCPClient )
        FOnTileReceived : TTileReceivedEvent;
        FOnTreeReceived : TTreeReceivedEvent;
        protected
        function CreateClientThread : TCastleTCPClientThread; override;
        procedure ClientOnTileReceived;
        procedure ClientOnTreeReceived;
        property OnTileReceived: TTileReceivedEvent read FOnTileReceived write FOnTileReceived;
        property OnTreeReceived: TTreeReceivedEvent read FOnTreeReceived write FOnTreeReceived;
        procedure Send(const AMessage: String); override;

      end;

const GParentComponent : TComponent = nil;
     // GDefaultHost     : string = 'localhost';
      GDefaultHost : string = '192.168.1.100';
      GDefaultPort     : integer = 10244;

      status_disconnected = 0;
      status_connecting = 1;
      status_connected = 2;

      glocksend : boolean = false;

procedure setCreateClientMode( mode : integer;
                               AButton : TCastleButton );

implementation

procedure setCreateClientMode( mode : integer;
                               AButton : TCastleButton );
 begin
   case mode of
     status_disconnected : begin
                             AButton.CustomColorPressed := vector4( 0.75, 0.75, 0, 0.75 );
                             AButton.CustomColorFocused := vector4( 0.5, 0, 0, 0.25 );
                             AButton.CustomColorNormal := vector4( 0.5, 0, 0, 0.5 );
                             AButton.Enabled := true;
                           end;
     status_connecting :   begin
                             AButton.CustomColorFocused := vector4( 0.5, 0.5, 0, 0.25 );
                             AButton.CustomColorNormal := vector4( 0.5, 0.5, 0, 0.5 );
                             AButton.Enabled := false;
                           end;
     status_connected :    begin
                             AButton.CustomColorPressed := vector4( 0.75, 0, 0, 0.75 );
                             AButton.CustomColorFocused := vector4( 0, 0.5, 0, 0.25 );
                             AButton.CustomColorNormal := vector4( 0, 0.5, 0, 0.5 );
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
   FTreeList := TSynchronisedTreeList.Create;
 end;

destructor TTerClientThread.Destroy;
 begin
   inherited;
   FTileList.Free;
   FTreeList.Free;
 end;

function TTerClientThread.ProcessMessage( const msgheader : TmsgHeader;
                                          Buffer : TIdBytes ) : boolean;
 var MsgLen : dword;
 begin
   glocksend := true;
   result := inherited ProcessMessage( msgheader, buffer );
   if not result then
    begin
       MsgLen := msgheader.msglen;
       if MsgLen > 0 then
        begin
          assert( MsgLen < 200000 );   {! crashes here when you increase view radius too fast. somehow the stream is out of sync}
          FClient.IOHandler.ReadBytes( Buffer, MsgLen, true );
          assert( length( buffer ) = MsgLen + SizeOf( msgheader ));

          case msgheader.msgtype of
              msg_Tile, msg_water, msg_flora, msg_LODUpdate, msg_splat, msg_flora  :
                 sendtile( msgheader, buffer, MsgLen + SizeOf( msgheader ));
              msg_tileobjs :
                 sendtrees( msgheader, buffer, msglen + SizeOf( msgheader ));
          end;
        end;
     end;
   glocksend := false;
 end;

procedure TTerClientThread.SendTile( const msgheader : TMsgHeader;
                                     const Buffer : TIdBytes;
                                           BufLen : dword );
 var tilerec : TTileRec;
     hdsz : integer;
     tilesz : integer;
     bufpos : integer;
 begin
   hdsz := sizeof(tmsgheader) + sizeof(ttileheader);
   assert( BufLen > hdsz );
   tilerec.msginfo := msgheader;
   Move( buffer[sizeof(tmsgheader)], tilerec.tileinfo, sizeof( ttileheader ));
   tilerec.tileGrid := TSingleGrid.createsize(tilerec.tileinfo.TileSz );
   tilerec.waterGrid := nil;
   tilerec.floragrid := nil;
   tilesz := tilerec.tileinfo.TileSz * tilerec.tileinfo.TileSz;
   bufpos := hdsz;
   Move( buffer[bufpos], tilerec.tileGrid.data^, tilesz * sizeof( single ));
   inc( bufpos, tilesz * sizeof( single ));
   case msgheader.msgtype of
      msg_LODUpdate: begin { send terrain + water depth + flora heihgt and let client build the water and texcoords }
                    tilerec.waterGrid := TSingleGrid.createsize( tilerec.tileinfo.TileSz );
                    Move( buffer[bufpos], tilerec.waterGrid.data^, tilesz * sizeof( single ));
                    inc( bufpos, tilesz * sizeof( single ));
                    tilerec.floraGrid := TSingleGrid.createsize( tilerec.tileinfo.TileSz );
                    Move( buffer[bufpos], tilerec.floraGrid.data^, tilesz * sizeof( single ));
                  end
    end;
   { make a new mesh if we can't figure out how to reuse existing mesh if same size }
   FTileList.Add( tilerec );
   Queue(FOnTileReceived);
 end;

procedure TTerClientThread.SendTrees( const msgheader : TMsgHeader;
                                      const Buffer : TIdBytes; BufLen : dword );
 var treesrec : TTreesRec;
     header : TTileObjHeader;
     hdsz : integer;
     bufpos : integer;
     treelist : TTileObj_RecList;
 begin
   hdsz := sizeof(tmsgheader) + sizeof(ttileobjheader);
   assert( BufLen > hdsz );
   Move( buffer[sizeof(tmsgheader)], header, sizeof( TTileObjHeader ));
   bufpos := hdsz;
   SetLength( treelist, header.ObjCount );
   Move( buffer[bufpos], treelist[0], header.ObjCount * SizeOf( ttileobj_rec ));
   TreesRec.MsgInfo := Header;
   TreesRec.ObjList := TreeList;
   FTreeList.Add( TreesRec );
   Queue(FOnTreeReceived );
 end;

//----------------------------------

function TTerClient.CreateClientThread : TCastleTCPClientThread;
 begin
   FClientThread := TTerClientThread.Create(FClient,
     {$ifdef FPC}@{$endif} ClientOnMessageReceived, FOnConnected, FOnDisconnected);
   TTerClientThread( FClientThread ).FOnTileReceived := {$ifdef FPC}@{$endif} ClientOnTileReceived;
   TTerClientThread( FClientThread ).FOnTreeReceived := {$ifdef FPC}@{$endif} ClientOnTreeReceived;
   Result := FClientThread;
 end;

procedure TTerClient.ClientOnTileReceived;
 var TileRec : TTileRec;
 begin
   if assigned( FOnTileReceived ) and assigned( FClientThread ) then
    begin
      for TileRec in TTerClientThread( FClientThread ).fTileList.LockList do
       begin
         FOnTileReceived( tilerec.msginfo, tilerec.tileinfo,
                          tilerec.tilegrid, tilerec.watergrid, tilerec.floragrid );

       end;
      TTerClientThread( FClientThread ).fTileList.Clear;
      TTerClientThread( FClientThread ).fTileList.UnlockList;
    end;
 end;

procedure TTerClient.ClientOnTreeReceived;
 var TreesRec : TTreesRec;
 begin
   if assigned( FOnTreeReceived ) and assigned( FClientThread ) then
    begin
      for TreesRec in TTerClientThread( FClientThread ).fTreeList.LockList do
         FOnTreeReceived( treesrec.msginfo, treesrec.ObjList );
      TTerClientThread( FClientThread ).fTreeList.Clear;
      TTerClientThread( FClientThread ).fTreeList.UnlockList;
    end;
 end;

procedure TTerClient.Send(const AMessage: String);
 begin
   {???  how insure it is safe to send }
   while glocksend do
    begin
//      writeln('.');
      FClientThread.Yield;
    end;
   inherited send( AMessage );

 end;





end.
