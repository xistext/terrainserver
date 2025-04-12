unit TerrainClient;

interface

uses Classes, Generics.Collections,
     Collect, TerServerCommon, terrainparams, idGlobal,
     CastleClientServer,
     CastleVectors, watergrid,
     TerrainData, BaseMesh, x3dnodes,
     IDTCPClient;

type TTerrainMesh = class( TAbstractTextureMesh )
        constructor create( aowner : TComponent );  override;
        constructor create2( aowner : TComponent;
                             iLinkedTile : TTerTile );
        procedure UpdateSize;
//        procedure updategraphics; override;
        procedure updatefromgrid( TerrainGrid : TSingleGrid );
        public
        LinkedTile : TTerTile;

        protected

        procedure setGridCount( iGridCount : integer ); override;
        function getGridCount : integer; override;

        function getGridStep : single; override;

      end;

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
 var tilerec : TTileRec;
     hdsz : integer;
     agrid : tSingleGrid;
     tileinfo : TTileHeader;
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
   tilerec.tilemesh := TTerrainMesh.create2( nil, tilerec.Tile );
   tilerec.tilemesh.UpdateFromGrid( AGrid );
   AGrid.Free;

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

constructor TTerrainMesh.create( aowner : TComponent );
 begin
   inherited create( aowner );
   LinkedTile := nil;
 end;

constructor TTerrainMesh.create2( aowner : TComponent;
                                  iLinkedTile : TTerTile );
var offsetx, offsety : single;
    sz : single;
 begin
   inherited create( aowner );
   LinkedTile := iLinkedTile;
   if assigned( LinkedTile ) then with LinkedTile do
    begin
      //Graphics := self;
      UpdateSize;
      sz := GDefGridCellCount * GDefGridStep;
      offsetx := Info.TileX * sz;
      offsety := Info.TileY * sz;
      position := vector3( offsetx, 0, offsety );
    end;
 end;

procedure TTerrainMesh.UpdateSize;
 begin
   InitializeData;
 end;

procedure TTerrainMesh.setGridCount( iGridCount : integer );
 begin
   LinkedTile.Info.TileSz := iGridCount;
 end;

function TTerrainMesh.getGridCount : integer;
begin
  result := LinkedTile.Info.TileSz;
end;

function TTerrainMesh.getGridStep : single;
begin
  Result := LinkedTile.GridStep;
end;


procedure TTerrainMesh.updatefromgrid( TerrainGrid : TSingleGrid );
var TerrainAndWater : single;
   i, j, ix, pstep, lstep : integer;
   pos : TPoint;
   VertexPtr : ^TVector3;
   TexPtr : ^TVector2;
   TexturePos : TVector2;
   h : single;
   hptr : PSingle;
   c : integer;
 procedure setYandTex( y : single; const tex : TVector2 );
  begin
    VertexPtr^.Y := y;
    TexPtr^ := tex;
    inc( VertexPtr );
    inc( TexPtr );
  end;
 procedure setYandTexLast( y : single; const tex : TVector2 );
  begin
    VertexPtr^.Y := y;
    TexPtr^ := tex;
  end;
var fLod : integer;
begin
  fLod := 1;
  pos := point(0,0);
  pstep := fLOD; { pointstep for walking ptr }
  lstep := pstep * terraingrid.w;    { linestep for walking ptr }
  VertexPtr := CoordinateNode.FdPoint.Items.Ptr(0);  { starting vertex pointer }
  TexPtr := TexCoordNode.FdPoint.Items.Ptr(0);       { starting texture pointer }
  TexturePos := Vector2( 0, 0 );{!!!}
  c := GridCount;
  for i := c - 2 downto 0 do
   begin
     hptr := TerrainGrid.ptrix( pos.y );
     { do rest except last }
     for j := c - 2 downto 0 do
      begin
        setYandTex( hptr^, TexturePos );
        inc( hptr, lstep );
      end;
     h := 0; //getneighborvalues( neighbors[2], point( 0, pos.y ), h );
     { do last cell in row x = fGridCount - 1 from x+1 neighboring grid }
     setYandTex( h, TexturePos );
     inc( pos.y, pstep );
   end;
  { do the last line of y = fGridCount - 1 from y+1 neighboring grid }
  pos.x := 0;
  ix := pos.y - pstep; { step back get edge terrainheights from last line }
  for j := c - 2 downto 0 do
   begin
     h := TerrainGrid.valueix( ix ); { get last terrain height }
     setYandTex( h , TexturePos );
     inc( ix, lstep );
     inc( pos.x, pstep );
   end;
  { do last cell of x = fGridCount - 1 from x+1 y+1 neighboring grid }
  setYandTexLast( h , TexturePos );
  TexCoordNode.FdPoint.changed; { trigger mesh to rebuild }
//  dirty := false;
end;



end.
