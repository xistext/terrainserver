unit clientlist;

interface

uses Classes,
     Collect,
     IdGlobal,
     CastleClientServer,
     TerServerCommon,
     TerrainData;

type TSubscription = record
        LOD  : integer;
        Tile : TTerTile;
        LastUpdateTime : single;
      end;

     TTileClient = class; { forward }

     TSubscriptionProc = procedure( client : TTileClient;
                                    tile   : TTerTile;
                                    LOD    : integer;
                                    data   : pointer );

     TTileClient = class

        constructor create( const aclient : TClientConnection );
        function equals( atileclient : TTileClient ) : boolean;
        function connected : boolean;

        { send data handling }
        procedure Send( const Buffer; BufLength : integer );
        procedure SendBuffer( const buffer : TIdBytes; ALength : integer );
        procedure SendString( AString : string );

        procedure SendClientMsgHeader( msgtype : TMsgType;
                                       msglen  : dword = 0;
                                       requestid : dword = 0 );

        { subscription management }
        procedure setsubscription( atile : ttertile; iLOD : integer );
        procedure removesubscription( atile : ttertile );
        function getsubscription( atile : ttertile; var subscription : TSubscription ) : boolean;
        procedure iteratesubscriptions( callback : tsubscriptionproc; data : pointer );

        procedure unsubdistanttiles( const pos : tpoint; radius : integer );

        protected

        fClient : TClientConnection;
        subscriptions : array of TSubscription;

      end;

     TTileClients = class( tcollection )

        function getsubscriber( const aclient : TClientConnection ) : TTileClient;
        function removesubscriber( const aclient : TClientConnection ) : boolean;
        procedure iteratesubscriptions( callback : tsubscriptionproc; data : pointer );

      end;

const GClientList : TTileClients = nil;

implementation

constructor TTileClient.create( const aclient : TClientConnection );
 begin
   fClient := aClient;
 end;

function TTileClient.equals( atileclient : TTileClient ) : boolean;
 begin
   result := fClient.Context = atileclient.fClient.Context;
 end;

function TTileClient.connected : boolean;
 begin
   result := assigned( fClient.Context.Connection );
 end;

function TTileClient.getsubscription( atile : ttertile; var subscription : TSubscription ) : boolean;
 var i : integer;
 begin
   result := false;
   for i := 0 to length( subscriptions ) - 1 do
    begin
      subscription := subscriptions[i];
      result := subscription.tile = atile;
      if result then
         exit;
    end;
 end;

procedure TTileClient.setsubscription( atile : ttertile; iLOD : integer );
 var i, c : integer;
 begin
   i := 0;
   c := length( subscriptions );
   for i := 0 to c - 1 do with subscriptions[i] do
    begin
      if tile = atile then
       begin
         if LOD <> iLOD then
          begin
            LOD := iLOD;
            LastUpdateTime := -1;
          end;
         exit;
       end;
    end;
   // not found, create
   setlength( subscriptions, c + 1 );
   with subscriptions[c] do
    begin
      Tile := aTile;
      LastUpdateTime := -1;
      LOD := iLOD;
    end;
 end;

procedure TTileClient.removesubscription( atile : ttertile );
 var i : integer;
 begin
   i := 0;
   for i := 0 to length( subscriptions ) - 1 do
      if subscriptions[i].tile = atile then
       begin
         delete( subscriptions, i, 1 );
         exit;
       end;
 end;

procedure TTileClient.SendClientMsgHeader( msgtype : TMsgType;
                                           msglen  : dword = 0;
                                           requestid : dword = 0 );
 var h : TMsgHeader;
 begin
   h.requestid := requestid;
   h.msgtype := msgtype;
   h.msglen := msglen;
   fClient.Send( h, sizeof( h ));
 end;

procedure TTileClient.Send( const Buffer; BufLength : integer );
 begin
   fClient.Send( Buffer, BufLength );
 end;

procedure TTileClient.SendBuffer( const buffer : TIdBytes; ALength : integer );
 begin
   fClient.SendBuffer( buffer, ALength );
 end;

procedure TTileClient.SendString( AString : string );
 begin
   fClient.SendString( AString );
 end;

procedure TTileClient.iteratesubscriptions( callback : tsubscriptionproc; data : pointer );
 var i, c : integer;
     tileupdatetime : single;
 begin
   c := length( subscriptions );
   for i := 0 to c - 1 do with subscriptions[i] do
    begin
      TileUpdateTime := Tile.WaterUpdateTime;

      if (( LastUpdateTime < 0 ) and ( TileUpdateTime > 0 )) or
         (( LastUpdateTime > 0 ) and ( LastUpdateTime < TileUpdateTime )) then
       begin
         callback( self, Tile, LOD, data );
         LastUpdateTime := TileUpdateTime;
       end;
    end;
 end;

procedure TTileClient.unsubdistanttiles( const pos : tpoint; radius : integer );
 var i : integer;
     item : TSubscription;
     d : integer;
 begin
   i := 0;
   while i < length( subscriptions ) do
    begin
      item := subscriptions[i];
      d := trunc( sqrt( sqr( item.Tile.Info.TileX - pos.x ) + sqr( item.Tile.Info.TileY - pos.y )));
      if d > radius then
         delete( subscriptions, i, 1 )
      else
         inc( i );
    end;
 end;

//-----------------------

function TTileClients.getsubscriber( const aclient : TClientConnection ) : TTileClient;
 var i, c : integer;
 begin
   c := count;
   for i := 0 to c - 1 do
    begin
      result := TTileClient( at( i ));
      if result.fClient.Context = aclient.Context then
         exit; // found, exit
    end;
   // not found, create
   result := TTileClient.create( aclient );
   insert( result );
 end;

function TTileClients.removesubscriber( const aclient : TClientConnection ) : boolean;
 var i, c : integer;
     item : TTileClient;
 begin
   result := false;
   c := count;
   for i := 0 to c - 1 do
    begin
      item := TTileClient( at( i ));
      result := item.fClient.Context = aclient.Context;
      if result then
       begin
         atdelete( i );
         item.free;
         exit;
       end;
    end;
 end;

procedure TTileClients.iteratesubscriptions( callback : tsubscriptionproc; data : pointer );
 var i : integer;
 begin
   for i := 0 to count - 1 do
      TTileClient( at( i )).iteratesubscriptions( callback, data );
 end;


initialization
   GClientList := TTileClients.create;
finalization
   GClientList.Free;
end.
