unit clientlist;

interface

uses Collect,
     CastleClientServer,
     TerrainData;

type TSubscription = record
        LOD  : integer;
        Tile : TTerTile;
        LastUpdateTime : integer;
      end;


     TTileSubscriber = class

        subscriptions : array of TSubscription;

        constructor create( const aclient : TClientConnection );

        procedure setsubscription( atile : ttertile; iLOD : integer );

        protected

        fClient : TClientConnection;

      end;

     TTileSubscribers = class( tcollection )

        function getsubscriber( const aclient : TClientConnection ) : TTileSubscriber;
        function removesubscriber( const aclient : TClientConnection ) : boolean;

      end;

const GClientList : TTileSubscribers = nil;

implementation

constructor TTileSubscriber.create( const aclient : TClientConnection );
 begin
   fClient := aClient;
 end;

procedure TTileSubscriber.setsubscription( atile : ttertile; iLOD : integer );
 var i, c : integer;
 begin
   i := 0;
   c := length( subscriptions );
   for i := 0 to c - 1 do with subscriptions[i] do
    begin
      if tile = atile then
       begin
         if LOD <> iLOD then
            LastUpdateTime := -1;
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

//-----------------------

function TTileSubscribers.getsubscriber( const aclient : TClientConnection ) : TTileSubscriber;
 var i, c : integer;
 begin
   c := count;
   for i := 0 to c - 1 do
    begin
      result := TTileSubscriber( at( i ));
      if result.fClient.Context = aclient.Context then
         exit; // found, exit
    end;
   // not found, create
   result := TTileSubscriber.create( aclient );
   insert( result );
 end;

function TTileSubscribers.removesubscriber( const aclient : TClientConnection ) : boolean;
 var i, c : integer;
     item : TTileSubscriber;
 begin
   result := false;
   c := count;
   for i := 0 to c - 1 do
    begin
      item := TTileSubscriber( at( i ));
      result := item.fClient.Context = aclient.Context;
      if result then
       begin
         atdelete( i );
         item.free;
         exit;
       end;
    end;
 end;

initialization
   GClientList := TTileSubscribers.create;
finalization
   GClientList.Free;
end.
