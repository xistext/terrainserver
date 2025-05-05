unit water_low;

interface

uses
  Collect, BaseTools;

{ records to consolidate connected values }
type THeightsRec = record
        terrainheight : single;
        waterdepth    : single;
        floradepth    : single;
      end;

     TFlowPtrRec = record
        dptr : psingle;
        hptr : psingle;
        deltaptr : psingle;
      end;

     TUpdatePtrRec = record
        hptr, dptr : psingle;
        fptr : psmallint;
      end;

{ cell data used for flow }
     PCellData = ^TCellData;
     TCellData = record
                    valid     : boolean;
                    flowptrs  : TFlowPtrRec;
                    terrainheight : single;
                    waterdepth    : single;
                    terrainandwater : single;
                  end;

procedure setcelldata( var cell : TCellData;
                       const iflowptrs : TFlowPtrRec );


{ classes used to sort flow to neighbors by steepnees }
type pneighbor = ^tneighbor;
     tneighbor = class
                    delta      : single;
                    neighborix : integer;
                  end;

     tneighborlist = class( tsortedcollection )

        function keyof( item : pointer ) : pointer;  override;
        function compare( key1, key2 : pointer ) : integer;   override;

        procedure addneighbor( neighborix : integer;
                               d : single );
      end;

procedure setheights( var heights : THeightsRec;
                      terrainh, waterd, florad : single );

procedure walkflowptrs( var flowptrrec : TFlowptrRec;
                            delta : integer = 1 );
procedure walkupdateptrs( var updateptrrec : TupdateptrRec;
                              delta : integer = 1 );

implementation

function tneighborlist.keyof( item : pointer ) : pointer;
 begin
   result := pointer(tneighbor( item ).delta);
 end;

function tneighborlist.compare( key1, key2 : pointer ) : integer;
 begin
   result := comparesingle( single( key2 ), single( key1 )); { sorts highest to lowest }
 end;

procedure tneighborlist.addneighbor( neighborix : integer;
                                     d : single );
 var n : tneighbor;
 begin
   n := tneighbor.create;
   n.delta := d;
   n.neighborix := neighborix;
   insert( n );
 end;

//--------------------------------------

procedure setheights( var heights : THeightsRec;
                      terrainh, waterd, florad : single );
 begin
   with heights do
    begin
      terrainheight := terrainh;
      waterdepth := waterd;
      floradepth := florad;
    end;
 end;

procedure walkflowptrs( var flowptrrec : TFlowptrRec;
                            delta : integer = 1 );
 begin
   with flowptrrec do
    begin
      inc( dptr, delta );
      inc( hptr, delta );
      inc( deltaptr, delta );
    end;
 end;

procedure walkupdateptrs( var updateptrrec : TupdateptrRec;
                              delta : integer = 1 );
 begin
   with updateptrrec do
    begin
      inc( hptr, delta );
      inc( dptr, delta );
      inc( fptr, delta );
    end;
 end;

//----------------------------------

procedure setcelldata( var cell : TCellData;
                       const iflowptrs : TFlowPtrRec );
 begin
   with cell do
    begin
      valid := true;
      flowptrs := iflowptrs;
      terrainheight := flowptrs.hptr^;
      waterdepth := flowptrs.dptr^;
      if assigned( flowptrs.deltaptr ) then
         waterdepth := waterdepth + flowptrs.deltaptr^;
      terrainandwater := terrainheight + waterdepth;
    end;
 end;

end.

