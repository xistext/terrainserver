unit waterflow;

interface

{ water flow logic and thread }

uses
  {$ifndef FPC}System.types,{$endif}
  Classes, SysUtils,
  Collect,
  livetime, BaseTools,
  basethread,
  waterparams, water_low, watergrid,
  TerrainParams, TerrainData;

const FlowStartTime : single = -1;
      FlowCounter   : dword = 0;
      FlowRunning   : boolean = false;

procedure StartWaterFlowThreads;
procedure StopWaterFlowThreads;

type TDirtyList = class( tcollection ) { of TTerTile }

        locked : boolean;
        { activetile well be kept out of the list and able to update more often }
        activetile : TTerTile;
        constructor create;
        procedure addtile( tile : TTerTile );
        function nexttile( var tile : TTerTile ) : boolean;

     end;

      TWaterFlowThread = class( TTaskThread )
        public

        DirtyTileList : TDirtyList; { list of tiles that need their graphics updated in the main thread }

        constructor Create;
        destructor Destroy; override;
        procedure Execute; override;
        protected
        function DoTaskFromList : boolean;

        private
        FlowIx : integer;
        DeltaGrid : TSingleGrid;
      end;

      TWaterTask = class( TThreadTask )
         watertile : TTerTile;
         constructor create( iwatertile : tTerTile );
         procedure RunTask; override;
         function flowtile( amounttoflow : single ) : boolean;
       end;

const WaterToFlowList_High : TThreadTaskList = nil;  { list of LOD=1 unowned tiles }
      WaterToFlowList_Low  : TThreadTaskList = nil;  { list of LOD>1 unowned tiles }
      cancelflow : boolean = false;
var   WaterFlowThreads : array[0..1] of TWaterFlowThread ; { thread for flowing the LOD=1 tiles }

implementation //===============================================================

const isqrt2 : single = 0;

procedure StartWaterFlowThreads;
 begin
   cancelflow := false;
   FlowRunning := true;
   FlowStartTime := gametime;
   FlowCounter   := 0;

(*   if WaterFlowThreads[0].Suspended then
      WaterFlowthreads[0].Resume
   else*)
      WaterFlowThreads[0].Start;
(*   if WaterFlowThreads[1].Suspended then
      WaterFlowthreads[1].Resume
   else
      WaterFlowThreads[1].Start;*)
 end;

procedure StopWaterFlowThreads;
 begin
   cancelflow := true;
   sleep(10);
   FlowRunning := false;
   WaterFlowThreads[0].Terminate;
   WaterFlowThreads[0].DirtyTileList.DeleteAll;;
(*   WaterFlowThreads[1].Suspend;
   WaterFlowThreads[1].DirtyTileList.DeleteAll;;*)
 end;

constructor TDirtyList.create;
 begin
   inherited create;
   ownitems := false;
   locked := false;
   activetile := nil;
 end;

procedure TDirtylist.addtile( tile : TTerTile );
 begin
//   if not tile.dirty and ( tile <> activetile ) then { if not already dirty }
    begin
//      tile.dirty := true;
      while locked do { another thread is adding a tile }
         sleep(1);
      locked := true;
      atinsert( 0, tile );
      locked := false;
    end;
 end;

function TDirtyList.nexttile( var tile : TTerTile ) : boolean;

 begin
   result := not locked and ( count > 0 );
   if result then
    begin
      locked := true;
      tile := TTerTile(at( count - 1 ));
      atdelete( count - 1 );
      locked := false;
    end;
 end;

//------------------------------------

Type tflowrunner = class
        parentthread : TWaterFlowThread;
        parentgrid : TTerTile;
        pos : tpoint;
        linesz : integer;
        celldata : TCellData;
        CellNeighbors : array[0..7] of TCellData;
        amounttoflow : single; { flow speed }
        tileneighbors : TTileNeighbors; { array of neighbors around the active tile }
        neighborlist : TNeighborlist;   { list of which neighbors this tile would flow to }
        constructor create( iparentthread : TWaterFlowThread;
                            iparentgrid : TTerTile  );
        destructor destroy; override;
        function nextcell : boolean;
        function flowtoneighbor( var neighbor : TCellData ) : boolean;
        function flowtoneighbors( x, y : integer ) : boolean;
        function flowtodistantneighbor( distantneighbor : TTerTile;
                                 x, y : integer ) : boolean;

        function validateneighbor( localneighborix : integer ) : boolean; overload;
        function validateneighbor( localneighborix, distanttileix : integer;
                                   distantx, distanty : integer ) : boolean; overload;
        procedure calcneighbors( x, y : integer );
        procedure calcdiagonalneighbor( nid : integer );
      end;

constructor tflowrunner.create( iparentthread : TWaterFlowThread;
                                iparentgrid : TTerTile );
 var flowptrs : TFlowPtrRec;
 begin
   parentthread := iparentthread;
   celldata.valid := true;
   pos := Point(0,0);
   LineSz := GDefGridCellCount; { water grid dimensions match terrain }
   CellNeighbors[7].valid := false;
   CellNeighbors[0].valid := false;
   CellNeighbors[1].valid := false;
   Cellneighbors[5].valid := false;
   Cellneighbors[6].valid := false;
   parentgrid := iparentgrid;
   flowptrs.dptr := parentgrid.WaterGrid.ptrix(0);
   flowptrs.hptr := parentgrid.TerrainGrid.ptrix(0);
   flowptrs.deltaptr := parentthread.DeltaGrid.ptrix(0);
   setcelldata( celldata, flowptrs );
   walkflowptrs( flowptrs );
   setcelldata( Cellneighbors[2], flowptrs );
   walkflowptrs( flowptrs, linesz );
   setcelldata( Cellneighbors[3], flowptrs );
   walkflowptrs( flowptrs, -1 );
   setcelldata( Cellneighbors[4], flowptrs );

   neighborlist := TNeighborlist.create;
   neighborlist.OwnItems := true;
   neighborlist.Duplicates := true;
 end;

destructor tflowrunner.destroy;
 begin
   inherited;
   neighborlist.Free;
 end;

const _next = 0;
      _valid = 1;
      _validandlast = 3;

function tflowrunner.nextcell : boolean;
 var flowptrs, flowptrs1 : TFlowPtrRec;
   procedure do_y_all;
    begin
      CellNeighbors[7] := CellNeighbors[0];
      CellNeighbors[0] := CellNeighbors[1];
      CellNeighbors[6] := CellData;
      CellData := CellNeighbors[2];
      flowptrs := CellData.flowptrs;
      CellNeighbors[5] := CellNeighbors[4];
      CellNeighbors[4] := CellNeighbors[3];
      CellNeighbors[1].valid := false;
      CellNeighbors[2].valid := false;
      CellNeighbors[3].valid := false;
    end;
   procedure do_y_notlast;
    var xnotfirst, xnotlast : boolean;
        whichcase : integer;
    begin
      flowptrs1 := flowptrs;
      walkflowptrs( flowptrs1 );
      setcelldata( Cellneighbors[2], flowptrs1 );
      xnotfirst := pos.x > 0;
      xnotlast := pos.x < LineSz - 1;
      whichcase := ord( xnotfirst ) + ord( xnotlast ) shl 1;
      case whichcase of
         1 : begin { xnotfirst, not xnotlast }
               walkflowptrs( flowptrs1, -linesz );
               setcelldata( Cellneighbors[1], flowptrs1 );
               walkflowptrs( flowptrs1, linesz );
             end;
         2 : begin { not xnotfirst, xnotlast }
               walkflowptrs( flowptrs1, linesz );
               setcelldata( Cellneighbors[3], flowptrs1 );
             end;
         3 : begin { xnotfirst, xnotlast }
               walkflowptrs( flowptrs1, -linesz );
               setcelldata( Cellneighbors[1], flowptrs1 );
               walkflowptrs( flowptrs1, linesz );
               walkflowptrs( flowptrs1, linesz );
               setcelldata( Cellneighbors[3], flowptrs1 );
             end;
      end;
    end;
   procedure do_x_all;
    begin
      flowptrs := CellData.flowptrs;
      walkflowptrs(flowptrs);
      setcelldata( celldata, flowptrs );
      CellNeighbors[7].valid := false;
      CellNeighbors[6].valid := false;
      CellNeighbors[5].valid := false;
      flowptrs1 := flowptrs;
      walkflowptrs( flowptrs1, -linesz );
      setcelldata( Cellneighbors[0], flowptrs1 );
      walkflowptrs( flowptrs1 );
      setcelldata( Cellneighbors[1], flowptrs1 );
      walkflowptrs( flowptrs1, linesz );
      setcelldata( Cellneighbors[2], flowptrs1 );
      Cellneighbors[4].valid := false;
      Cellneighbors[3].valid := false;
    end;
   procedure do_x_notlast;
    begin
      walkflowptrs( flowptrs1, linesz );
      setcelldata( Cellneighbors[3], flowptrs1 );
      walkflowptrs( flowptrs1, -1 );
      setcelldata( Cellneighbors[4], flowptrs1 );
    end;
 var valid : integer;
 begin
   result := true;
   inc( pos.y );
   valid := ord( pos.y < linesz ) +               { line valid }
            ord( pos.y = linesz - 1 ) shl 1;      { line last }
   case valid of
      _valid : begin
                 do_y_all;
                 do_y_notlast;
               end;
      _validandlast : do_y_all;
      _next : { next x line }
        begin
          pos.y := 0;
          inc( pos.x );
          result := pos.x < LineSz;
          valid := ord( result ) +                  { line valid }
                   ord( pos.x = linesz - 1 ) SHL 1;  { line last }
          case valid of
             _valid : begin
                          do_x_all;
                          do_x_notlast;
                        end;
             _validandlast : do_x_all;
           end
       end;
    end;
 end;

{      1 2 3               7 0 1
       4   5               6   2
  bit  6 7 8  tileneighbor 5 4 3 }

const _invalid = 0;
      _localvalid = 1;
      _distantvalid = 2;
      _bothvalid = 3;


function tflowrunner.validateneighbor( localneighborix : integer ) : boolean; overload;
 { will get local neighbor and if that is out of bounds, look to neighboring tile }
 begin
   result := Cellneighbors[localneighborix].valid;
 end;


function tflowrunner.validateneighbor( localneighborix, distanttileix : integer;
                                       distantx, distanty : integer ) : boolean; overload;
 { will get local neighbor and if that is out of bounds, look to neighboring tile }
 var flowptrs : TFlowPtrRec;
     ix : integer;
     distanttile : TTerTile;
     valid : integer;
 begin
   distanttile := tileneighbors[distanttileix];
   valid := ord( Cellneighbors[localneighborix].valid ) + ord( assigned( distanttile )) shl 1;
   result := valid > _invalid;
   if valid = _distantvalid then
    begin
      ix := distantx * distanttile.watergrid.wh + distanty;
      FlowPtrs.hptr := distanttile.terraingrid.ptrix( ix );
      FlowPtrs.dptr := distanttile.watergrid.ptrix( ix );
      FlowPtrs.deltaptr := nil;
      setcelldata( Cellneighbors[localneighborix], flowptrs );
    end;
 end;

procedure TFlowRunner.calcdiagonalneighbor( nid : integer );
 begin
   with celldata do
      if validateneighbor(nid) and ( terrainandwater > Cellneighbors[nid].terrainandwater ) then
         neighborlist.addneighbor(nid, ( terrainandwater - Cellneighbors[nid].terrainandwater ) * isqrt2 );
 end;

procedure TFlowRunner.calcneighbors( x, y : integer );
 { determine which neighbors this will flow to and add them to the list }
 begin
   with celldata do
    begin
      if validateneighbor(0,6,linesz-1,y) and ( terrainandwater > Cellneighbors[0].terrainandwater ) then
         neighborlist.addneighbor(0, terrainandwater - Cellneighbors[0].terrainandwater );
      calcdiagonalneighbor( 1 );
      if validateneighbor(2,4,x,0) and  ( terrainandwater > Cellneighbors[2].terrainandwater ) then
         neighborlist.addneighbor(2, terrainandwater - Cellneighbors[2].terrainandwater );
      calcdiagonalneighbor( 3 );
      if validateneighbor(4, 2, 0, y ) and  ( terrainandwater > Cellneighbors[4].terrainandwater ) then
         neighborlist.addneighbor(4, terrainandwater - Cellneighbors[4].terrainandwater );
      calcdiagonalneighbor( 5 );
      if validateneighbor(6,0, x, linesz -1) and  ( terrainandwater > Cellneighbors[6].terrainandwater ) then
         neighborlist.addneighbor(6, terrainandwater - Cellneighbors[6].terrainandwater );
      calcdiagonalneighbor( 7 );
    end;
 end;

function TFlowRunner.flowtodistantneighbor( distantneighbor : TTerTile;
                                            x, y : integer ) : boolean;
 var distantcell : TCellData;
     flowptrs : TFlowPtrRec;
     ix : integer;
 begin
   result := false;
   if assigned( distantneighbor ) then
    begin
      ix := x * distantneighbor.watergrid.wh + y;
      flowptrs.hptr := distantneighbor.terraingrid.ptrix( ix );
      flowptrs.dptr := distantneighbor.watergrid.ptrix( ix );
      flowptrs.deltaptr := nil;
      setcelldata( distantcell, flowptrs );
      result := flowtoneighbor( distantcell );
    end
 end;

function TFlowRunner.flowtoneighbor( var neighbor : TCellData) : boolean;
 var delta, snowfactor : single;
 begin
   with celldata do
    begin
      delta := ( TerrainAndWater - (Neighbor.waterdepth + Neighbor.terrainheight))*0.5 * amounttoflow;
      { limit delta to water depth }
      limitmax( delta, waterdepth );
      Result := ( delta > 0 );
      if Result then
       begin
         snowfactor := terrainheight - Defaultsnowline;
         limitminmax( snowfactor, 0, MaxSnowFactor );
         delta := delta * ( 1 - snowfactor );

         flowptrs.deltaptr^ := flowptrs.deltaptr^ - delta;
         if assigned( neighbor.flowptrs.deltaptr ) then
            Neighbor.flowptrs.deltaptr^ := Neighbor.flowptrs.deltaptr^ + delta
         else
            Neighbor.flowptrs.dptr^ := Neighbor.flowptrs.dptr^ + delta;
         waterdepth := waterdepth - delta;
         TerrainAndWater := TerrainAndWater - delta;
       end;
    end;
 end;

function TFlowRunner.flowtoneighbors( x,y : integer ) : boolean;

 var i : integer;
     neighbor : pneighbor;
 begin
   Result := false;
   tileneighbors := parentgrid.getneighbors;
   calcneighbors( x, y );
   neighbor := pneighbor( neighborlist.firstptr );
   for i := 0 to neighborlist.count - 1 do
    begin
      result := flowtoneighbor( Cellneighbors[ neighbor^.neighborix ]) or result;
      inc( neighbor );
      if ( celldata.waterdepth <= mindepth ) then
         break;
    end;
   neighborlist.FreeAll;
 end;

//------------------------------------

constructor TWaterFlowThread.Create;
 begin
   inherited Create( true {suspended} );
   FlowIx := 0;
   deltagrid := tsinglegrid.create(0, GDefGridCellCount );
   DirtyTileList := TDirtyList.Create;
 end;

destructor TWaterFlowThread.Destroy;
 begin
   inherited destroy;
   deltagrid.free;
   DirtyTileList.Free;
 end;

function TWaterFlowThread.DoTaskFromList : boolean;
 var Task : TThreadTask;
 begin
   result := ( not Terminated ) and ( TaskList.Count > 0 );
   if result then
    begin
      flowix := flowix mod TaskList.Count;
      if TaskList.GetTask( flowix, task ) then
       begin
         task.parentthread := self;
         task.RunTask;
         inc( flowix );
       end;
    end
 end;

procedure TWaterFlowThread.Execute;
 begin
   repeat
      if not DoTaskFromList then
         yield
    until Terminated or cancelflow or finished;
 end;

//---------------------------------

constructor TWaterTask.create( iwatertile : TTerTile );
 begin
   watertile := iwatertile;
   parentThread := nil;
 end;

function TWaterTask.FlowTile( amounttoflow : single ) : boolean;
     { local copies of grid parameters }
 var x, y : integer;
     FlowRunner : TFlowRunner;
 begin
   Result := false;
   with twaterflowthread(parentthread) do
    begin
      DeltaGrid.zerogrid;
      FlowRunner := TFlowRunner.create( twaterflowthread( parentthread ), watertile );
      FlowRunner.amounttoflow := amounttoflow;
      for x := 0 to flowrunner.linesz - 1 do for y := 0 to flowrunner.linesz - 1 do
       begin
         if FlowRunner.CellData.waterdepth > mindepth then
            Result := FlowRunner.FlowToNeighbors( x, y ) or Result;
         FlowRunner.NextCell; { walk the pointer and positions }
       end;
      if Result then
       begin
         inc( flowcounter );
         watertile.WaterGrid.addgrid(DeltaGrid);
         DirtyTileList.AddTile( watertile );
       end;
      FlowRunner.Free;
    end;
 end;

procedure TWaterTask.RunTask;
 var UpdateTime : single;
     Delta : single;
     TileUpdateTime : single;
 begin
   UpdateTime := GameTime;
   TileUpdateTime := WaterTile.WaterUpdateTime;
   if ( TimeSpeed > 1E-5 ) and ( not cancelflow ) and ( TileUpdateTime >= 0 ) then
    begin
      Delta := UpdateTime - TileUpdateTime;
      if delta < 0.1 then
         exit;
      Delta := Delta * flowfactor;
      FlowTile( Delta );
    end;
   WaterTile.WaterUpdateTime := UpdateTime;
 end;

initialization //===============================================================
  isqrt2 := 1/sqrt(2);
  //WaterToFlowList_low := TThreadTaskList.create;
  WaterToFlowList_high := TThreadTaskList.create;
  WaterFlowThreads[0] := TWaterFlowThread.Create;
  WaterFlowThreads[0].TaskList:= WaterToFlowList_high;
(*  WaterFlowThreads[1] := TWaterFlowThread.Create;
  WaterFlowThreads[1].TaskList:= WaterToFlowList_low;*)
finalization
  WaterFlowThreads[0].free;
//  WaterFlowThreads[1].free;
//  WaterToFlowList_low.Free;
  WaterToFlowList_high.Free;
end.


