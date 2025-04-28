unit waterflow;

interface

{ water flow logic and thread }

uses
  {$ifndef FPC}System.types,{$endif}
  Classes, SysUtils,
  livetime, BaseTools,
  basethread,
  waterparams,
  water_low, watergrid, waterterrain,
  Collect;

procedure StartWaterFlowThreads;
procedure StopWaterFlowThreads;

type TDirtyList = class( tcollection ) { of TBaseWaterTerrain }

        locked : boolean;
        { activetile well be kept out of the list and able to update more often }
        activetile : TBaseWaterTerrain;
        constructor create;
        procedure addtile( tile : TBaseWaterTerrain );
        function nexttile( var tile : TBaseWaterTerrain ) : boolean;

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
        { stores working values for the water tasks, per thread }
        neighborlist : tneighborlist;
        deltagrid : TSingleGrid;
      end;

      TWaterTask = class( TThreadTask )
         watertile : TBaseWaterTerrain;
         constructor create( iwatertile : tbasewaterterrain );
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
   WaterFlowThreads[0].Start;
   WaterFlowThreads[1].Start;
 end;

procedure StopWaterFlowThreads;
 begin
   cancelflow := true;
 end;

constructor TDirtyList.create;
 begin
   inherited create;
   ownitems := false;
   locked := false;
   activetile := nil;
 end;

procedure TDirtylist.addtile( tile : TBaseWaterTerrain );
 begin
   if not tile.dirty and ( tile <> activetile ) then { if not already dirty }
    begin
      tile.dirty := true;
      while locked do { another thread is adding a tile }
         sleep(1);
      locked := true;
      atinsert( 0, tile );
      locked := false;
    end;
 end;

function TDirtyList.nexttile( var tile : TBaseWaterTerrain ) : boolean;

 begin
   result := not locked and ( count > 0 );
   if result then
    begin
      locked := true;
      tile := tbasewaterterrain(at( count - 1 ));
      atdelete( count - 1 );
      locked := false;
    end;
 end;

//------------------------------------

Type tflowrunner = class
        parentthread : TWaterFlowThread;
        parentgrid : TBaseWaterTerrain;
        pos : tpoint;
        linesz : integer;
        celldata : TCellData;
        Neighbors : array[0..7] of TCellData;
        amounttoflow : single; { flow speed }
        constructor create( iparentthread : TWaterFlowThread;
                            iparentgrid : TBaseWaterTerrain );
        function nextcell : boolean;
        function flowtoneighbor( var neighbor : TCellData ) : boolean;
        function flowtoneighbors( x, y : integer ) : boolean;
        function flowtodistantneighbor( distantneighbor : TBaseWaterTerrain;
                                 x, y : integer ) : boolean;
      end;

constructor tflowrunner.create( iparentthread : TWaterFlowThread;
                                iparentgrid : TBaseWaterTerrain );
 var flowptrs : TFlowPtrRec;
 begin
   parentthread := iparentthread;
   celldata.valid := true;
   pos := Point(0,0);
   LineSz := WaterGrid.defaultsize;
   //FillChar( Neightbors, Sizeof( Neightbor
   Neighbors[7].valid := false;
   Neighbors[0].valid := false;
   Neighbors[1].valid := false;
   neighbors[5].valid := false;
   neighbors[6].valid := false;
   parentgrid := iparentgrid;
   flowptrs.dptr := parentgrid.WaterGrid.ptrix(0);
   flowptrs.hptr := parentgrid.LinkedTerrainTile.TerrainGrid.ptrix(0);
   flowptrs.deltaptr := parentthread.DeltaGrid.ptrix(0);
   setcelldata( celldata, flowptrs );
   walkflowptrs( flowptrs );
   setcelldata( neighbors[2], flowptrs );
   walkflowptrs( flowptrs, linesz );
   setcelldata( neighbors[3], flowptrs );
   walkflowptrs( flowptrs, -1 );
   setcelldata( neighbors[4], flowptrs );
 end;

const _next = 0;
      _valid = 1;
      _validandlast = 3;

function tflowrunner.nextcell : boolean;
 var flowptrs, flowptrs1 : TFlowPtrRec;
   procedure do_y_all;
    begin
      Neighbors[7] := Neighbors[0];
      Neighbors[0] := Neighbors[1];
      Neighbors[6] := CellData;
      CellData := Neighbors[2];
      flowptrs := CellData.flowptrs;
      Neighbors[5] := Neighbors[4];
      Neighbors[4] := Neighbors[3];
      Neighbors[1].valid := false;
      Neighbors[2].valid := false;
      Neighbors[3].valid := false;
    end;
   procedure do_y_notlast;
    var xnotfirst, xnotlast : boolean;
        whichcase : integer;
    begin
      flowptrs1 := flowptrs;
      walkflowptrs( flowptrs1 );
      setcelldata( neighbors[2], flowptrs1 );
      xnotfirst := pos.x > 0;
      xnotlast := pos.x < LineSz - 1;
      whichcase := ord( xnotfirst ) + ord( xnotlast ) shl 1;
      case whichcase of
         1 : begin { xnotfirst, not xnotlast }
               walkflowptrs( flowptrs1, -linesz );
               setcelldata( neighbors[1], flowptrs1 );
               walkflowptrs( flowptrs1, linesz );
             end;
         2 : begin { not xnotfirst, xnotlast }
               walkflowptrs( flowptrs1, linesz );
               setcelldata( neighbors[3], flowptrs1 );
             end;
         3 : begin { xnotfirst, xnotlast }
               walkflowptrs( flowptrs1, -linesz );
               setcelldata( neighbors[1], flowptrs1 );
               walkflowptrs( flowptrs1, linesz );
               walkflowptrs( flowptrs1, linesz );
               setcelldata( neighbors[3], flowptrs1 );
             end;
      end;
    end;
   procedure do_x_all;
    begin
      flowptrs := CellData.flowptrs;
      walkflowptrs(flowptrs);
      setcelldata( celldata, flowptrs );
      Neighbors[7].valid := false;
      Neighbors[6].valid := false;
      Neighbors[5].valid := false;
      flowptrs1 := flowptrs;
      walkflowptrs( flowptrs1, -linesz );
      setcelldata( neighbors[0], flowptrs1 );
      walkflowptrs( flowptrs1 );
      setcelldata( neighbors[1], flowptrs1 );
      walkflowptrs( flowptrs1, linesz );
      setcelldata( neighbors[2], flowptrs1 );
      neighbors[4].valid := false;
      neighbors[3].valid := false;
    end;
   procedure do_x_notlast;
    begin
      walkflowptrs( flowptrs1, linesz );
      setcelldata( neighbors[3], flowptrs1 );
      walkflowptrs( flowptrs1, -1 );
      setcelldata( neighbors[4], flowptrs1 );
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

function TFlowRunner.flowtoneighbors( x,y : integer ) : boolean;

 function validateneighbor( localneighborix, distanttileix : integer;
                            distantx, distanty : integer ) : boolean; overload;
  { will get local neighbor and if that is out of bounds, look to neighboring tile }
 var flowptrs : TFlowPtrRec;
     ix : integer;
     distanttile : TBaseWaterTerrain;
     nptr : PCellData;
     valid : integer;
  begin
    distanttile := parentgrid.neighbors[distanttileix];
    nptr := @neighbors[localneighborix];
    valid := ord( nptr^.valid ) + ord( assigned( distanttile )) shl 1;
    case valid of
       _localvalid, _bothvalid :;
       _distantvalid :
         begin
           ix := distantx * distanttile.watergrid.wh + distanty;
           FlowPtrs.hptr := distanttile.LinkedTerrainTile.terraingrid.ptrix( ix );
           FlowPtrs.dptr := distanttile.watergrid.ptrix( ix );
           FlowPtrs.deltaptr := nil;
           setcelldata( neighbors[localneighborix], flowptrs );
         end;
     end;
    result := valid > _invalid;
  end;

 function validateneighbor( localneighborix : integer ) : boolean; overload;
  { will get local neighbor and if that is out of bounds, look to neighboring tile }
  var nptr : PCellData;
  begin
    nptr := @neighbors[localneighborix];
    result := not cancelflow and nptr^.valid;
  end;

 procedure calcneighbors( var neighborlist : Tneighborlist );
  var delta : single;
  begin
    with celldata do
     begin
       if validateneighbor(0,6,linesz-1,y) and ( terrainandwater > neighbors[0].terrainandwater ) then
        begin
          delta := terrainandwater - neighbors[0].terrainandwater;
          neighborlist.addneighbor(0, delta );
        end;
       if validateneighbor(1) and  ( terrainandwater > neighbors[1].terrainandwater ) then
        begin
          delta := terrainandwater - neighbors[1].terrainandwater;
          delta := delta * isqrt2; { adjust for diagonal }
          neighborlist.addneighbor(1, delta );
        end;
       if validateneighbor(2,4,x,0) and  ( terrainandwater > neighbors[2].terrainandwater ) then
        begin
          delta := terrainandwater - neighbors[2].terrainandwater;
          neighborlist.addneighbor(2, delta );
        end;
       if validateneighbor(3) and  ( terrainandwater > neighbors[3].terrainandwater ) then
        begin
          delta := terrainandwater - neighbors[3].terrainandwater;
          delta := delta * isqrt2; { adjust for diagonal }
          neighborlist.addneighbor(3, delta );
        end;
       if validateneighbor(4, 2, 0, y ) and  ( terrainandwater > neighbors[4].terrainandwater ) then
        begin
          delta := terrainandwater - neighbors[4].terrainandwater;
          neighborlist.addneighbor(4, delta );
        end;
       if validateneighbor(5) and  ( terrainandwater > neighbors[5].terrainandwater ) then
        begin
          delta := terrainandwater - neighbors[5].terrainandwater;
          delta := delta * isqrt2; { adjust for diagonal }
          neighborlist.addneighbor(5, delta );
        end;
       if validateneighbor(6,0, x, linesz -1) and  ( terrainandwater > neighbors[6].terrainandwater ) then
        begin
          delta := terrainandwater - neighbors[6].terrainandwater;
          neighborlist.addneighbor(6, delta );
        end;
       if validateneighbor(7) and  ( terrainandwater > neighbors[7].terrainandwater ) then
        begin
          delta := terrainandwater - neighbors[7].terrainandwater;
          delta := delta * isqrt2; { adjust for diagonal }
          neighborlist.addneighbor(7, delta );
        end;
     end;
  end;
 var i : integer;
     neighbor : pneighbor;
 begin
   Result := false;
   calcneighbors( parentthread.neighborlist );
   neighbor := pneighbor( parentthread.neighborlist.firstptr );
   for i := 0 to parentthread.neighborlist.count - 1 do if not cancelflow then
    begin
      result := flowtoneighbor( neighbors[ neighbor^.neighborix ]) or result;
      inc( neighbor );
      if ( celldata.waterdepth <= mindepth ) or cancelflow then
         break;
    end;
   parentthread.neighborlist.FreeAll;
 end;

function TFlowRunner.flowtoneighbor( var neighbor : TCellData) : boolean;
 var delta, snowfactor : single;
 begin
   with celldata do
    begin
      delta := ( TerrainAndWater - (Neighbor.waterdepth + Neighbor.terrainheight))*0.5 * amounttoflow;
      { limit delta to water depth }
      limitmax( delta, waterdepth );
      Result := ( delta > 0 ) and not cancelflow;
      if Result then
       begin
         if terrainheight > DefaultSnowLine then
          begin
            snowfactor := terrainheight - Defaultsnowline;
            { limit snow factor }
            limitmax( snowfactor, MaxSnowFactor );
            delta := delta * ( 1 - snowfactor );
          end;
         with flowptrs do
          begin
            deltaptr^ := deltaptr^ - delta;
            if assigned( neighbor.flowptrs.deltaptr ) then
               Neighbor.flowptrs.deltaptr^ := Neighbor.flowptrs.deltaptr^ + delta
            else
               Neighbor.flowptrs.dptr^ := Neighbor.flowptrs.dptr^ + delta;
          end;
         waterdepth := waterdepth - delta;
         TerrainAndWater := TerrainAndWater - delta;
       end;
    end;
 end;

function TFlowRunner.flowtodistantneighbor( distantneighbor : TBaseWaterTerrain;
                                            x, y : integer ) : boolean;
 var distantcell : TCellData;
     flowptrs : TFlowPtrRec;
     ix : integer;
 begin
   result := false;
   if not cancelflow and assigned( distantneighbor ) then
    begin
      ix := x * distantneighbor.watergrid.wh + y;
      flowptrs.hptr := distantneighbor.LinkedTerrainTile.terraingrid.ptrix( ix );
      flowptrs.dptr := distantneighbor.watergrid.ptrix( ix );
      flowptrs.deltaptr := nil;
      setcelldata( distantcell, flowptrs );
      result := flowtoneighbor( distantcell );
    end
 end;


//------------------------------------

constructor TWaterFlowThread.Create;
 var value : boolean;
 begin
   inherited Create( true {suspended} );
   FlowIx := 0;
   deltagrid := tsinglegrid.create(0);
   neighborlist := tneighborlist.create;
   neighborlist.ownitems := true;
   neighborlist.Duplicates := true;
   DirtyTileList := TDirtyList.Create;
 end;

destructor TWaterFlowThread.Destroy;
 begin
   inherited destroy;
   deltagrid.free;
   neighborlist.freeall;
   neighborlist.free;
   DirtyTileList.Free;
 end;

function TWaterFlowThread.DoTaskFromList : boolean;
 var Task : TThreadTask;
 begin
   result := ( not Terminated ) and ( not cancelflow ) and ( TaskList.Count > 0 );
   if result then
    begin
      flowix := flowix mod TaskList.Count;
      task := TThreadTask(TaskList.at(flowix));
      task.parentthread := self;
      task.RunTask;
      inc( flowix );
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

constructor TWaterTask.create( iwatertile : tbasewaterterrain );
 begin
   watertile := iwatertile;
   parentThread := nil;
 end;

procedure TWaterTask.RunTask;
 var UpdateTime : single;
     Delta : single;
 begin
   UpdateTime := GameTime;
   if ( TimeSpeed > 1E-5 ) and ( not cancelflow ) and ( WaterTile.LastUpdateTime >= 0 ) then
    begin
      Delta := UpdateTime - WaterTile.LastUpdateTime;
      Delta := Delta * flowfactor;
      FlowTile( Delta );
    end;
   if cancelflow then
      exit;
   WaterTile.LastUpdateTime := GameTime;
 end;

function TWaterTask.flowtile( amounttoflow : single ) : boolean;
     { local copies of grid parameters }
 var x, y : integer;
     FlowRunner : TFlowRunner;
 begin
   Result := false;
   with twaterflowthread(parentthread) do
    begin
      {$ifdef dbgbehavior}
      write( '~' );
      {$endif}

      DeltaGrid.zerogrid;
      FlowRunner := TFlowRunner.create( twaterflowthread( parentthread ), watertile );
      FlowRunner.amounttoflow := amounttoflow;
      for x := 0 to flowrunner.linesz - 1 do for y := 0 to flowrunner.linesz - 1 do
      if not cancelflow then
       begin
         if FlowRunner.CellData.waterdepth > mindepth then
            Result := FlowRunner.FlowToNeighbors( x, y ) or Result;
         FlowRunner.NextCell; { walk the pointer and positions }
       end;
      if ( not cancelflow ) and Result then
       begin
         watertile.WaterGrid.addgrid(DeltaGrid);
         DirtyTileList.AddTile( watertile );
       end;
      FlowRunner.Free;
    end;
 end;


initialization //===============================================================
  isqrt2 := 1/sqrt(2);
  WaterToFlowList_low := TThreadTaskList.create;
  WaterToFlowList_high := TThreadTaskList.create;
  WaterFlowThreads[0] := TWaterFlowThread.Create;
  WaterFlowThreads[0].TaskList:= WaterToFlowList_high;
  WaterFlowThreads[1] := TWaterFlowThread.Create;
  WaterFlowThreads[1].TaskList:= WaterToFlowList_low;
finalization
  WaterFlowThreads[0].free;
  WaterFlowThreads[1].free;
  WaterToFlowList_low.Free;
  WaterToFlowList_high.Free;
end.


