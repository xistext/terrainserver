unit toolbuild;

interface

uses
  Classes, SysUtils,
  CastleVectors, CastleBoxes, CastleScene, CastleCameras, CastleTransform, CastleKeysMouse, CastleColors,
  baseTools,
  layermarkup;

type (*TToolContext = record
        ParentView     : TCastleTransform;
        MainCamera     : TCastleCamera;
        MainNavigation : TCastleNavigation;
      end;*)

     TToolNavigation = class( TCastleWalkNavigation )
        constructor Create(AOwner: TComponent); override;

        function MoveAllowed2(terrainheight : THeightAboveTerrainEvent;
                              const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3 ): boolean; virtual;
        public
        LockToGround : boolean;
      end;

     TToolEvent = class
        ParentView     : TCastleTransform;
        MainCamera     : TCastleCamera;
        MainNavigation : TToolNavigation;

        terrainheight : THeightAboveTerrainEvent;
        constructor create( iParentView : TCastleTransform;
                            iMainCamera : TCastleCamera;
                            iMainNavigation : TToolNavigation;
                            iterrainheight : THeightAboveTerrainEvent );
        function press(const Event: TInputPressRelease): boolean; virtual;
        function motion( itemhit : TCastleTransform;
                         const point : TVector3 ) : boolean; virtual;
        function mousewheel( direction : integer ) : boolean; virtual;
        function release( itemhit : TCastleTransform;
                          const pos : TVector3 ) : boolean; virtual;

      end;

implementation //===============================================================


constructor TToolNavigation.Create(AOwner: TComponent);
 begin
   inherited;
   MouseDragMode := mdNone;
   MouseLook := false;
   Gravity := false;
   MoveSpeed := 10;
   PreferredHeight := 8;
   Radius := 0.2;
   LockToGround := false;
 end;

function TToolNavigation.MoveAllowed2(terrainheight : THeightAboveTerrainEvent;
                                      const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3 ): boolean;
 var terrainh : single;
 begin
   TerrainHeight( ProposedNewPos, TerrainH );
   NewPos := ProposedNewPos;
   locktoground := ( NewPos.Y - radius < Terrainh ) or locktoground;
   if locktoground then
    begin
      NewPos := Vector3(ProposedNewPos.X, TerrainH + radius, ProposedNewPos.Z );
      MoveHorizontalSpeed := sqrt(radius);
    end;
 end;

//------------------------------

constructor TToolEvent.create( iParentView : TCastleTransform;
                               iMainCamera : TCastleCamera;
                               iMainNavigation : TToolNavigation;
                               iterrainheight : THeightAboveTerrainEvent );
 begin
   assert( assigned( iParentView ));
   ParentView := iParentView;
   MainCamera := iMainCamera;
   MainNavigation := iMainNavigation;
   terrainHeight := iTerrainHeight;
 end;

function TToolEvent.mousewheel( direction : integer ) : boolean;
var h, amt : single;
    delta : single;
    pos : TVector3;
    TerrainH : single;
begin
(*   if altdown or ( ToolEvent = GDefaultTool ) or not ToolEvent.mousewheel( direction ) then
   begin*)
     h := MainCamera.translation.y;
     pos := Vector3( MainCamera.Translation.X, h, MainCamera.Translation.Z );
     TerrainHeight( Pos, TerrainH );
     amt := h - TerrainH;
     if amt < 1 then
        amt := 1;
     { if no tools are taking mousewheel then use it to change view height }
     delta := direction/3 * amt;
     h := h + delta;
     if h - MainNavigation.radius < TerrainH then
      begin
        MainNavigation.locktoground := true;
        h := MainNavigation.radius + TerrainH;
        MainNavigation.MoveHorizontalSpeed := 0.1;
      end
     else
        MainNavigation.locktoground := false;
     pos.Y := h;
     MainNavigation.MoveHorizontalSpeed := sqrt(amt);
   MainCamera.Translation := pos;
   result := true;
end;

function TToolEvent.press(const Event: TInputPressRelease): boolean;
 begin
   result := false;
 end;

function TToolEvent.motion( itemhit : TCastleTransform;
                            const point : TVector3 ) : boolean;
 begin
   result := false;
 end;

function TToolEvent.release( itemhit : TCastleTransform;
                             const pos : TVector3 ) : boolean;
 begin
   result := false;
 end;

end.

