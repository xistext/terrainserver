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
        public
        LockToGround : boolean;
      end;

     TToolEvent = class
        ParentView     : TCastleTransform;
        MainCamera     : TCastleCamera;
        MainNavigation : TCastleWalkNavigation;

        terrainheight : THeightAboveTerrainEvent;
        constructor create( iParentView : TCastleTransform;
                            iMainCamera : TCastleCamera;
                            iMainNavigation : TCastleWalkNavigation;
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

constructor TToolEvent.create( iParentView : TCastleTransform;
                               iMainCamera : TCastleCamera;
                               iMainNavigation : TCastleWalkNavigation;
                               iterrainheight : THeightAboveTerrainEvent );
 begin
   assert( assigned( iParentView ));
   ParentView := iParentView;
   MainCamera := iMainCamera;
   MainNavigation := iMainNavigation;
   terrainHeight := iTerrainHeight;
 end;

function TToolEvent.mousewheel( direction : integer ) : boolean;
 begin
   result := false;
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

