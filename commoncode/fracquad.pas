Unit fracquad;

{ quadtree index }

interface

TYPE TWIndex = dword;
     TWBounds = record { record for single precision bounds rect }
                  XMin, XMax, YMin, YMax : single;
                end;


const AreaBit = 5; { actually 6th bit 0 is area, 1 is point }

function WIndexofPoint( X, Y : single ) : LONGINT; { 2 dimensional }

FUNCTION WIndexOfBounds( MinX, MinY, MaxX, MaxY : single ) : INTEGER;

FUNCTION PointOfWIndex( WIndex : Integer; VAR X, Y : single ) : BOOLEAN;

FUNCTION FracOverlap( Frac1, Frac2 : LONGINT ) : INTEGER;

FUNCTION AddWIndexLevel( WIndex : TWIndex;
                         High   : BOOLEAN ) : TWIndex;

PROCEDURE GetWIndexDepth( WIndex : TWIndex;
                          VAR Depth : INTEGER );
FUNCTION SetWIndexDepth( WIndex : TWIndex;
                         iDepth : INTEGER ) : INTEGER;
{ bit 0 is world, 31 is point, 1..26 are areas }

FUNCTION GetEndWix( Wix : TWIndex ) : TWIndex;

FUNCTION StrOfWix( Wix : TWindex ) : STRING;

VAR WorldB : TWBounds; { stores global world bounds }

IMPLEMENTATION //===============================================================

USES BitCalc, sysUtils;

FUNCTION AddWIndexLevel( WIndex : TWIndex;
                         High   : BOOLEAN ) : TWIndex;
 VAR Depth : INTEGER;
 BEGIN
   Result := WIndex;
   GetWIndexDepth( WIndex, Depth );
   IF Depth < ( 32 - AreaBit ) THEN
    BEGIN
      INC( Depth ); { 1..27 }
      IF High THEN
         TurnBitOn( WIndex, 32 - Depth )
      ELSE
         TurnBitOff( WIndex, 32 - Depth );
      Result := SetWIndexDepth( WIndex, Depth );
    END
 END;

PROCEDURE GetWIndexDepth( WIndex : TWIndex;
                          VAR Depth : INTEGER );
 { returns depth of the WIndex, 0..26 for area or 31 if point }
 BEGIN
   Depth := 32; { point depth is full 32 bits }
   IF NOT BitIsOn( WIndex, AreaBit ) THEN
    BEGIN { area depth, 0 is whole world }
      Depth := WIndex SHL ( 32 - AreaBit );
      Depth := Depth SHR ( 32 - AreaBit );
    END
 END;

FUNCTION SetWIndexDepth( WIndex : TWIndex;
                         iDepth : INTEGER ) : INTEGER;
 BEGIN
   Result := WIndex SHR ( AreaBit - 1 ); { strip old depth }
   Result := Result SHL ( AreaBit - 1 );
   Result := Result OR iDepth;
 END;

FUNCTION FracOverlap( Frac1, Frac2 : LONGINT ) : INTEGER;
 { calculates the 'similarity' of two points }
 BEGIN
   Result :=0;
   WHILE ( Result < 32 ) AND
         (( Frac1 AND ( 1 SHL Result )) = ( Frac2 AND ( 1 SHL Result ))) DO
      INC( Result );
 END;

FUNCTION WIndexofPoint( X, Y : single ) : LONGINT;
 { encodes fractal value of point, by alternately dividing width and height of space }
 VAR fminx, fmaxx, fminy, fmaxy, middle : single;
     bit        : INTEGER;
     procedure DoHorizontal;
      begin
        Middle := ( fminx + fmaxx )*0.5;
        IF X > Middle THEN
         BEGIN
           TurnBitOn( Result, bit );
           fMinX := Middle;
         END
        ELSE
           fMaxX := Middle;
      end;
     procedure DoVertical;
      begin
        Middle := ( fminy + fmaxy )*0.5;
        IF Y > Middle THEN
         BEGIN
           TurnBitOn( Result, bit );
           fMinY := Middle;
         END
        ELSE
           fMaxY := Middle;
      end;
 var i : integer;
 BEGIN
   Result := 0; { not in }
   fminx := WorldB.XMin;
   fmaxx := WorldB.XMax;
   fminy := WorldB.YMin;
   fmaxy := WorldB.YMax;
   bit := 31;
   FOR i := 16 DOWNTO 0 DO
    BEGIN
      DoHorizontal;
      dec( bit );
      DoVertical;
      dec( bit );
    END;
 END;

FUNCTION PointOfWIndex( WIndex : LONGINT; VAR X, Y : single ) : BOOLEAN;
 VAR fminx, fmaxx, fminy, fmaxy, Middle : single;
     bit : INTEGER;
  procedure DoHorizontal;
   BEGIN
     Middle := ( fminX + fmaxX )*0.5;
     IF ( WIndex AND ( 1 SHL bit )) > 0 THEN
      BEGIN
        fminx := Middle;
        X := Middle;
      END
     ELSE
        fmaxx := Middle;
   END;
  procedure DoVertical;
   BEGIN
     Middle := ( fminy + fmaxy )*0.5;
     IF ( WIndex AND ( 1 SHL bit )) > 0 THEN
      BEGIN
        fminy := Middle;
        Y     := Middle
      END
     ELSE
        fmaxy := Middle;
   END;

 var i : integer;
 BEGIN
   fmaxx := WorldB.XMax;
   fmaxy := WorldB.YMax;
   fminx := WorldB.XMin;
   fminy := WorldB.YMin;
   X := 0;
   Y := 0;
   bit := 31;
   FOR i := 15 DOWNTO 0 DO
    BEGIN
      DoHorizontal;
      dec( bit );
      DoVertical;
      dec( bit );
    END;

   Result := TRUE;
 END;

FUNCTION ContainsBounds( ObjXMin, ObjYMin, ObjXMax, ObjYMax : single;
                         XMin, YMin, XMax, YMax : single;
                         Middle      : single;
                         Horizontal : BOOLEAN ) : INTEGER;
 { 0 no
   1 in left/lower child
   2 in right/upper child
   3 fully in this node, and partially in each child }
 BEGIN
   Result := 0;
   IF ( ObjYMin > YMin ) AND ( ObjYMax > YMin ) AND
      ( ObjYMin < YMax ) AND ( ObjYMax < YMax ) AND
      ( ObjXMin > XMin ) AND ( ObjXMax > XMin ) AND
      ( ObjXMin < XMax ) AND ( ObjXMax < XMax ) THEN
    BEGIN
      Result := 3;
      IF Horizontal THEN { split space horizontally }
       BEGIN
         IF ( ObjXMin < Middle ) AND ( ObjXMax < Middle ) THEN
            Result := 1
         ELSE
         IF ( ObjXMin > Middle ) AND( ObjXMax > Middle ) THEN
            Result := 2
       END
      ELSE               { split space vertically }
       BEGIN
         IF ( ObjYMin < Middle ) AND ( ObjYMax < Middle ) THEN
            Result := 1
         ELSE
         IF ( ObjYMin > Middle ) AND ( ObjYMax > Middle ) THEN
            Result := 2
       END
    END
 END;


FUNCTION WIndexOfBounds( MinX, MinY, MaxX, MaxY : single ) : INTEGER;
 { returns windex for area represented by given bounds }
 VAR fminx, fmaxx, fminy, fmaxy, middle : single;
     Depth, bit, ChildIndex, CrossCount   : INTEGER;
     Horizontal, IsDone       : BOOLEAN;
 BEGIN
   IF ( MinX = MaxX ) AND ( MinY = MaxY ) THEN
      Result := WIndexOfPoint( MinX, MaxX )
   ELSE
    BEGIN
      fmaxx := WorldB.XMax;
      fmaxy := WorldB.YMax;
      fminx := WorldB.XMin;
      fminy := WorldB.YMin;
      Horizontal := TRUE;
      CrossCount := 0;
      IsDone := FALSE;
      Result := 0;
      Depth := 0;
      Bit := 31;

      WHILE NOT IsDone AND ( Bit > AreaBit ) DO
       BEGIN
         IF Horizontal THEN
            Middle := ( fminX + fmaxX ) * 0.5
         ELSE
            Middle := ( fminY + fmaxY ) * 0.5;

         ChildIndex := ContainsBounds( MinX, MinY, MaxX, MaxY,
                                       fMinX, fMinY, fMaxX, fMaxY,
                                       Middle, Horizontal );
         CASE ChildIndex OF
            0 : IsDone := TRUE; { doesn't contain }
            1 : BEGIN { low side of middle }
                  DEC( Bit );
                  IF Horizontal THEN
                     fmaxX := Middle
                  ELSE
                     fmaxY := Middle;
                  IF CrossCount = 0 THEN
                     INC( Depth )
                  ELSE
                     IsDone := TRUE;
                END;
            2 : BEGIN { highside of middle }
                  TurnBitOn( Result, Bit );
                  DEC( Bit );
                  IF Horizontal THEN
                     fminX := Middle
                  ELSE
                     fminY := Middle;
                  IF CrossCount = 0 THEN
                     INC( Depth )
                  ELSE
                     IsDone := TRUE;
                END;
            3 : BEGIN
                  TurnBitOn( Result, Bit );
                  IsDone := TRUE; { crosses, on both sides of middle }
                  INC( CrossCount );
                END;
          END;
         Horizontal := NOT Horizontal;
       END;
      Result := Result OR Depth; { add depth bits }
    END
 END;

FUNCTION GetEndWix( Wix : TWIndex ) : TWIndex;
 { calculates the end of the child range of the given Wix value }
 VAR WixDepth, Mask : INTEGER;
 CONST BaseMask = $FFFFFFFF;
 BEGIN
   GetWIndexDepth( Wix, WixDepth );
   Mask := BaseMask SHR WixDepth;
   Result := WIx OR Mask;     { turn all contained bits to 1 }
 END;

FUNCTION StrOfWix( Wix : TWindex ) : STRING;
 VAR AStr : STRING[50];
     I, Depth: INTEGER;
 BEGIN
   AStr := '|';
   FOR I := 0 TO ( 31 - AreaBit ) DO
    BEGIN
      IF BitIsOn( Wix, 31 - I ) THEN
         AStr := AStr + '1'
      ELSE
         AStr := AStr + '0';
      CASE I OF
         7,23 : AStr := AStr + ':';
         15   : AStr := AStr + '|';
       END;
    END;
   GetWIndexDepth( WIx, Depth );
   AStr := AStr + ';'+IntToStr( Depth )+'|';
   Result := AStr;
 END;

INITIALIZATION
  { default global radians }
  WorldB.XMin := -Pi;
  WorldB.XMax := Pi;
  WorldB.YMin := -Pi/2;
  WorldB.YMax :=  Pi/2;
END.
