unit livetime;

interface

uses
  Classes, SysUtils;

const SecondsPerDay = 86400;
      SecondsPerDayFactor : Single = 1/SecondsPerDay;
      StartTime     = 32400; { 9am }

const StandardTimeSpeed : integer = 1; { what game speed will return to when you unpause }
      TimeSpeed : integer = 1; { set to 0 to pause }
      GameTime  : single = StartTime;

function UpdateGameTime( GameSeconds : single ) : single;

implementation //===============================================================

var PercentOfDay : double;

function UpdateGameTime( GameSeconds : single ) : single; { already multiplid by TimeSpeed }
 begin
   GameTime := GameTime + GameSeconds;
   PercentOfDay := frac( GameTime * secondsperdayfactor );
   result := PercentOfDay;
 end;

function getGameClockTime : double;

 begin
   result := PercentOfDay; { precalculated at update }
 end;

end.

