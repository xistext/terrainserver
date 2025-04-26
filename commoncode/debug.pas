UNIT Debug;

{ basic debugging output
  1995-1998 Erik Johnson }
{stripped out file io}

INTERFACE

{$J+}

USES Classes, Windows, SysUtils;
//{$define dbgfile}  // writeln debug output to debug file in current dir
{$define viewdebug}  // writeln debug output to crt window

PROCEDURE DBGWriteln( S: STRING = '' );
PROCEDURE DBGLnWrite( S: STRING ); { guarantees new line at beginning, but doesn't add ln at end }
PROCEDURE DBGWrite( S: STRING );

PROCEDURE DBGNew( S: STRING );
PROCEDURE DBGDispose( S: STRING );

IMPLEMENTATION //===============================================================

CONST LinePos : INTEGER = 0;

{-------------------------------------}
FUNCTION WaitforWrite : BOOLEAN;
 BEGIN
   {$ifdef dbgfile }
   IF ReuseDBG AND NOT DebugOutput.DBGFileOpen THEN
      Result := DebugOutput.WaitforWrite
   ELSE
   {$endif}
      Result := TRUE;
 END;

VAR BlankLine : STRING[80];

PROCEDURE DBGWriteln( S: String = '' );
 VAR LastPos : INTEGER;
 BEGIN
   LastPos := LinePos;
   {$ifdef dbgfile}
   {$I-}
   IF NOT NoLog THEN
      DebugOutput.Writeln( S );
   {$endif}
   {$ifdef viewdebug}
   Writeln( S );
   {$endif}
   {$I+}
   LinePos := 0;
 END;

PROCEDURE DBGWrite( S: String );
 BEGIN
   {$ifdef dbgfile}
   {$I-}
   IF NOT NoLog THEN
      DebugOutput.Write( S );
   {$I+}
   {$endif}
   {$ifdef viewdebug}
   Write( S );
   {$endif}
   LinePos := LinePos + LENGTH( S );
 END;

PROCEDURE DBGLnWrite( S: STRING );
 { guarantees new line at beginning, but doesn't add ln at end }
 BEGIN
   IF LinePos > 0 THEN
      DBGWriteln( '' );
   DBGWrite( S )
 END;

PROCEDURE DBGNew( S: String );
 BEGIN
   {$ifdef dbg}
   INC( NewCount );
   DBGWriteln( DBG, 'NEW     (',NewCount:4,')+ : ',S );
   {$endif}
 END;

PROCEDURE DBGDispose( S: String );
 BEGIN
   {$ifdef dbg}
   INC( DoneCount );
   DBGWriteln( DBG, 'DISPOSE (',DoneCount:4,')- : ',S );
  {$endif}
 END;

INITIALIZATION
 {$ifdef viewdebug}
 AllocConsole;
 {$ifdef fpc}
 IsConsole := True; // in System unit
 SysInitStdIO;      // in System unit
 {$endif}
 {$endif}
END.



