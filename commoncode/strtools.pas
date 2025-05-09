unit StrTools;

{ string parsing tools used by TerrainCommand }

interface

function singleofstr( astr : string ) : single;
function intofstr( astr : string ) : integer;
function parseInt( var params : string;
                   var anint : integer ) : boolean;
function parseSingle( var params : string;
                      var asingle : single ) : boolean;
function parseWorldXY( var params : string;
                       var WorldX, WorldY : single ) : boolean;
function parseTileXY( var params : string;
                       var TileX, TileY : integer ) : boolean;
function nextparam( var params : string;
                 var param : string ) : boolean;
procedure stripleadingspaces( var astr : string ); inline;

implementation


procedure stripleadingspaces( var astr : string ); inline;
 var spacecount : integer;
     l : integer;
 begin
   spacecount := 0;
   l := length( astr );
   while ( spacecount < l ) and ( astr[spacecount+1] = ' ' ) do
      inc( spacecount );
   delete( astr, 1, spacecount );
 end;

function nextparam( var params : string;
                 var param : string ) : boolean;
 var i : integer;
 begin
   param := params;
   i := pos( ',', params );
   result := i > 0;
   if result then
    begin
      param := copy( params, 1, i - 1 );
      delete( params, 1, i );

    end
   else
    begin
      param := params;
      params := '';
      result := param <> '';
    end;

 end;

function parseTileXY( var params : string;
                       var TileX, TileY : integer ) : boolean;
 var param : string;
 begin
   result := nextparam( params, param );
   if result then
    begin
      TileX := intofstr( param );
      result := nextparam( params, param );
      if result then
         TileY := intofstr( param );
    end;
 end;

function parseWorldXY( var params : string;
                       var WorldX, WorldY : single ) : boolean;
 var param : string;
 begin
   result := nextparam( params, param );
   if result then
    begin
      WorldX := singleofstr( param );
      result := nextparam( params, param );
      if result then
         WorldY := singleofstr( param );
    end;
 end;

function parseInt( var params : string;
                   var anint : integer ) : boolean;
var param : string;
begin
  result := nextparam( params, param );
  if result then
     anint := intofstr( param );
end;

function parseSingle( var params : string;
                      var asingle : single ) : boolean;
var param : string;
begin
  result := nextparam( params, param );
  if result then
     asingle := singleofstr( param );
end;

function intofstr( astr : string ) : integer;
 var code : integer;
 begin
   val( AStr, result, code );
 end;

function singleofstr( astr : string ) : single;
var code : integer;
 begin
   val( AStr, result, code );
 end;

end.
