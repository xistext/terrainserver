unit watercolor;

{ calculates the texture coordinates based on depth and other factors }

interface

uses
   Classes, SysUtils,
   CastleVectors,
   basetools,
   TerrainData,
   water_low, waterparams;

function CalcDepthAndTexture( var waterdepth : single;
                              floraheight, terrainheight : single;
                              wiggle : boolean ) : TVector2;

implementation

function whichlayer( waterdepth, floraheight, terrainheight : single ) : integer;
 { determines if it is building this cell as snow, water or flora,
   0 = water, 1 = snow, 2 = flora }
 var dosnow, doflora : boolean;
 begin
   dosnow := ( terrainheight > defaultsnowline ) and ( waterdepth > 0.001 );
   doflora := ( not dosnow ) and
             ( floraheight > 0 ) and (( floraheight - waterdepth ) > 0.002 );
   result := ord( dosnow ) + ord( doflora ) SHL 1;
 end;

function CalcSnowDepthAndTexture( var waterdepth : single;
                                  floraheight, terrainheight : single;
                                  wiggle : boolean ) : TVector2;
begin
  result := vector2( texsnow, texsnow ); { white }
  waterdepth := sqrt( waterdepth ) + floraheight; { compact extra deep snow and expand shallow snow toward 1 unit }
end;

function CalcFloraDepthAndTexture( var waterdepth : single;
                                  floraheight, terrainheight : single;
                                  wiggle : boolean ) : TVector2;
var texy, fdepth : single;
    haswater : boolean;
    whichcase : integer;
begin
  haswater := waterdepth > 0;
  whichcase := ord( haswater ) + ord( wiggle ) shl 1;
  case whichcase of
    1, 3 : { haswater, any wiggle }
           texy := ( floraheight + waterdepth * 2 ) * 10; { more wet more green }
    0 : { not has water, not wiggle }
        texy := floraheight * 10;
    2 : { not has water, wiggle }
        texy := floraheight * 10 + 0.02 * random - 0.01;  { dry }
   end;
  limitminmax( texy, mintexflora_y, maxtexflora_y );
  Result := vector2( texflora_x, texy );
   fdepth := sqrt( floraheight );
  whichcase := ord( fdepth > 0 ) + ord( fdepth >= MinDisplayFloraHeight ) shl 1;
  case whichcase of
     1 : { fdepth > 0, fdepth < Min }
         fdepth  := MinDisplayFloraHeight;
     0, 2 : { fdepth = 0 }
         fdepth := ZeroDisplayFloraHeight;
   end;
  waterdepth := fdepth;
end;

 procedure modifywdepth( var wdepth : single;
                         terrainheight : single;
                         wiggle : boolean );
  { will limit wdepth and add wiggle as needed }
  var haswater : boolean;
      whichcase : integer;
  begin
    haswater := wdepth > 0;
    whichcase := ord( haswater ) +
                 ord( wiggle ) shl 1 +
                 ord( haswater and ( wdepth < MinDisplayWaterHeight )) shl 2 +
                 ord( terrainheight > 0 ) shl 3;
    case whichcase of
       8, 10, 14 : { not haswater, whatever wiggle, whatever min }
           wdepth := ZeroDisplayWaterHeight;
       11 : { haswater, wiggle, not below min }
           wdepth := wdepth - random*0.05*wdepth; { wiggle }
       13, 15 : { haswater, whatever wiggle, below min }
           wdepth := MinDisplayWaterHeight;
     end;
  end;

function CalcWaterDepthAndTexture( var waterdepth : single;
                                   floraheight, terrainheight : single;
                                   wiggle : boolean ) : TVector2;
var wdepth : single;
begin
  wdepth := waterdepth;
  modifywdepth( wdepth, terrainheight, wiggle );
  waterdepth := wdepth;
  limitminmax( wdepth, texwet, texmax );
  result := vector2( wdepth, texwater_y );
end;

type TDepthAndTextureFunc = function ( var waterheight : single;
                                       floraheight, terrainheight : single;
                                       wiggle : boolean ) : TVector2;
var CalcFuncs : array[0..2] of TDepthAndTextureFunc;

function CalcDepthAndTexture( var waterdepth : single;
                              floraheight, terrainheight : single;
                              wiggle : boolean ) : TVector2;
var calcfunc : TDepthAndTextureFunc;
begin
calcfunc := CalcFuncs[whichlayer( waterdepth, floraheight, terrainheight )];
result := calcfunc( waterdepth, floraheight, terrainheight, wiggle );
end;


//--------------------------





initialization
  calcfuncs[0] := @CalcWaterDepthAndTexture;
  calcfuncs[1] := @CalcSnowDepthAndTexture;
  calcfuncs[2] := @CalcFloraDepthAndTexture;
end.

