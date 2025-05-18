unit terrainparams;

interface

uses classes, collect, sysutils,
     castleterrain, castlevectors,
     terservercommon;

const buildradius : integer = 4; //4
      disablewater : boolean = false;

      GDefGridCellCount : integer = 120;
      GDefGridStep : single = 0.5;

    //  GSteepEmphasis : single = 10;
      GTerrainSeed   : integer = 0;
      freshdigcolor : integer = 0;

type TTerrainNoiseParams = record
        Octaves    : single;
        Smoothness : single;
        Amplitude  : single;
        Frequency  : single;
        Heterogeneous : single;
      end;

     TTerrainParams = class
        Name  : string;
        NoiseParams : TTerrainNoiseParams;
        constructor create( iName : string );

        private
        fNoise : TCastleTerrainNoise;
        function getNoise : TCastleTerrainNoise;
        public
        property Noise : TCastleTerrainNoise read getNoise;
      end;

     TTerrainTypeList = class( TSortedCollection )
        function findTerrainType( name : string;
                                  out TerrainType : TTerrainParams ) : boolean;
        function compare(item1, item2 : pointer) : integer; override;
        function keyof( item : pointer ) : pointer; override;
      end;

function divofLOD( LOD : dword ) : dword;
function worldtotile( wpos : tvector2 ) : tvector2;

const TerrainTypes : TTerrainTypeList = nil;

implementation

function divofLOD( LOD : dword ) : dword;
 begin
   result := 60;
   case LOD of
      0, 1 : result := 1;
      2 : result := 2;
      3 : result := 3;
      4 : result := 4;
      5 : result := 5;
      6 : result := 6;
      7 : result := 8;
      8 : result := 10;
      9 : result := 12;
      10 : result := 15;
      11 : result := 20;
      12 : result := 24;
      13 : result := 30;
      14 : result := 40;
    end;
 end;

function worldtotile( wpos : tvector2 ) : tvector2;
 var sizefactor : single;
     tilesz : single;
 begin
   tilesz := GDefGridCellCount * GDefGridStep;
   sizefactor := 1/tilesz;
   result := vector2( frac( wPos.X * SizeFactor + 0.5 ), frac( wPos.Y * SizeFactor + 0.5 ));
 end;

//-----------------------------

function TTerrainTypeList.compare(item1, item2 : pointer) : integer;
 begin
   result := comparetext( pchar( item1 ), pchar( item2 ));
 end;

function TTerrainTypeList.keyof( item : pointer ) : pointer;
 begin
   Result := PChar(TTerrainParams( item ).Name);
 end;

function TTerrainTypeList.findTerrainType( name : string;
                                        out TerrainType : TTerrainParams ) : boolean;
 var i : integer;
 begin
   Result := search( pchar(name), i );
   if result then
      TerrainType := TTerrainParams( At( i ))
 end;

//------------------------------------------

constructor TTerrainParams.create( iName : string );
 begin
   inherited create;
   Name := iName;
   fNoise := nil;
   FillChar( fNoise, sizeof( fNoise ), 0 );
 end;

function TTerrainParams.getNoise : TCastleTerrainNoise;
 begin
   if not assigned( fNoise ) then
    begin
      fNoise := TCastleTerrainNoise.create( nil );
      with fNoise do
       begin
         Seed := GTerrainSeed;
         Interpolation := niSpline; // slowest but best quality
         Octaves := NoiseParams.Octaves;
         Smoothness := NoiseParams.Smoothness;
         Amplitude := NoiseParams.Amplitude;
         Frequency := NoiseParams.Frequency;
         Heterogeneous := NoiseParams.Heterogeneous;
       end;
    end;
   result := fNoise;
 end;

//------------------------------------------

procedure InitializeDefaultTypes;
 var item : TTerrainParams;
 begin
   item := TTerrainParams.create( 'mountain' );
   with item.NoiseParams do
    begin
      Octaves := 6.94;
      Smoothness := 1.63; //1.9;
      Amplitude := 7.85;
      Frequency := 0.04;
      Heterogeneous := 0.64;
    end;
   TerrainTypes.insert( item );
   item := TTerrainParams.create( 'flat' );
   with item.NoiseParams do
    begin
      Octaves := 6.94;
      Smoothness := 1.9;
      Amplitude := 1;
      Frequency := 0.04;
      Heterogeneous := 0.64;
    end;
   TerrainTypes.insert( item );
 end;


initialization
   freshdigcolor := encodesplatcell( 13, 12, 6, 8, 1, 13 );
   TerrainTypes := TTerrainTypeList.create;
   InitializeDefaultTypes;
finalization
   TerrainTypes.Free;
end.

