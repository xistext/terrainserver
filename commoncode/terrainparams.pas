unit terrainparams;

interface

uses classes, collect, sysutils,
     castleterrain,
     terservercommon;

const buildradius : integer = 4; //4
      disablewater : boolean = false;

      GDefGridCellCount : integer = 120;
      GDefGridStep : single = 0.5;

    //  GSteepEmphasis : single = 10;
      GTerrainSeed   : integer = 0;

      freshdigcolor : integer = 0;

         (*
const transitionsize = 20; { how many base grid cells to use as transition zone
                             around a tile where it borders a tile with different
                             terraindata source }
           *)
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

const TerrainTypes : TTerrainTypeList = nil;

implementation

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

