unit waterparams;

interface

const DefaultSnowLine : single = 8; { 8 world units is about halfway up the default mountains }
      StartWaterDepth   = 0.1;
      StartFloraDepth = 0.02;
      //ErosionFactor = 0.01;
      mindepth : single = -5; { amount water will still flow underground, helps to level water edges }
      flowfactor : single = 0.2;
      snowmeltfactor : single = 0.01;
      WaterTextureUrl = 'castle-data:/textures/testwater3.png';

      { low positive values will be lifted to avoid moire interaction with terrain }
      MinDisplayFloraHeight = 0.02;
      MinDisplayWaterHeight = 0.002;
      { low negative values will be lowered to avoid moire interaction with terrain }
      ZeroDisplayFloraHeight = -0.1;
      ZeroDisplayWaterHeight = -0.01;

{ texture locations }
const texsnow = 0.5/128;
      texwet = 4.5/128;
      texmax = 127/128;
      texflora_x = 2.5/128;
      mintexflora_y = 1/128;
      maxtexflora_y = 127/128;
      texwater_y = 0.5;

{ used by flow }
const MaxSnowFactor : single = 0.999; { limits snow flow factor }

implementation

end.

