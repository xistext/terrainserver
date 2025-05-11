/* OpenGL shader effect (used to enhance the Castle Game Engine shaders,
   see https://castle-engine.io/compositing_shaders.php ),
   applied over terrain.

   This adjusts terrain color, mixing textures, based on current height. */

uniform sampler2D tex_1;
uniform sampler2D tex_2;
uniform sampler2D tex_3;
uniform sampler2D tex_4;
//uniform sampler2D ramp;

uniform int splat_sz;
uniform int splatmap[3721];

uniform vec4 grid_color;
uniform vec4 grid10_color;
uniform vec4 contour_color;

uniform bool blur;

// These values are packed in vec4, one float per layer
uniform vec4 uv_scale;
uniform vec4 metallic;
uniform vec4 roughness;

uniform float height_1;
uniform float height_2;
uniform float grid_scale;
uniform float contour_scale;

uniform float layers_influence;
uniform float steep_emphasize;

varying vec3 terrain_position;
varying vec3 terrain_normal;

int contour_mode = 1;

// avoid redeclaring when no "separate compilation units" available (OpenGLES)
#ifndef GL_ES
vec4 castle_texture_color_to_linear(const in vec4 srgbIn);
#endif

vec3 rgb2hsv(vec3 c)
{
    vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
    vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

vec3 hsv2rgb(vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

  // terrain textures
  vec2 uv = vec2(terrain_position.x, -terrain_position.z);
  vec3 tex[4] = vec3[4]( castle_texture_color_to_linear(texture2D(tex_1, uv * uv_scale.x)).rgb,
                         castle_texture_color_to_linear(texture2D(tex_2, uv * uv_scale.y)).rgb,
  			 castle_texture_color_to_linear(texture2D(tex_3, uv * uv_scale.z)).rgb,
			 castle_texture_color_to_linear(texture2D(tex_4, uv * uv_scale.w)).rgb  );

float factor = 1.0/15.0;

vec4 decodecolor( int c, out vec4 texture )
 { int texid = ( c >> 16 ) & 15;
   float texalpha = (( c >> 20 ) & 15 ) * factor;
   vec3 texc = tex[texid];
   texture = vec4( texc.r, texc.g, texc.b, texalpha );
   return vec4( c & 15, ( c >> 4 ) & 15, ( c >> 8 ) & 15, ( c >> 12 ) & 15 ) * factor;
  }

vec4 getsplatcolor( int ax, int ay, out vec4 texture )
 { if (( ax < splat_sz ) && ( ay < splat_sz ))
    { int c = splatmap[( ax * splat_sz ) + ay];
      return decodecolor( c, texture );
      }
   else
    { return decodecolor( 65535, texture );
     }
  }

vec4 bilerp( int ax, int ay, in vec2 pos, int steps, out vec4 texture )
 {  //if ( steps > 0 )
    // { pos = vec2( floor( pos.x * steps) / steps, floor( pos.y * steps) / steps );}
    float ratio4 = pos.y;
    float ratio3 = 1 - ratio4;
    float ratio2 = pos.x;
    float ratio1 = 1 - ratio2;
    vec4 tx0;
    vec4 cx0 = getsplatcolor( ax, ay, tx0 );
    if (( ratio2 != 0 ) || ( ratio4 != 0 ))
     { vec4 tx1;
       vec4 cx1 = getsplatcolor( ax + 1, ay, tx1 );

       vec4 ty0;
       vec4 cy0 = getsplatcolor( ax, ay+1, ty0 );

       vec4 ty1;
       vec4 cy1 = getsplatcolor( ax + 1, ay+1, ty1 );

       float alpha = ratio3 * ( cx0.a * ratio1 + cx1.a * ratio2 ) + ratio4 * ( cy0.a * ratio1 + cy1.a * ratio2 );
       vec3 c = ratio3 * ( cx0.rgb * ratio1 + cx1.rgb * ratio2 ) + ratio4 * ( cy0.rgb * ratio1 + cy1.rgb * ratio2 );

       float talpha = ratio3 * ( tx0.a * ratio1 + tx1.a * ratio2 ) + ratio4 * ( ty0.a * ratio1 + ty1.a * ratio2 );
       vec3 t = ratio3 * ( tx0.rgb * ratio1 + tx1.rgb * ratio2 ) + ratio4 * ( ty0.rgb * ratio1 + ty1.rgb * ratio2 );
       texture = vec4( t.r, t.g, t.b, talpha );

       return vec4( c.r, c.g, c.b, alpha );

      }
      else
       { return cx0; }
  }

  vec4 lerpx( int ax, int ay, in float posx, int steps, out vec4 texture )
 {  if ( steps > 0 )
      { posx = floor( posx * steps)/steps; }
    float ratio2 = posx;
    float ratio1 = 1 - ratio2;
    vec4 tx0;
    vec4 cx0 = getsplatcolor( ax, ay, tx0 );
    if ( ratio2 != 0 )
     { vec4 tx1;
       vec4 cx1 = getsplatcolor( ax + 1, ay, tx1 );
       float alpha = tx0.a * ratio1 + tx1.a * ratio2;
       vec3 t = tx0.rgb * ratio1 + tx1.rgb * ratio2;
       texture = vec4( t.r, t.g, t.b, alpha );

       return ( cx0 * ratio1 + cx1 * ratio2 );
      }
      else
       { return cx0; }
  }

vec4 lerpy( int ax, int ay, in float posy, int steps, out vec4 texture )
 {  if ( steps > 0 )
      { posy = floor( posy * steps) / steps; }
    float ratio4 = posy;
    float ratio3 = 1 - ratio4;
    vec4 ty0;
    vec4 cy0 = getsplatcolor( ax, ay, ty0 );
    if ( ratio4 != 0 )
     { vec4 ty1;
       vec4 cy1 = getsplatcolor( ax, ay+1, ty1 );
       float alpha = ratio3 * ty0.a + ratio4 * ty1.a;
       vec3 t = ratio3 * ty0.rgb + ratio4 * ty1.rgb;
       texture = vec4( t.r, t.g, t.b, alpha );

       return ratio3 * cy0 + ratio4 * cy1;
      }
      else
       { return cy0; }
  }

float absroundfract( float v )
 { return abs( v - round( v )); }

void drawgrid( inout vec3 terrain_color, vec2 uv )
 {
   float grid10_scale = grid_scale * 0.1;
   float grid_liner = 0.015 * grid10_scale;
   vec2 g = uv * grid10_scale;
   vec4 gcolor = grid10_color;

   bool doline = ( absroundfract( g.x ) < grid_liner ) || ( absroundfract( g.y ) < grid_liner );
   if ( !doline )
    { grid_liner = 0.015 * grid_scale;
      g = uv * grid_scale;
      doline = ( absroundfract( g.x ) < grid_liner ) || ( absroundfract( g.y ) < grid_liner );
      gcolor = grid_color;
      gcolor.a = gcolor.a * int( doline );
     }
   terrain_color = mix( terrain_color, gcolor.rgb, gcolor.a );
  }

void drawcontourline( inout vec3 terrain_color,
                      float h )
 {
   float gy = h * contour_scale;
   float grid_Posy = absroundfract( gy );
   float contour_liner = 0.02 * contour_scale;
   if ( grid_Posy < contour_liner ) {
      terrain_color = mix( terrain_color, contour_color.rgb, ( 1 - grid_Posy / contour_liner ) * contour_color.a );
    }
  }

void drawcontourstripe( inout vec3 terrain_color,
                        float h )
 {
   float gy = h * contour_scale;
   if ( int( floor( mod( gy, 2 ))) == 1 )
      {
        //terrain_color = mix( terrain_color, contour_color.rgb, 0.2 );
        vec3 hsv = rgb2hsv( terrain_color );
        //hsv.y = 1 - ( 1 - hsv.y ) / 2;
        hsv.y = hsv.y * 0.66;
        terrain_color = hsv2rgb( hsv );
        }
  }

void PLUG_main_texture_apply(inout vec4 fragment_color, const in vec3 normal)
{
  float h = terrain_position.y;
  
  /* Flip terrain_position.z to map texture more naturally when viewed from above.
     Consistent with calculating TexCoord for TCastleTerrainData.Height.
     Just flip the sign, because the terrain textures always have repeat = true,
     so no need to shift the texture in any way.
  */

  // normal_slope (normal.y) = 0 means a vertical face = 1 means a horizontal face
  float normal_slope = normalize(terrain_normal).y;

  // default castle terrain mixing calculation
  float height_mix = smoothstep(height_1, height_2, h);
  vec3 flat_color = mix(tex[0].rgb, tex[2].rgb, height_mix);
  vec3 steep_color = mix(tex[1].rgb, tex[3].rgb, height_mix);
  vec3 modified_color = mix(steep_color, flat_color, pow(normal_slope, steep_emphasize));
  vec3 terrain_color = mix(fragment_color.rgb, modified_color, layers_influence);

  // splat map
  if ( splat_sz > 0 )
   {
     vec4 t1;
     vec2 splatpos = vec2( uv.x - 30, uv.y - 30  );
     // calculate 2d index into splatmap
     // position on tile
     splatpos.x = mod( splatpos.x, 60 );
     splatpos.y = mod( splatpos.y, 60 );

     int ax = int( floor( splatpos.x ));
     int ay = int( floor( splatpos.y ));

     if ( blur )
      {
        vec2 posincell = splatpos;
        posincell.x = fract( splatpos.x );
        posincell.y = fract( splatpos.y );
	float shadepct = 0.50;
        int steps = 0;
	float ishadepct = 1/shadepct;

        if ( ( posincell.x > ( 1 - shadepct ))  )
         { if  (( posincell.y > ( 1 - shadepct )) )
            { // ur corner
              vec4 c1 = bilerp( ax, ay, vec2(0.5*( posincell.x - ( 1-shadepct))/shadepct ,
                                             0.5*( posincell.y - ( 1-shadepct))/shadepct ), steps, t1 );
              terrain_color = mix( terrain_color, t1.rgb, t1.a );
              terrain_color = mix( terrain_color, c1.rgb, c1.a );
             }
           else
	   if (( posincell.y < shadepct) && ( ay > 0 ))
	    { // lr corner
              vec4 c1 = bilerp( ax, ay-1, vec2( 0.5* ( posincell.x - ( 1-shadepct))/shadepct,
                                                0.5 + 0.5 * posincell.y/shadepct ), steps, t1 );
              terrain_color = mix( terrain_color, t1.rgb, t1.a );
              terrain_color = mix( terrain_color, c1.rgb, c1.a  );
	     }
            else //r
             { vec4 c1 = lerpx( ax, ay, 0.5 * ( posincell.x - ( 1-shadepct))/shadepct, steps, t1 );
              terrain_color = mix( terrain_color, t1.rgb, t1.a );
              terrain_color = mix( terrain_color, c1.rgb, c1.a  );
             }
         }
        else
        if (( ax > 0 ) && ( posincell.x < shadepct ))
         { if ( ( posincell.y > ( 1 - shadepct )))
	    { // ulcorner
              vec4 c1 = bilerp( ax-1, ay, vec2( 0.5 + 0.5 * posincell.x/shadepct,
                                              0.5* ( posincell.y - ( 1-shadepct))/shadepct ), steps, t1);
              terrain_color = mix( terrain_color, t1.rgb, t1.a );
              terrain_color = mix( terrain_color, c1.rgb, c1.a  );
	     }
           else
           if (( ay > 0 ) && ( posincell.y < shadepct ))
             { // llcorner
              vec4 c1 = bilerp( ax-1, ay-1, vec2( 0.5 + 0.5 * posincell.x/shadepct,
                                                  0.5 + 0.5 * posincell.y/shadepct ), steps, t1);
              terrain_color = mix( terrain_color, t1.rgb, t1.a );
              terrain_color = mix( terrain_color, c1.rgb, c1.a );
             }
            else //l
             { vec4 c1 = lerpx( ax-1, ay, 0.5 + 0.5 * posincell.x/shadepct, steps, t1 );
              terrain_color = mix( terrain_color, t1.rgb, t1.a );
              terrain_color = mix( terrain_color, c1.rgb, c1.a  );
              }
         }
         else
          if  ( ( posincell.y > ( 1 - shadepct )) )
           {
             vec4 c1 = lerpy( ax, ay, 0.5 * ( posincell.y - ( 1-shadepct))/shadepct, steps, t1 );
             terrain_color = mix( terrain_color, t1.rgb, t1.a );
             terrain_color = mix( terrain_color, c1.rgb, c1.a  );
           }
         else
          if (( ay > 0 ) && ( posincell.y < shadepct ))
           {
              vec4 c1 = lerpy( ax, ay-1, 0.5 + 0.5 * posincell.y/shadepct, steps, t1 );
              terrain_color = mix( terrain_color, t1.rgb, t1.a );
              terrain_color = mix( terrain_color, c1.rgb, c1.a );
           }
         else
          { vec4 c = getsplatcolor( ax, ay, t1 );
            terrain_color = mix( terrain_color, t1.rgb, t1.a );
            terrain_color = mix( terrain_color, c.rgb, c.a  );
           }
      }
     else
      { vec4 c = getsplatcolor( ax, ay, t1 );
        terrain_color = mix( terrain_color, t1.rgb, t1.a );
        terrain_color = mix( terrain_color, c.rgb, c.a  );

    //    terrain_color = mix( terrain_color, vec3(1,0,0), 1-splatpos.x / 60 );
       }
    }

	
  // grid
  if ( grid_scale > 0 )
   { drawgrid( terrain_color, uv ); }

  // contour
  if ( contour_scale > 0 )
   { switch ( contour_mode ) {
       case 0 : drawcontourline( terrain_color, h );
                break;
       case 1 : drawcontourstripe( terrain_color, h );
                break;
    }
  }	
  fragment_color.rgb = terrain_color;
}

void PLUG_material_metallic_roughness(inout float metallic_final, inout float roughness_final)
{
  float h = terrain_position.y;
  float normal_slope = normalize(terrain_normal).y;
  float height_mix = smoothstep(height_1, height_2, h);

  float flat_metallic = mix(metallic.x, metallic.z, height_mix);
  float steep_metallic = mix(metallic.y, metallic.w, height_mix);
  float modified_metallic = mix(steep_metallic, flat_metallic, pow(normal_slope, steep_emphasize));
  metallic_final = mix(metallic_final, modified_metallic, layers_influence);

  float flat_roughness = mix(roughness.x, roughness.z, height_mix);
  float steep_roughness = mix(roughness.y, roughness.w, height_mix);
  float modified_roughness = mix(steep_roughness, flat_roughness, pow(normal_slope, steep_emphasize));
  roughness_final = mix(roughness_final, modified_roughness, layers_influence);
}
