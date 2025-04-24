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
uniform int splatmap[3600];

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

float factor = 1.0/15.0;

vec4 decodecolor( int c, out int texid, out float texalpha )
 { texid = ( c >> 16 ) & 15;
   texalpha = (( c >> 20 ) & 15 ) * factor;
   return vec4( c & 15, ( c >> 4 ) & 15, ( c >> 8 ) & 15, ( c >> 12 ) & 15 ) * factor; 
  }
 
vec4 getsplatcolor( int ax, int ay, out int texid, out float texalpha )
 { int c = splatmap[( ax * splat_sz ) + ay];
   return decodecolor( c, texid, texalpha ); 
  }

vec4 getsplatcolor( int ax, int ay )
 { int texid;
   float texalpha;  
   return decodecolor( splatmap[( ax * splat_sz ) + ay], texid, texalpha ); }

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
  vec2 uv = vec2(terrain_position.x, -terrain_position.z);

  // normal_slope (normal.y) = 0 means a vertical face = 1 means a horizontal face
  float normal_slope = normalize(terrain_normal).y;

  // terrain textures
  vec3 tex[4] = vec3[4]( castle_texture_color_to_linear(texture2D(tex_1, uv * uv_scale.x)).rgb,
                         castle_texture_color_to_linear(texture2D(tex_2, uv * uv_scale.y)).rgb,
  			 castle_texture_color_to_linear(texture2D(tex_3, uv * uv_scale.z)).rgb,
			 castle_texture_color_to_linear(texture2D(tex_4, uv * uv_scale.w)).rgb  );

  // default castle terrain mixing calculation
  float height_mix = smoothstep(height_1, height_2, h);
  vec3 flat_color = mix(tex[0].rgb, tex[2].rgb, height_mix);
  vec3 steep_color = mix(tex[1].rgb, tex[3].rgb, height_mix);
  vec3 modified_color = mix(steep_color, flat_color, pow(normal_slope, steep_emphasize));
  vec3 terrain_color = mix(fragment_color.rgb, modified_color, layers_influence);

  // splat map
  if ( splat_sz > 0 )
   {
     int texid = 0;
     float texalpha = 0;
     float dim = 120/splat_sz;
     float idim = 1/dim;
     vec2 splatpos = vec2( uv.x, uv.y ) * idim;
     // calculate 2d index into splatmap
     int ax = int( floor( mod( splatpos.x, splat_sz )));
     int ay = int( floor( mod( splatpos.y, splat_sz )));
     vec4 c = getsplatcolor( ax, ay, texid, texalpha );
     vec3 splatcolor = c.rgb;
     float alpha = c.a;
     if ( texalpha > 0 )
      { splatcolor = mix( tex[texid], splatcolor, alpha );
        alpha = texalpha;
       }

     if ( blur )
      {
	vec2 posincell = vec2( mod(uv.x, dim), mod( uv.y, dim )) * idim;
	float shadepct = 0.33;
	float ishadepct = 1/shadepct;
	if (( ax < splat_sz - 1 ) && ( posincell.x > ( 1 - shadepct )))
	 {
           vec4 c1 = getsplatcolor( ax + 1, ay, texid, texalpha );
  	   float alpha1 = c1.a * ( posincell.x - ( 1 - shadepct )) * ishadepct;
           splatcolor = mix( splatcolor, tex[texid], texalpha * 0.5 );
           splatcolor = mix( splatcolor, c1.rgb, alpha1 * 0.5 );
	  }
        else
	if (( ax > 0 ) && ( posincell.x < shadepct ))
	 {
            vec4 c1 = getsplatcolor( ax - 1, ay, texid, texalpha );
  	    float alpha1 = c1.a * (shadepct - posincell.x ) * ishadepct;
            splatcolor = mix( splatcolor, tex[texid], texalpha * 0.5 );
            splatcolor = mix( splatcolor, c1.rgb, alpha1 * 0.5 );
	  }
	if (( ay < splat_sz - 1 ) && ( posincell.y > ( 1 - shadepct )))
	 {
           vec4 c1 = getsplatcolor( ax, ay + 1, texid, texalpha );
  	   float alpha1 = c1.a * ( posincell.y - ( 1 - shadepct )) * ishadepct;
           splatcolor = mix( splatcolor, tex[texid], texalpha * 0.5 );
           splatcolor = mix( splatcolor, c1.rgb, alpha1 * 0.5 );
	  }
        else
        if (( ay > 0 ) && ( posincell.y < shadepct ))
	 {
           vec4 c1 = getsplatcolor( ax, ay - 1, texid, texalpha );
  	   float alpha1 = c1.a * (shadepct - posincell.y ) * ishadepct * ( posincell.x - ( 1 - shadepct )) * ishadepct;
           splatcolor = mix( splatcolor, tex[texid], texalpha * 0.5 );
           splatcolor = mix( splatcolor, c1.rgb, alpha1* 0.5 );
	  }

	 if (( ax < splat_sz - 1 ) && ( ay < splat_sz - 1 ) && ( posincell.y > ( 1 - shadepct )) && ( posincell.x > ( 1 - shadepct )))
	  { vec4 c1 = getsplatcolor( ax + 1, ay + 1, texid, texalpha );
  	   float alpha1 = c1.a * (shadepct - posincell.y ) * ishadepct;
           splatcolor = mix( splatcolor, tex[texid], texalpha * 0.5 );
           splatcolor = mix( splatcolor, c1.rgb, alpha1* 0.5 );
	   }
         else
	 if (( ax < splat_sz - 1 ) && ( ay > 0 ) && ( shadepct < posincell.y ) && ( posincell.x > ( 1 - shadepct )))
	  { vec4 c1 = getsplatcolor( ax + 1, ay - 1, texid, texalpha );
  	    float alpha1 = c1.a * ( shadepct - posincell.y ) * ishadepct * ( posincell.x - ( 1 - shadepct )) * ishadepct;
            splatcolor = mix( splatcolor, tex[texid], texalpha * 0.5 );
            splatcolor = mix( splatcolor, c1.rgb, alpha1* 0.5 );
	   }
         else
	 if (( ax > 0 ) && ( ay < splat_sz - 1 ) && ( posincell.y > ( 1 - shadepct )) && ( posincell.x > ( 1 - shadepct )))
	  { vec4 c1 = getsplatcolor( ax - 1, ay + 1, texid, texalpha );
  	    float alpha1 = c1.a * ( posincell.y - ( 1 - shadepct )) * ishadepct * ( posincell.x - ( 1 - shadepct )) * ishadepct;
            splatcolor = mix( splatcolor, tex[texid], texalpha * 0.5 );
            splatcolor = mix( splatcolor, c1.rgb, alpha1* 0.5 );
	   }
	 else	 
         if (( ax > 0 ) && ( ay > 0 ) && ( posincell.y <  shadepct ) && ( posincell.x > ( 1 - shadepct )))
	  { vec4 c1 = getsplatcolor( ax - 1, ax + 1, texid, texalpha );
  	    float alpha1 = c1.a * ( shadepct - posincell.y ) * ishadepct * ( posincell.x - ( 1 - shadepct )) * ishadepct;
            splatcolor = mix( splatcolor, tex[texid], texalpha * 0.5 );
            splatcolor = mix( splatcolor, c1.rgb, alpha1* 0.5 );
	   }

     if ( alpha > 0 ) 
       { terrain_color = mix( terrain_color, splatcolor, alpha ); }
   }}	   
	
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
