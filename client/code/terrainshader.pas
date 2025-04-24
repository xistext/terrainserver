unit terrainshader;

interface

uses
   CastleVectors;

{ ground types
     clay    reddish
     sand    yellowish
     dirt    grayish brown
     soil    brown
     gravel  gray
     rocks   gray
     bouldersgray
     asphalt dark gray
     cement  light gray
 }



type ttileshader = class
        name : string;
        fscale : single;
        constructor create( iname : string );
      end;

     tgridshader = class( ttileshader )
        constructor create( iname : string; gridcellcount : integer; scale : single );
      end;

     televationshader = class( ttileshader )
        constructor create( iname : string; iscale : single );
      end;

implementation

constructor ttileshader.create( iname : string );
 begin
   inherited create;
   fscale := 1;
   name := iname;
 end;

constructor tgridshader.create( iname : string; gridcellcount : integer; scale : single );
 begin
   inherited create( iname );
   assert( gridcellcount > 1 );
   fscale := scale;
 end;

constructor televationshader.create( iname : string;
                                     iscale : single );
 begin
   inherited create( iname );
   fscale := iscale;
 end;

end.

