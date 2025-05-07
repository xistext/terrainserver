This is a terrain client/server demo is implemented using the
Castle Game Engine and Indy libraries and builds with Lazarus/FPC though it
should build with Delphi. You will need to download and install the current
versions of these in order to build.
<p>
Server is \TerrainServer\server_standalone.lpi (lazarus) or .dpr (delphi)<br>
Client is \TerrainServer\client\client_standalone.lpi (lazarus) or .dpr (delphi)
<p>
You will need to set the client unit path to
  \TerrainServer\client\code and \TerrainServer\commoncode and to indy and cge.
<p>
You will need to set the server unit path to
  \TerrainServer\code and \TerrainServer\commoncode and to indy and cge.
<p>
You will want to set the client and server output paths to the
<p>
Though client/server, it is currently only configured to work on the local
machine.  Your firewall may need you to enable the port it uses, but it is only
connecting from localhost, sending commands and receiving terrain data.
<p>
This prototype is proof-of-concept code and will contain bugs.  You may use it
as you see fit with the understanding that it is incomplete.  It builds a world
(seedable, but currently seeded at 0) of 120x120 cell tiles with water and flora
depth data.  Each tile has a 60x60 splatmap that allows 4bit r-g-b-a color
combined with a texture and alpha.  This allows you to paint the map using the
brush tool.  You can also deform the map with the dig/pile tools.
<p>
You can toggle grid display, contour stripe display, water layer display and fog
layer display with the buttons on the upper left.
<p>
You will need to build and run both the client and the server. It will create
tile files in the TerrainServer/data/terrain folder.  These are for persistance
and performance since loading data is faster than generating it.  You can delete
at any time.  There is no check that you don't fill your harddrive at 187200
bytes per tile. You can delete this data with the trashcan button on the server.
<p>
You can move around with WASD.  You can mouselook/turn with mouse middle button
Mousewheel moves you up and down.  It is currently possible to move faster than
the tile updates can keep up, especially if the data isn't already generated.
<p>
Known bugs: <br>
The splatmap has a seam between tiles,mixing splat from the opposite side of the
  tile. <br>
If you increase the view radius too fast, it may hit an assertion that the msg
   size is a huge number.  Somehow the stream is getting out of sync when this
   happens. <br>
If you have too many tiles built in the server, water flow and update will be
   very slow.  If this gets too slow, the water will get 'spikey'.  This happens
   for me when it is trying to update 500tiles at around 50tiles/seconds... so
   about 10secs between flow updates. <br>
There are slight gaps in the water mesh between tiles, especially when the
  world is big, since they don't all update at the same time. <br>
Gaps on the edge of freshly generated tiles that are built before their neighbors
  have been generated.  The gaps will go away when the tile updates because you
  moved far to trigger a rebuild. <br>
If you stop flow on the server, or delete all the tiles which also stops flow,
  you may not be able to restart flow without restarting the server application.<br>
Halting the client or server in the middle of a message may cause the ide to
  throw exceptions or trigger actual exception.<br>
<p>
Not implemented yet:<br>
  Water isn't doesn't update the file, so flow restarts if you rerun the server.<br>
  Only the 'sand' texture in the brush palette is wired.  The others don't do
    anything yet even though supported internally.
<P>

erik@edj.net




