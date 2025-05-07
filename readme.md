This is a terrain client/server demo is implemented using the
Castle Game Engine and Indy libaries and builds with Lazarus/FPC though it
should build with Delphi.

Though client/server, it is currently only configured to work on the local
machine.  You firewall may need you to enable the port it uses, but it is only
connecting from localhost, sending commands and receiving terrain data.

This prototype proof-of-concept code and will contain bugs.  You may use it
as you see fit with the understanding that it is incomplete.

You will need to build and run both the client and the server. It will create
tile files in the TerrainServer/data/terrain folder.  These are for persistance
and performance since loading data is faster than generating it.  You can delete
at any time.  There is no check that you don't fill your harddrive at 187200
bytes per tile. You can delete this data with the trashcan button on the server.


erik@edj.net




