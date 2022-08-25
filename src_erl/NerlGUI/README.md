nerlGUI
=====

NerlGUI is an external tool for NerlNet that allows to monitor each Entity in the network.

video example: https://www.youtube.com/watch?v=PJFUqJaL9mI

Build and run
-----

JSONPATH file should be:

    $ ../../../inputJsonFiles/Architecture/jsonArch3PC2Workers1SourceMonitored.json
    $ ../../../inputJsonFiles/ConnectionMap/3Router2ClientsH.json
    $ 
    $ # For Python API:
    $ /home/nerlnet/workspace/NErlNet/inputJsonFiles/Architecture/jsonArch3PC2Workers1SourceMonitored.json
    $ /home/nerlnet/workspace/NErlNet/inputJsonFiles/experimentsFlow/jsonExampleExpFlow2Worke3Router1Source.json

* Make sure to change "nerlnet" to ubuntu username
* Make sure files exist in their dirs
* Make sure to open the Graph screen before other screens to validate connection to network

Then run in the NerlGUI dir:

    $ rebar3 compile
    $ rebar3 shell


Design
====

![NERL GUI](https://user-images.githubusercontent.com/1270091/186702022-98313349-fdeb-472c-b30e-ef9874a3705d.png)

First, a main wx_object (of type gen_server) window is initiated. Then a cowboy server is initiated and holds a pointer to this window.

Pressing any of the buttons in the main window will open a subsequent child wx_object which will start probing the relevant NerlNetwork entities for changes in their states. These changes are logged in relevant labels and are displayed to the user in each window.

closing any child window will stop the probe.
