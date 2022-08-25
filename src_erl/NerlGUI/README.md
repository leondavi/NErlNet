nerlGUI
=====

NerlGUI is an external tool for NerlNet that allows to monitor each Entity in the network.
* Make sure that there's a nerlGUI entity in the Arch json
* Make sure to open the Graph screen before other screens to validate connection to network

video example: https://www.youtube.com/watch?v=PJFUqJaL9mI

Build and run
-----

    $ rebar3 compile
    $ rebar3 shell


Design
====
First, a main wx_object (of type gen_server) window is initiated. Then a cowboy server is opened and holds a pointer to this window.
Pressing any of the buttons in the main window will open a subsequent child wx_object which will start probing the relevant NerlNetwork entities for changes in their state.
closing any child window will stop the probe.