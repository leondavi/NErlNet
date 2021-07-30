# NErlNet
<p align="center">
  <img src="Nerlnet_logo.jpg" width="350" title="NerlNet">
</p>

Distributed Machine Learning Platform. 
The communication is based on Erlang VM.
Neural Network implementation is based on cppSANN library. 

Youtube link:https://youtu.be/wSG8nbs1GQY
https://www.youtube.com/watch?v=Y9xT4foxwpY&feature=youtu.be

prerequisite:

1. python 3.7
2. Eigen library ```sudo apt install libeigen3-dev```
3. Installing Rebar3 package https://riptutorial.com/erlang/example/15669/installing-rebar3
4. ```conda install -c anaconda scons```
5. Clone this repository
6. Update cppSANN submodule ```git submodule update --init --recursive```
7. Install the latest version of Erlang: 
   ```wget -O- https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc | sudo apt-key add -```
   ```echo "deb https://packages.erlang-solutions.com/ubuntu focal contrib" | sudo tee /etc/apt/sources.list.d/rabbitmq.list```
   ```sudo apt update```
   ```sudo apt install erlang```

Building Nerlnet:
```./build.sh``` to build the project. 


This project uses C++11 Wrapper for Erlang NIF API (NIFPP: https://github.com/goertzenator/nifpp). 

