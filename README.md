# NErlNet

[![Join the chat at https://gitter.im/NErlNet/community](https://badges.gitter.im/NErlNet/community.svg)](https://gitter.im/NErlNet/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

<p align="center">
  <img src="Nerlnet_logo.jpg" width="350" title="NerlNet">
</p>

Nerlnet is an open-source library for research of distributed machine learning algorithms that gives full insights into both edge devices that run neural network models and both network performance and statistics.

Nerlnet is written by three different languages: <br>
•	Erlang implements the network and communication, The communication layer is based on Cowboy http web server and Erlang VM. <br>
•	C++ OpenNN library implements the neural network edge compute devices. <br>
•	Python manages Nerlnet and gathers information from it. <br>

Youtube link:https://youtu.be/wSG8nbs1GQY
https://www.youtube.com/watch?v=Y9xT4foxwpY&feature=youtu.be

prerequisite:

1. python 3.7
2. Latest Erlang version (OTP-24 minimum):
   ```wget -O- https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc | sudo apt-key add -```<br>
   ```echo "deb https://packages.erlang-solutions.com/ubuntu focal contrib" | sudo tee /etc/apt/sources.list.d/rabbitmq.list```<br>
   ```sudo apt update```<br>
   ```sudo apt install erlang```<br>
4. Eigen and boost libraries ```sudo apt install libeigen3-dev libboost-all-dev```
5. Install (or build from source) Rebar3 package https://github.com/erlang/rebar3 (Installation: https://riptutorial.com/erlang/example/15669/installing-rebar3)
6. ```conda install -c anaconda scons``` or ```sudo apt install scons```
7. Clone this repository with its subomdules ```git clone --recurse-submodules <link to this repo>```
8. Update cppSANN submodule ```git submodule update --init --recursive```

# Building Nerlnet:
```./build.sh``` to build the project and run main server. 


This project uses C++11 Wrapper for Erlang NIF API (NIFPP: https://github.com/goertzenator/nifpp). 

### Nerlnet architecture:
![Nerlnet Architecture](https://user-images.githubusercontent.com/18975070/141692829-f0cdca7d-96d1-43b0-920a-5821a14242f7.jpg)

