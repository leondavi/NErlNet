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
2. Latest Erlang version:
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
```./build.sh``` to build the project. 


This project uses C++11 Wrapper for Erlang NIF API (NIFPP: https://github.com/goertzenator/nifpp). 

### Nerlnet architecture:
![Nerlnet Architecture](https://user-images.githubusercontent.com/18975070/130082950-0ebcf728-206d-444e-9459-0465b771cd97.jpg)

