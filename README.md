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
3. Rebar3 package ```sudo apt-get install -y rebar```
4. ```conda install -c anaconda scons```
5. Clone this repository
6. Update cppSANN submodule ```git submodule update --init --recursive```
7. Call ```./build.sh``` to build the project. 


This project uses C++11 Wrapper for Erlang NIF API (NIFPP: https://github.com/goertzenator/nifpp). 

