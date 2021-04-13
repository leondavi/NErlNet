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
2. ```conda install -c anaconda scons```
3. Clone this repository
4. Update cppSANN submodule ```git submodule update --init --recursive```
5. Call ```./build.sh``` to build the project. 


This project uses C++11 Wrapper for Erlang NIF API (NIFPP: https://github.com/goertzenator/nifpp). 
