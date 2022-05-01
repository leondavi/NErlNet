# NErlNet

[![Join the chat at https://gitter.im/NErlNet/community](https://badges.gitter.im/NErlNet/community.svg)](https://gitter.im/NErlNet/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

<p align="center">
  <img src="Nerlnet_logo.jpg" width="350" title="NerlNet">
</p>

Nerlnet is an open-source library for research of distributed machine learning algorithms on IoT devices that gives full insights into both edge devices that run neural network models and network performance and statistics. Nerlnet can simulate distributed ML architectures and deploy them on various IoT devices. <br><br>

Nerlnet is implemented using the following languages and libraries: <br>
• Erlang is used for the communication layer. The implementation is based on Cowboy's http web server. <br>
• C++ OpenNN library implements the neural network edge compute devices.<br>
• Python manages NerlNet and gathers information from it. <br>

![image](https://user-images.githubusercontent.com/18975070/144730156-5bd03ad7-fc5f-45e9-8b4e-62d582af2200.png) 
![image](https://user-images.githubusercontent.com/18975070/144730182-c535b20a-a5f9-4d4f-8632-77d49732f17f.png) 
![image](https://user-images.githubusercontent.com/18975070/144730189-4bad4fba-e559-45a6-b163-d3e5d7d87e1f.png) 
![image](https://user-images.githubusercontent.com/18975070/144730205-5a665819-4be0-40aa-88e5-868ba99aab17.png)
 
A Json script defines a distributed network layout that consists the following instances:  <br>
Edge Compute Device (ECD) which is a worker that runs a neural network model. <br>
Sensor, generates data and send it through the network. <br>
Router that connects ECDs, sensors and other routers. <br>
Communication with Nerlnet is done through a simple python API that can be easily used through Jupyter notebook.  <br> <br>
The API allows the user to collect statistics insights of a distributed machine learning network: <br>
Messages, throughput, loss, predictions, ECD performance monitor.

References and libraries:
- [OpenNN](https://www.opennn.net/), an open-source neural networks library for machine learning. <br>
- [Cowboy](https://github.com/ninenines/cowboy) an HTTP server for Erlang/OTP.<br>
- [NIFPP](https://github.com/goertzenator/nifpp) C++11 Wrapper for Erlang NIF API.<br> 
- [Rebar3](https://github.com/erlang/rebar3), an Erlang tool that makes it easy to create, develop, and release Erlang libraries, applications, and systems in a repeatable manner.

<p align="center"> <img src="https://user-images.githubusercontent.com/18975070/145023471-eb02efa1-01e5-4d7e-9b8d-697252e51568.png" width="120"> </p>
Nerlnet is developed by BGU-DML lab group from the School of Computer and Electrical Engineering in Ben Gurion University. <br>

### Introducing Nerlnet


https://user-images.githubusercontent.com/18975070/145286597-340bbd31-0050-492f-8a27-e9a01eadd905.mp4


### Nerlnet Architecture:
![Nerlnet Architecture](https://user-images.githubusercontent.com/18975070/141692829-f0cdca7d-96d1-43b0-920a-5821a14242f7.jpg)

# Building Nerlnet:
prerequisite:

1. python 3.7 with jupyter lab (only for the api gui server). 
2. Latest Erlang version (OTP-24 minimum) Ubuntu:
   Use apt or build from source (https://www.erlang.org/doc/installation_guide/install) <br>
   ```wget -O- https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc | sudo apt-key add -```<br>
   ```echo "deb https://packages.erlang-solutions.com/ubuntu focal contrib" | sudo tee /etc/apt/sources.list.d/rabbitmq.list```<br>
   ```sudo apt update```<br>
   ```sudo apt install erlang```<br>
   ```sudo apt-get install build-essential```<br>
   Latest Erlang version Debian: <br>
   ```wget https://packages.erlang-solutions.com/erlang/debian/pool/esl-erlang_24.2.1-1~debian~buster_amd64.deb```<br>
   ```apt install ./esl-erlang_24.2.1-1~debian~buster_amd64.deb```<br>
3. Clone this repository with its subomdules ```git clone --recurse-submodules <link to this repo>```
4. Install CMake minimal supported version 3.18: ```sudo apt install cmake```


Build and run Instructions: 

1. Give execution permission for NerlnetBuild.sh
2. Run: ```./NerlnetBuild.sh```

Contact Email: nerlnet@outlook.com 
