# NErlNet
![version](https://img.shields.io/github/v/release/leondavi/NErlNet)


<p align="center">
  <img src="NerlnetLogo.png" width="200" title="NerlNet">
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
- [Simple Cpp Logger](https://github.com/nadrino/simple-cpp-logger), simple cpp logger headers-only implementation.

<p align="center"> <img src="https://user-images.githubusercontent.com/18975070/145023471-eb02efa1-01e5-4d7e-9b8d-697252e51568.png" width="120"> </p>
Nerlnet is developed by BGU-DML lab group from the School of Computer and Electrical Engineering in Ben Gurion University. <br>

### Introducing Nerlnet


https://user-images.githubusercontent.com/18975070/145286597-340bbd31-0050-492f-8a27-e9a01eadd905.mp4


### Nerlnet Architecture:
![Nerlnet Architecture](https://user-images.githubusercontent.com/18975070/141692829-f0cdca7d-96d1-43b0-920a-5821a14242f7.jpg)

# Build and Run Nerlnet:
Recommended cmake version 3.26 <br>
Minimum erlang version otp 24 <br>
Minimum gcc/g++ version 8.4 <br>

On each machine that runs Nerlnet go through the following steps:

1. Clone this repository with its subomdules ```git clone --recurse-submodules <link to this repo> NErlNet```  
2. Run ```sudo ./NerlnetInstall.sh```  
  2.1 With argument -i script builds and installs Erlang, latest stable, and CMake.  
  2.2 After running NerlnetInstall.sh, NErlNet directory can be accessed  
    via the following path: ```/usr/local/lib/nerlnet-lib```  
3. Create json files of architecture, connection map and experiment flow.  
json configuration files names must follow the convention of prefixes:  
arch_<any>.json, conn_<any>.json, exp_<any>.json.  

Optional: Run ```./NerlnetGetData.sh``` to create or get default inputDataDir  
4. Run ```./NerlnetRun.sh``` to start Nerlnet  


## Python API and Jupyter (For UI Server): 
Minimum Python version: 3.8

1. Create and activate a virtual environment for Nerlnet: (https://docs.python.org/3/library/venv.html)  
  ```python -m venv VENV_PATH/VENV_NAME ```  
  ```source VENV/bin/activate``` 
2. Install required modules while in venv ```pip install -r src_py/requirements.txt```
3. Call Jupyter environment creator script with an experiment directory ```./NerlnetJupyterEnvGenerator.sh --j <experiment_direcotry>```
4. Run Jupyter notebook with ```jupyter-notebook``` and create a new notebook in the created dir from 4. 
5. Follow the example: https://github.com/leondavi/NErlNet/blob/master/examples/example_run.ipynb 

Contact Email: nerlnet@outlook.com
