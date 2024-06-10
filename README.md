![Version](https://img.shields.io/github/v/release/leondavi/NErlNet)
![Contributors](https://img.shields.io/github/contributors/leondavi/NErlNet)
![Issues](https://img.shields.io/github/issues/leondavi/NErlNet)
[![Discord](https://shields.microej.com/discord/914616114204516393)](https://discord.gg/xwBTbzER)  
[![LinkedIn](https://img.shields.io/badge/Linkedin-%230077B5.svg?logo=linkedin&logoColor=white)](https://www.linkedin.com/company/nerlnet)
[![YouTube](https://img.shields.io/badge/YouTube-%23FF0000.svg?logo=YouTube&logoColor=white)](https://www.youtube.com/channel/UCnnWPPKiHioTBy7Zq5shrQw)


# NErlNet

<p align="center">
  <img src="NerlnetLogo.png" width="200" title="NerlNet">
</p>

Nerlnet is an open-source library for researching distributed machine learning algorithms on IoT devices. It provides comprehensive insights into both edge devices that run neural network models and network performance and statistics. Nerlnet can simulate distributed ML clusters on a single or multiple machines and deploy these clusters, with minor changes, on various kinds of IoT devices.  

Nerlnet simplifies the setup of a distributed cluster that consists of many models on its edge, communication flow can be fully controlled and monitored, and Nerlnet's Python API allows users to manage and gather data from the distributed cluster throughout the experiment.  

Nerlnet library combines the following languages to achieve a stable and efficient distributed ML system framework:  
• The communication layer of Nerlnet is based on an Cowboy - an HTTP web server open-source library.  
• ML on the edge of the distributed cluster is based on OpenNN library, an open-source project of Cpp Neural Network library.  
• Managemnt of Nerlnet cluster - An HTTP server of Flask communicates with Nerlnet's main server to control the cluster's entities.  

![image](https://user-images.githubusercontent.com/18975070/144730156-5bd03ad7-fc5f-45e9-8b4e-62d582af2200.png) 
![image](https://user-images.githubusercontent.com/18975070/144730182-c535b20a-a5f9-4d4f-8632-77d49732f17f.png) 
![image](https://user-images.githubusercontent.com/18975070/144730189-4bad4fba-e559-45a6-b163-d3e5d7d87e1f.png) 
![image](https://user-images.githubusercontent.com/18975070/144730205-5a665819-4be0-40aa-88e5-868ba99aab17.png)
 
### Nerlnet cluster is defined by three configuration files (Json files):
- Distributed Configuration that defines entities of Nerlnet: Source, Router, Client.
  - A client is a host of workers. A worker is a NN model that can move between phases of train and predict.
  - Source generates data streams that are sent to workers.
  - Router controls the data flow through Nerlnet cluster.

### References and libraries:
- [OpenNN](https://www.opennn.net/), an open-source neural networks library for machine learning.   
- [Cowboy](https://github.com/ninenines/cowboy) an HTTP server for Erlang/OTP.  
- [NIFPP](https://github.com/goertzenator/nifpp) C++11 Wrapper for Erlang NIF API.   
- [Rebar3](https://github.com/erlang/rebar3), an Erlang tool that makes it easy to create, develop, and release Erlang libraries, applications, and systems in a repeatable manner.
- [Simple Cpp Logger](https://github.com/nadrino/simple-cpp-logger), simple cpp logger headers-only implementation.

Nerlnet is developed by David Leon, Dr. Yehuda Ben-Shimol, and the community of Nerlnet open-source contributors.  

### Nerlnet Architecture Example:
![Nerlnet Architecture](https://user-images.githubusercontent.com/18975070/141692829-f0cdca7d-96d1-43b0-920a-5821a14242f7.jpg)

# Build and Run Nerlnet:
Recommended cmake version 3.26   
Minimum erlang version otp 25 (Tested 24,25,26)   
Minimum gcc/g++ version 10.3.0   

On every device that is a part of Nerlnet cluster the following steps should be taken:

1. Clone this repository with its subomdules ```git clone --recurse-submodules <link to this repo> NErlNet```  
2. Run ```sudo ./NerlnetInstall.sh```  
  2.1 With argument -i script builds and installs Erlang, latest stable, and CMake.
      (validate that erlang is not installed before executing installation from source)
  2.2 On successful installation, NErlNet directory is accessible  
      via the following path: ```/usr/local/lib/nerlnet-lib```
3. Run ```./NerlnetBuild.sh```
4. Test Nerlnet by running: ```./tests/NerlnetFullFlowTest.sh```
5. [Nerlplanner](https://github.com/leondavi/NErlNet/tree/master/src_py/nerlPlanner) is a Nerlnet tool to generate required jsons files to setup a distributed system of Nerlnet.  
To use NerlPlanner execute ```./NerlPlanner.sh``` (support starts from version 1.3.0).  
Create json files of distributed configurations, connection map and experiment flow as follows:  
- dc_\<any name\>.json  
- conn_\<any name\>.json  
- exp_\<any name\>.json       
6. Run ```./NerlnetRun.sh``` to start Nerlnet.
7. Start Jupyter NB with ```./NerlnetJupyterLaunch.sh``` and follow ApiServerInstance.help() and [examples](https://github.com/leondavi/NErlNet/tree/master/examples).

## Python API and Jupyter-lab (For Api-Server): 
Minimum Python version: 3.8  
  
Communication with Nerlnet is done through a simple python API that can be easily used through Jupyter notebook.       
The API allows the user to collect statistics insights of a distributed machine learning network:   
Number of messages, throughput, loss, predictions, models performance, etc.  

### Instructions
1. Open a jupyter lab environment using ```./NerlnetJupyterLaunch.sh -d <experiment_direcotry>```  
1.1    Use -h to see the help menu of NerlnetJupyterLaunch.sh script.  
1.2    If --no-venv option is selected then required modules can be read from ```src_py/requirements.txt```.  
3. Read the instructions of importing Api-Server within the generated readme.md file inside <experiment_directory> folder. 
4. Follow the example: https://github.com/leondavi/NErlNet/blob/master/examples/example_run.ipynb 

Visit our hugging face organization page for more datasets and models of Nerlnet.  
[<img width="150" alt="hf-logo-with-title" src="https://github.com/leondavi/NErlNet/assets/18975070/93e736b9-732e-4d33-a51a-ca5f68308772">](https://huggingface.co/Nerlnet)
### Social
Distributed ML on the edge - A new evolution step of AI.  

https://github.com/leondavi/NErlNet/assets/18975070/15a3957a-3fd6-4fb2-a365-7e1578468298  
  
[<img width="20" alt="nerlnet-linkedin-page" src="https://github.com/leondavi/NErlNet/assets/18975070/b39d6793-2c31-4cfd-8436-c495267c353a">](https://www.linkedin.com/company/nerlnet)  


## Gratitudes
<h3 align="center">Microsoft Azure</h1>
<p align="center"> <img src="https://github.com/leondavi/NErlNet/assets/18975070/d3255b30-ae3b-46fd-a87f-6c1ec7ae231b" width="50" title="Microsoft Azure Sponsorship"></p>  
<p align="center"> A grant of Azure credits as part of Microsoft’s Azure credits for open source projects program (2024).</p>  
<h3 align="center">Amazon AWS</h1>
<p align="center"> <img src="https://github.com/leondavi/NErlNet/assets/18975070/5fe285fd-43c9-4de8-a619-5ebaace33b29" width="50" title="Amazon AWS Sponsorship"></p>  

<p align="center"> A grant of AWS credits as part of AWSOpen program for open source projects (2024).</p>
