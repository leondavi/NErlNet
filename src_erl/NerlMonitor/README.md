NerlMonitor
=====
NerlMonitor is an external tool that helps NErlNet users get more knowledge on the experiment flow and also gain the ability to terminate workers mid-experiment. This can help in monitoring your model behavior for different kinds of failures. The app is also used to gain various statistics.


# Dependencies
`pip install` these libraries (In addition to the src_py/requirements.txt file):
- NetowrkX
- PySimpleGUI
- PyGraphviz
- Nest-Asyncio

Also, install **Pyrlang** and **Term** libraries for Python-Erlang communication (follow their instructions **carefully**):

Pyrlang - https://github.com/Pyrlang/Pyrlang

Term - https://github.com/Pyrlang/Term

# Run The App
Run `./NerlnetMonitor.sh` script from a different shell (make sure you're using the same Python virtual environment where you installed all dependencies)

# Demo
Youtube Video Demo: https://youtu.be/X5RHLUTqBWk

https://github.com/leondavi/NErlNet/assets/79912473/4e69ad09-3a07-436e-9741-84a64baa4e47

When running the app:
1. Start up screen:

<img width="800" alt="SCR-20230815-lews" src="https://github.com/leondavi/NErlNet/assets/79912473/179a0559-d459-4510-969e-86d838649a9c">

2. Main Server is up:

<img width="800" alt="SCR-20230815-lghc" src="https://github.com/leondavi/NErlNet/assets/79912473/9bed7937-7e25-4011-a50f-5f79db436177">

3. Worker termination:

<img width="800" alt="SCR-20230815-lghc" src="https://github.com/leondavi/NErlNet/assets/79912473/cd7fd002-d392-4ab0-a1e5-84602f5aab38">



