

import os

os.chdir("src_cpp")
os.system("scons")

print(os.getcwd())