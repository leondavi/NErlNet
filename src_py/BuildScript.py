
import sys
import os
import shutil

#-------------- Build Cpp----------------#

def build_cpp():

    SO_RESULT_FILE = "libNErlNet.so"

    print(os.getcwd())
    os.system("scons src=../src_cpp shared=True")

    if os.path.exists("lib/"+SO_RESULT_FILE):
        shutil.move("lib/","../"+"lib/")


def clean():
    shutil.rmtree("../"+"lib/")

#-------------- Build Erlang --------------#




#---------------- Run Tests -----------------#



def main():

    build_cpp_f = False
    clean_f = False
    for arg in sys.argv[1:]:
        if arg == "cpp=True" or arg == "cpp=1":
            build_cpp_f = True
        if arg == "clean" or arg == "clean=True" or arg=="clean=1":
            clean_f = True



    if build_cpp_f:
        build_cpp()

    if clean_f:
        clean()


if __name__ == "__main__":
    main()