
import sys
import os
import shutil

# -------------- Build Cpp----------------#


def build_cpp():

    SO_RESULT_FILE = "libNerlNIF.so"

    print(os.getcwd())
    os.system("scons src=../src_cpp shared=True")

    if os.path.exists("../"+"lib/"):
        shutil.rmtree("../"+"lib/")

    if os.path.exists("lib/"+SO_RESULT_FILE):
        shutil.move("lib/","../"+"lib/")

    os.system("mv ../lib/%s ../src_cpp/cppBridge/" % SO_RESULT_FILE)

def clean():
    shutil.rmtree("../"+"lib/")


# -------------- Build Erlang --------------#
def build_erl():

    os.chdir('../src_erl/erlBridge')
    path = os.path.abspath(os.getcwd())

    for subdir, dirs, files in os.walk(path):
        for file in files:
            # print os.path.join(subdir, file)
            filepath = subdir + os.sep + file

            if filepath.endswith(".erl"):
                print(filepath)
                #os.system("erl -compile %s" % filepath)
                
    os.system("pwd")
    os.chdir('../Communication_Layer/http_Nerlserver')
    os.system("rebar3 shell")

# ---------------- Run Tests -----------------#


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

    build_erl()


if __name__ == "__main__":
    main()
