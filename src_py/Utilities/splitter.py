import os

DEFAULT_FILE_SIZE = 1000

def init(inputName = '../../inputDataFiles/heartTrain.csv', fileSize = DEFAULT_FILE_SIZE):
    newFileName = inputName[0:inputName.rindex('.')]
    parent_dir = inputName[0:inputName.rindex('/')]
    path = os.path.join("", newFileName+"_splitted")
    os.mkdir(path)
    csvfile = open(inputName, 'r').readlines()
    filename = 1
    print(newFileName)
    for i in range(len(csvfile)):
        if i % fileSize == 0:
            open(newFileName+"_splitted/"+ str(filename) + '.csv', 'w+').writelines(csvfile[i:i + fileSize])
            filename += 1

if __name__ == "__main__":
    import sys
    import  getopt
    inputfile = None
    opts, args = getopt.getopt(sys.argv,"hi:o:",["ifile=","ofile="])
    print(opts)
    print(args)
    for arg in args:
      currentArgument = arg.split(" ")
      print(currentArgument)
      if currentArgument[0] == '-h':
         print('splitter.py -i <inputfile> -o <outputDir>')
         sys.exit()
      elif currentArgument[0] == '-i':
         inputfile = currentArgument[-1]
         print("Input file: " + inputfile)
      elif currentArgument[0] == '-o':
         print("not supported yet")
         #outputfile = arg
    init(inputfile)