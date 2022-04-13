import os

def init(inputName = '../../inputDataFiles/heartTrain.csv', batchSize = 1000):
    newFileName = inputName[0:inputName.rindex('.')]
    parent_dir = inputName[0:inputName.rindex('/')]
    path = os.path.join("", newFileName+"_splitted")
    os.mkdir(path)
    csvfile = open(inputName, 'r').readlines()
    filename = 1
    print(newFileName)
    for i in range(len(csvfile)):
        if i % batchSize == 0:
            open(newFileName+"_splitted/"+ str(filename) + '.csv', 'w+').writelines(csvfile[i:i + batchSize])
            filename += 1

if __name__ == "__main__":
    from sys import argv
    init()

