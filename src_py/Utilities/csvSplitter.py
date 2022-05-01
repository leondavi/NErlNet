import os
import shutil

def csvSplitter(csv = '/home/dor/workspace/NErlNet/src_py/Data/heart2020Edit.csv', batchSize = 1000):
    # rindex(str) returns the index of the last occurance of str
    path = csv[0:csv.rindex('/')] # Path of the csv's directory
    name = csv[(csv.rindex('/')+1):csv.rindex('.')] # Name of the csv file, without '.csv'
    newPath = path+'/'+name+'_splitted'

    # If the folder already exists - overwrite it:
    if os.path.exists(newPath):
         shutil.rmtree(newPath)
    os.mkdir(newPath) # Create a new directory for the splitted csv files

    # Split the csv according to the given batch size:
    csvfile = open(csv, 'r').readlines()
    csvNum = 1
    for row in range(0,len(csvfile),batchSize):
        open(newPath+'/'+name+'_splitted'+str(csvNum)+'.csv', 'w+').writelines(csvfile[row:row+batchSize])
        csvNum += 1

if __name__ == "__main__":
    csvSplitter()

