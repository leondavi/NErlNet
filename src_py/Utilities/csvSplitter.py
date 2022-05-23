import os
import shutil

def csvSplitter(csv= None, batchSize = 1000):
    if (csv != None):
        print("Using the CSV entered to the function:\n{}\n".format(csv))
        print(r"Would you like to proceed with this path, or change it? (y\n)") # r stands for repr, ignore \n, and print it regualry.
        choice = input()

        while True:
            if (choice == "y"):
                pass

            elif (choice == "n"):
                print("Please enter the CSV's path:")
                csv = input()
            
            else:
                print("Illegal input.\n")
                print(r"Would you like to proceed with this path, or change it? (y\n)")
                choice = input()
        

    else:
        print("Please enter the CSV's path:")
        csv = input()

    # rindex(str) returns the index of the last occurance of str
    path = csv[0:csv.rindex('/')] # Path of the csv's directory
    name = csv[(csv.rindex('/')+1):csv.rindex('.')] # Name of the csv file, without '.csv'
    newPath = path+'/'+name+'_splitted'
    print(path, name, newPath)

    # If the folder already exists - ask whether to overwrite it:
    if os.path.exists(newPath):
        instructions1 = "A folder containing a splitted CSV with the same name was found.\n"
        instructions2 = r"Would you like to overwrite it [y\n]?" 
        instructions = instructions1 + instructions2

        print(instructions)
        choice = input()

        while True:
            if (choice=='y'):
                shutil.rmtree(newPath)
                os.mkdir(newPath) # Create a new directory for the splitted csv files
                break
            
            elif (choice=='n'):
                print("Aborting...")
                return
            
            else:
                print("Illegal input.")
                print(instructions)
                choice = input()
    else:
        os.mkdir(newPath)

    # Split the csv according to the given batch size:
    csvfile = open(csv, 'r').readlines()
    csvNum = 1
    for row in range(0,len(csvfile),batchSize):
        open(newPath+'/'+name+'_splitted'+str(csvNum)+'.csv', 'w+').writelines(csvfile[row:row+batchSize])
        csvNum += 1

    print("Finished splitting successfully.")

if __name__ == "__main__":
    csvSplitter()

