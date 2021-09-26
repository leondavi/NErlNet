import sys

resFile = open(sys.argv[1], "r")
inputFile = open(sys.argv[2], "r")
roundNumbersFile = open(sys.argv[1]+"round", "a")

for y in resFile:
    ResLineWithIndex1 = y.split()
    ResLine1 = ResLineWithIndex1[1]
    numbers = ResLine1.split(",")
    numberslen = len(numbers) - 1
    roundNumbersFile.write(ResLineWithIndex1[0] + " ")
    for number, item in enumerate(numbers):
        if number == numberslen:
            roundedNumber = round(float(item))
            roundNumbersFile.write(str(roundedNumber))
        else:
            roundedNumber = round(float(item))
            roundNumbersFile.write(str(roundedNumber) + ",")

    roundNumbersFile.write("\n")

roundNumbersFile.close()
resFile.close()
resRoundFile = open(sys.argv[1]+"round", "r")
Lines = inputFile.readlines()
total=0
correct=0
for x in resRoundFile:
    splitted = x.split()
    Result = splitted[1]
    index = int(splitted[0])-1
    inputRes = Lines[index].rstrip()
    if Result == inputRes:
        #print("correct!")
        total += 1
        correct+=1
    else :
        #print("wrong!")
        total += 1

print("Accuracy:", str(correct) + "/"+str(total), "=", str((correct / total) * 100), "%")
inputFile.close()
resRoundFile.close()
