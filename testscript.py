#!/usr/bin/env python3
import subprocess
import os
import sys

testDir = sys.argv[1]
testFilePaths = []


#get test file names
def getTestFiles():
    if os.path.isfile(testDir):
        testFilePaths.append(testDir)
    for root, dirs, files in os.walk(testDir, topdown=False):
       for name in files:
          testFilePaths.append(os.path.join(root, name))
       # for name in dirs:
       #    testFilePaths.append(os.path.join(root, name))


# get correct output from one test files
# replace print.s with our output
#test is the path of the test file
def singleTest(path):
    subprocess.call(["./compile",  path])
    testName = path.split('.')[-2].split('/')[-1]
    assemFileName = testName + ".s"
    exitCode = 0
    subprocess.call(["arm-linux-gnueabi-gcc", "-o", testName, "-mcpu=arm1176jzf-s", "-mtune=arm1176jzf-s",  assemFileName])
    try:
        out = subprocess.check_output(["qemu-arm", "-L", "/usr/arm-linux-gnueabi/", testName]).decode()
    except subprocess.CalledProcessError as e:
        exitCode = e.returncode
        out = e.output.decode()


    with open(path) as file:
        lines = file.readlines()
    correctOutput = str()
    correctExitCode = 0
    for i in range(0, len(lines)):
        if "# Output:" in lines[i]:
            j = i + 1
            while lines[j] != "\n" :
                line = lines[j]
                line = line[2:]
                if line != "#empty#\n":
                    correctOutput += line
                j += 1
            i = j
        if "# Exit:" in lines[i]:
            line = lines[i + 1]
            line = line[2:]
            correctExitCode = int(line)


    print("----------Testing : " + testName + ".wacc----------")
    if correctOutput == out:
        print("\N{white heavy check mark} results correct")
    else:
        print("\N{cross mark} results incorrect")
        print("correct result : " + correctOutput )
        print("our result: " + out)

    if correctExitCode == exitCode:
        print("\N{white heavy check mark} exit code correct")
    else:
        print("\N{cross mark} exit code incorrect")
        print("exit code should be: " + str(correctExitCode))
        print("our exit code: " + str(exitCode))
    print("\n")
    #subprocess.call(["rm", "-rf", assemFileName])
    subprocess.call(["rm", "-rf", testName])

def testAll():
    subprocess.call(["make"])
    getTestFiles()
    for path in testFilePaths:
        singleTest(path)

testAll()




#
# out = subprocess.check_output(["qemu-arm", "-L", "/usr/arm-linux-gnueabi/", "print_machinecode"])
