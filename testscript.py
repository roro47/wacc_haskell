#!/usr/bin/env python3
import subprocess
import os
import sys

testDir = sys.argv[1]
testFileNames = []

#get test file names
def getTestFiles():
    if os.path.isfile(testDir):
        testFileNames.append(testDir)
    for root, dirs, files in os.walk(testDir, topdown=False):
       for name in files:
          testFileNames.append(os.path.join(root, name))
       for name in dirs:
          testFileNames.append(os.path.join(root, name))


# get correct output from one test files
# replace print.s with our output
def singleTest(test):
    exitCode = 0
    subprocess.call(["arm-linux-gnueabi-gcc", "-o", "exit_machinecode", "-mcpu=arm1176jzf-s", "-mtune=arm1176jzf-s", "exitsimple.s"])
    try:
        out = subprocess.check_output(["qemu-arm", "-L", "/usr/arm-linux-gnueabi/", "exit_machinecode"]).decode()
    except subprocess.CalledProcessError as e:
        exitCode = e.returncode
        out = e.output.decode()

    with open(test) as file:
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

    print("Testing :" + test)
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
        print("out exit code: " + str(exitCode))

def testAll():
    getTestFiles()
    for test in testFileNames:
        singleTest(test)

testAll()




#
# out = subprocess.check_output(["qemu-arm", "-L", "/usr/arm-linux-gnueabi/", "print_machinecode"])
