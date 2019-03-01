#!/usr/bin/env python3
import subprocess
from subprocess import Popen, PIPE, STDOUT
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
        if "echo" in testName:
            p = Popen(["qemu-arm", "-L", "/usr/arm-linux-gnueabi/",testName], stdout=PIPE, stdin=PIPE, stderr=STDOUT)
            if "int" in testName or "Int" in testName:
                grep_stdout = p.communicate(input=b'10')[0]
                out = grep_stdout.decode()
            elif "Char" in testName:
                grep_stdout = p.communicate(input=b'c')[0]
                out = grep_stdout.decode()
        elif "IO" in path:
            p = Popen(["qemu-arm", "-L", "/usr/arm-linux-gnueabi/",testName], stdout=PIPE, stdin=PIPE, stderr=STDOUT)
            grep_stdout = p.communicate(input=b'10\nN')[0]
            out = grep_stdout.decode()
        elif testName in ["fibonacciFullRec", "printInputTriangle", "fibonacciFullIt"]:
            p = Popen(["qemu-arm", "-L", "/usr/arm-linux-gnueabi/",testName], stdout=PIPE, stdin=PIPE, stderr=STDOUT)
            grep_stdout = p.communicate(input=b'10')[0]
            out = grep_stdout.decode()
        elif testName == "readPair":
            p = Popen(["qemu-arm", "-L", "/usr/arm-linux-gnueabi/",testName], stdout=PIPE, stdin=PIPE, stderr=STDOUT)
            grep_stdout = p.communicate(input=b'a\n10')[0]
            out = grep_stdout.decode()
        else:
            subprocess.call(["qemu-arm", "-L", "/usr/arm-linux-gnueabi/",testName])
            out = subprocess.check_output(["qemu-arm", "-L", "/usr/arm-linux-gnueabi/", testName]).decode()
    except subprocess.CalledProcessError as e:
        exitCode = e.returncode
        out = e.output.decode()


    with open(path) as file:
        lines = file.readlines()
    correctOutput = str()
    correctExitCode = 0
    runTimeErr = False
    for i in range(0, len(lines)):
        if "# Output:" in lines[i]:
            j = i + 1
            while lines[j] != "\n" :
                line = lines[j]
                line = line[2:]
                if "runtime" in line:
                    runTimeErr = True
                    break
                elif line != "#empty#\n":
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
    elif (("Error" in out) and runTimeErr):
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
    subprocess.call(["rm", "-rf", assemFileName])
    subprocess.call(["rm", "-rf", testName])

def testAll():
    subprocess.call(["make"])
    getTestFiles()
    for path in testFilePaths:
        if ("advanced/" in path):
            pass
        elif "wacc" in path:
            singleTest(path)

testAll()
