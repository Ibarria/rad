#!/usr/bin/python

import sys
import os
import subprocess
from colorama import init, Fore, Style 
  
def myremove(file):
    if os.path.exists(file):
        os.remove(file)

def compare_files(file1, file2):
    with open(file1, 'r') as file:
        f1str = file.read()

    with open(file2, 'r') as file:
        f2str = file.read()

    if f1str == f2str :
        return True
    else:
        print "Files", file1, file2, "differ!"
        return False

def execute_test( exe, golddir, testdir, testname):
    # Ensure a clean slate
    myremove('compileout.txt')
    myremove('runout.txt')

    # Start by compiling    
    cmd = exe + " -quiet " + os.path.join(testdir, testname) 
    with open('compileout.txt', 'w') as file:
        subprocess.call(cmd, stdout=file, stderr=subprocess.STDOUT, shell=True)
    goldcomp = os.path.join(golddir, os.path.splitext(testname)[0] + '.cmptxt');
    if os.path.exists( goldcomp ) :
        if not compare_files('compileout.txt', goldcomp):
            return False
    # run the executable
    runcmd = os.path.splitext(testname)[0] + ".exe"
    with open('runout.txt', 'w') as file:
        subprocess.call(runcmd, stdout=file, stderr=subprocess.STDOUT, shell=True)
    goldrun = os.path.join(golddir, os.path.splitext(testname)[0] + '.runtxt');
    if not os.path.exists ( goldrun ):
        print "Could not find a gold for", testname
        return False

    if not compare_files('runout.txt', goldrun):
        return False

    # clean up
    myremove('compileout.txt')
    myremove('runout.txt')
    myremove(runcmd)

    return True

init() 

rundir = 'debug'
if len(sys.argv) > 1:
  print "First argument:", sys.argv[1]
  rundir = sys.argv[1]

exe = os.path.join(rundir, 'rad.exe')
testdir = 'sanity'
golddir = 'golds'

for filename in os.listdir(testdir):
    if filename.endswith(".rad"): 
        print os.path.join(testdir, filename),
        if not execute_test(exe, golddir, testdir, filename) :
            print "Failed, stopping..."            
            sys.exit(1)
        else:
            print Fore.GREEN + "OK"
            print Style.RESET_ALL
        continue
    else:
        continue

print "All tests have run successfully"

#with open('pyout.txt', 'w') as file:
#    subprocess.call(exe, stdout=file, stderr=subprocess.STDOUT, shell=True)

# print "The result of running", exe, "is", outstr

# with open('pyout.txt', 'w') as file:
#    file.write(outstr)

#with open('out.txt', 'r') as file:
#    goldstr = file.read()

#with open('pyout.txt', 'r') as file:
#    outstr = file.read()

#print "File read from disk:", goldstr

#if goldstr == outstr :
#    print "Execution and file match"
#else:
#    print "Execution and file mismatch"

