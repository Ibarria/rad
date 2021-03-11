#!python

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
        print("Files", file1, file2, "differ!")
        return False

def execute_test( radexe, golddir, testdir, testname):
    # Ensure a clean slate
    myremove('compileout.txt')
    myremove('runout.txt')

    nameonly = os.path.splitext(testname)[0]

    # Start by compiling    
    cmd = radexe + " -quiet " + os.path.join(testdir, testname)
    file = open('compileout.txt', 'w')
    with open('compileout.txt', 'w') as file:
        subprocess.call(cmd, stdout=file, stderr=subprocess.STDOUT, shell=True)
    goldcomp = os.path.join(golddir, nameonly + '.cmptxt');
    
    if os.path.exists( goldcomp ) :
        if not compare_files('compileout.txt', goldcomp):
            return False
    # run the executable
    runcmd = os.path.splitext(testname)[0] + ".exe"
    with open('runout.txt', 'w') as file:
        subprocess.call(runcmd, stdout=file, stderr=subprocess.STDOUT, shell=True)
    goldrun = os.path.join(golddir, nameonly + '.runtxt');
    if not os.path.exists ( goldrun ):
        print("Could not find a gold for", testname)
        return False

    if not compare_files('runout.txt', goldrun):
        return False

    # clean up
    myremove('compileout.txt')
    myremove('runout.txt')
    myremove(os.path.join(testdir,nameonly + ".o"))
    myremove(runcmd)
    myremove(nameonly + ".pdb")

    return True

init() 

rundir = 'debug'
if len(sys.argv) > 1:
  print("Running in", sys.argv[1], "mode")
  rundir = sys.argv[1]

radexe = os.path.join(rundir, 'rad')
if not os.path.exists(radexe):
    print("FATAL, could not find compiler:", radexe)
    sys.exit(1)

testdir = 'sanity'
golddir = 'golds'

num_tests = 0

for filename in os.listdir(testdir):
    if filename.endswith(".rad"): 
        print(os.path.join(testdir, filename)),
        if not execute_test(radexe, golddir, testdir, filename) :
            print("Failed, stopping...")
            sys.exit(1)
        else:
            print(Fore.GREEN + "OK" + Style.RESET_ALL)
            num_tests+=1
        continue
    else:
        continue

print("\nAll ", num_tests, "tests have run successfully")

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

