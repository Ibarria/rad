#!/usr/bin/env python3

import sys
import os
import platform
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

def execute_test( radexe, golddir, testdir, testname, cflags):
    # Ensure a clean slate
    myremove('compileout.txt')
    myremove('runout.txt')

    nameonly = os.path.splitext(testname)[0]

    # Start by compiling    
    cmd = radexe + " -quiet " + cflags + " " + os.path.join(testdir, testname)
    with open('compileout.txt', 'w') as file:
        subprocess.call(cmd, stdout=file, stderr=subprocess.STDOUT, shell=True)
   
    goldcomp = os.path.join(golddir, nameonly + '.cmptxt');
    
    if os.path.exists( goldcomp ) :
        if not compare_files('compileout.txt', goldcomp):
            print("Failed ", testname, " because of compileout")
            return False
    # run the executable
    runcmd = os.path.splitext(testname)[0]
    if platform.system() == "Windows":
        runcmd = runcmd + ".exe"
    else:
        runcmd = "./" + runcmd
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

compiler_bin = 'rad'
if platform.system() == "Windows":
    compiler_bin = 'rad.exe'

radexe = os.path.join(rundir, compiler_bin)
if not os.path.exists(radexe):
    print("FATAL, could not find compiler:", radexe)
    sys.exit(1)

testdir = 'sanity'
golddir = 'golds'

num_ok_tests = 0
num_bad_tests = 0;
bad_tests = []

for filename in os.listdir(testdir):
    if filename.endswith(".rad"): 
        print("%-30s" % os.path.join(testdir, filename), end = " ")
        normal = execute_test(radexe, golddir, testdir, filename, "")
        if normal: 
            print(Fore.GREEN + " NORMAL" + Style.RESET_ALL, end = " ")
        else:
            print(Fore.RED + " NORMAL" + Style.RESET_ALL, end = " ")

        dbinfo = execute_test(radexe, golddir, testdir, filename, "-g")
        if dbinfo: 
            print(Fore.GREEN + " DBINFO" + Style.RESET_ALL, end = " ")
        else:
            print(Fore.RED + " DBINFO" + Style.RESET_ALL, end = " ")

        opt = execute_test(radexe, golddir, testdir, filename, "-opt")
        if opt: 
            print(Fore.GREEN + " OPT" + Style.RESET_ALL, end = " ")
        else:
            print(Fore.RED + " OPT" + Style.RESET_ALL, end = " ")

        dbopt = execute_test(radexe, golddir, testdir, filename, "-g -opt")
        if dbopt: 
            print(Fore.GREEN + " DBOPT" + Style.RESET_ALL)
        else:
            print(Fore.RED + " DBOPT" + Style.RESET_ALL)

        if normal and dbinfo and opt and dbopt :
            num_ok_tests+=1
        else :
            num_bad_tests+=1
            bad_tests.append(filename)

        continue
    else:
        continue

print("\n", num_ok_tests, "tests have run successfully ", num_bad_tests, " have failed")
if num_bad_tests > 0: print("Failed tests ", bad_tests)
else: print(Fore.GREEN + " ALL TESTS PASSED" + Style.RESET_ALL)

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

