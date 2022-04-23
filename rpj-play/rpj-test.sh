#! /bin/bash

test=$1

echo "Test-case $test"

mkdir -p build
mkdir -p logs

gcc_exe=build/$1.gcc.exe
gcc_exe_out=logs/$test.gcc.exe.out
gcc_exe_err=logs/$test.gcc.exe.err

# C -> exe (gcc)
if ! gcc -o $gcc_exe $test > $gcc_exe_out 2> $gcc_exe_err; then
    echo "    gcc compile failed - see $gcc_exe_out and $gcc_exe_err"
    exit 1
fi

ssa=build/$1.qbe.ssa
ssa_out=logs/$test.chibicc.out
ssa_err=logs/$test.chibicc.err

# C -> ssa
if ! ../chibicc -qbe -S -o $ssa $test > $ssa_out 2> $ssa_err; then
    echo "    chibicc compile failed - see $ssa_out and $ssa_err"
    exit 1
fi

s=build/$1.qbe.s
s_out=logs/$test.qbe.out
s_err=logs/$test.qbe.err

# ssa -> S
if ! ../../qbe/obj/qbe -o $s $ssa > $s_out 2> $s_err; then
    echo "    QBE optimization failed - see $s_out and $s_err"
    exit 1
fi

exe=build/$1.qbe.exe
exe_out=logs/$test.qbe.exe.out
exe_err=logs/$test.qbe.exe.err

# S -> exe
if ! gcc -no-pie -o $exe $s > $exe_out 2> $exe_err; then
    echo "    Assembly to exe failed - see $exe_out and $exe_err"
    exit 1
fi

# Compare gcc to chibicc/qbe

$gcc_exe > $gcc_exe.run.out 2> $gcc_exe.run.err
gcc_exe_rc=$?

$exe > $exe.run.out 2> $exe.run.err
exe_rc=$?

if ! diff -q $gcc_exe.run.out $exe.run.out; then
   echo "$gcc_exe.run.out and $exe.run.out differ"
   exit 1
fi

if ! diff -q $gcc_exe.run.out $exe.run.out; then
   echo "$gcc_exe.run.out and $exe.run.out differ"
   exit 1
fi



exit 0
