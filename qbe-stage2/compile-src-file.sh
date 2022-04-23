#! /bin/bash

src_file=$1
base_name=`basename $src_file`

echo "Compiling $src_file..."

mkdir -p build
mkdir -p logs

ssa=build/$1.qbe.ssa
ssa_out=logs/$base_name.chibicc.out
ssa_err=logs/$base_name.chibicc.err

# C -> ssa
if ! ../chibicc -qbe -S -o $ssa $src_file > $ssa_out 2> $ssa_err; then
    echo "    chibicc compile failed - see $ssa_out and $ssa_err"
    exit 1
fi

s=build/$1.qbe.s
s_out=logs/$base_name.qbe.out
s_err=logs/$base_name.qbe.err

# ssa -> S
if ! ../../qbe/obj/qbe -o $s $ssa > $s_out 2> $s_err; then
    echo "    QBE optimization failed - see $s_out and $s_err"
    exit 1
fi

