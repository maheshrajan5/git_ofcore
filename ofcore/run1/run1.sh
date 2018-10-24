#!/bin/bash
#this is test run of the out of core solver with a small matrix
echo "this is test run of the out of core solver with a small matrix"
output_file=$$.out
../luc >> $output_file 2>&1
#remove the disk files created
rm -rf *.1

