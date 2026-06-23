#!/bin/sh
set -eux

input_file="$1"
input_name=$(basename "$input_file")

mkdir -p "$TMP"
output_name="$TMP/$input_name.ll"
if [ "$#" -eq 2 ]; then
  output_name="$2"
fi


timeout 2 "$BRIO" -o "$output_name" "$input_file"
timeout 2 lli "$output_name"
