#!/usr/bin/env bash

dir="$HOME/Downloads"

echo "${dir}/$(ls ${dir} -t | head -n1)"
