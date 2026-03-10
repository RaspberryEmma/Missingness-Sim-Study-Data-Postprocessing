#!/bin/bash

# convert CSV's into TeX code
python tably.py "temp/MAR_results_table.csv"  -o "outputs/MAR_results_table_tex.txt"  -r
python tably.py "temp/MCAR_results_table.csv" -o "outputs/MCAR_results_table_tex.txt" -r
python tably.py "temp/MNAR_results_table.csv" -o "outputs/MNAR_results_table_tex.txt" -r

# remove first 4 lines (tably header)
sed -i 1,4d "outputs/MAR_results_table_tex.txt"
sed -i 1,4d "outputs/MCAR_results_table_tex.txt"
sed -i 1,4d "outputs/MNAR_results_table_tex.txt"

# # remove last 2 lines (tably footer)
sed -i '$d' "outputs/MAR_results_table_tex.txt"
sed -i '$d' "outputs/MCAR_results_table_tex.txt"
sed -i '$d' "outputs/MNAR_results_table_tex.txt"

sed -i '$d' "outputs/MAR_results_table_tex.txt"
sed -i '$d' "outputs/MCAR_results_table_tex.txt"
sed -i '$d' "outputs/MNAR_results_table_tex.txt"


