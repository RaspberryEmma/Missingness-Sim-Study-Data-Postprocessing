#!/bin/bash


for i in 1 2 3 4 5;
do
	# # convert CSV's into TeX code
	python tably.py "temp/scenario_"$i"_causal_estimation_first_half.csv"  -o "outputs/scenario_"$i"_causal_estimation_first_half_tex.txt" -r
	python tably.py "temp/scenario_"$i"_causal_estimation_second_half.csv" -o "outputs/scenario_"$i"_causal_estimation_second_half_tex.txt" -r
	python tably.py "temp/scenario_"$i"_backdoor_pathways.csv"              -o "outputs/scenario_"$i"_backdoor_pathways_tex.txt"  -r
	# python tably.py "temp/scenario_"$i"_standard_error_first_half.csv"     -o "outputs/scenario_"$i"_standard_error_first_half_tex.txt"  -r
	# python tably.py "temp/scenario_"$i"_standard_error_second_half.csv"    -o "outputs/scenario_"$i"_standard_error_second_half_tex.txt" -r

	# # remove first 4 lines (tabular header)
	sed -i 1,4d "outputs/scenario_"$i"_causal_estimation_first_half_tex.txt"
	sed -i 1,4d "outputs/scenario_"$i"_causal_estimation_second_half_tex.txt"
	sed -i 1,4d "outputs/scenario_"$i"_backdoor_pathways_tex.txt"
	# sed -i 1,4d "outputs/scenario_"$i"_standard_error_first_half_tex.txt"
	# sed -i 1,4d "outputs/scenario_"$i"_standard_error_second_half_tex.txt"

	# # remove last 2 lines (tabular footer)
	sed -i '$d' "outputs/scenario_"$i"_causal_estimation_first_half_tex.txt"
	sed -i '$d' "outputs/scenario_"$i"_causal_estimation_first_half_tex.txt"
	sed -i '$d' "outputs/scenario_"$i"_causal_estimation_second_half_tex.txt"
	sed -i '$d' "outputs/scenario_"$i"_causal_estimation_second_half_tex.txt"
	sed -i '$d' "outputs/scenario_"$i"_backdoor_pathways_tex.txt"
	sed -i '$d' "outputs/scenario_"$i"_backdoor_pathways_tex.txt"
	# sed -i '$d' "outputs/scenario_"$i"_standard_error_first_half_tex.txt"
	# sed -i '$d' "outputs/scenario_"$i"_standard_error_first_half_tex.txt"
	# sed -i '$d' "outputs/scenario_"$i"_standard_error_second_half_tex.txt"
	# sed -i '$d' "outputs/scenario_"$i"_standard_error_second_half_tex.txt"
	
done

