#!/bin/sh

for j in 10 100 300; do
	echo "nombre d'arretes sur 100 noeuds = $j" >> results/fiab_fr.txt
	for p in 1 10 50 100; do
		echo "nombre de procs = $p" >> results/fiab_fr.txt
		for f in $(seq 0.1 0.1 1 | sed 's/,/./'); do 
			echo "fr = $f" >> results/fiab_fr.txt
			r=$(echo "scale=10; 1+0.1*(1-$f)/$f" | bc)
				echo "ratio deadline : (1 +. 0.1*.(fmax-.fr)/.fr)" >> results/fiab_fr.txt
				for m in `seq 1 10`; do
					./const.sh 0.00001 $f 100 $r $p > config_fiab_fr
					./ggen generate-graph --do-dag erdos_gnm 100 $j > fiab_fr.dot
					./comp fiab_fr.dot config_fiab_fr >> results/fiab_fr.txt
				done;
			r=$(echo "scale=10; 1+(1-$f)/$f" | bc)
				echo "ratio deadline : (1 +. (fmax-.fr)/.fr)" >> results/fiab_fr.txt
				for m in `seq 1 10`; do
					./const.sh 0.00001 $f 100 $r $p > config_fiab_fr
					./ggen generate-graph --do-dag erdos_gnm 100 $j > fiab_fr.dot
					./comp fiab_fr.dot config_fiab_fr >> results/fiab_fr.txt
				done;
 			r=$(echo "scale=10; 1+3*(1-$f)/$f" | bc)
				echo "ratio deadline : (1 +. 3.*.(fmax-.fr)/.fr)" >> results/fiab_fr.txt
				for m in `seq 1 10`; do
					./const.sh 0.00001 $f 100 $r $p > config_fiab_fr
					./ggen generate-graph --do-dag erdos_gnm 100 $j > fiab_fr.dot
					./comp fiab_fr.dot config_fiab_fr >> results/fiab_fr.txt
				done;
		done;
	done;
done


