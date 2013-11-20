#!/bin/sh

for j in 10 100 300; do
	echo "nombre d'arretes sur 100 noeuds = $j" >> results/fiab_fr2.txt
	for p in 1 10 50 100; do
		echo "nombre de procs = $p" >> results/fiab_fr2.txt
		for r in 1 1.5 3; do
			echo "ratio deadline = $r" >> results/fiab_fr2.txt
			for f in $(seq 0.1 0.1 1 | sed 's/,/./'); do 
				echo "fr = $f" >> results/fiab_fr2.txt
				for m in `seq 1 10`; do
					./const.sh 0.00001 $f 100 $r $p > config_fiab_fr2
					./ggen generate-graph --do-dag erdos_gnm 100 $j > fiab_fr2.dot
					./comp fiab_fr2.dot config_fiab_fr2 >> results/fiab_fr2.txt
				done;
			done;
		done;
	done;
done


