#!/bin/sh


for j in 10 100 300; do
	echo "nombre d'arretes sur 100 noeuds = $j" >> results/ratio.txt
	for p in 1 10 50 70; do
		echo "nombre de procs = $p" >> results/ratio.txt
		for r in `seq 1 10`; do 
			echo "ratio = $r" >> results/ratio.txt
			for m in `seq 1 10`; do
				./const.sh 0.00001 0.66666 100 $r $p > config_ratio
				./ggen generate-graph --do-dag erdos_gnm 100 $j > ratio.dot
				./comp ratio.dot config_ratio >> results/ratio.txt
			done;
		done;
	done;
done


