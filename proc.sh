#!/bin/sh

for j in 10 100 300; do
	echo "nombre d'arretes sur 100 noeuds = $j" >> results/proc.txt
	for r in $(seq 1.2 0.4 2.4 | sed 's/,/./'); do
		echo "ratio deadline = $r" >> results/proc.txt
		for p in `seq 1 300`; do 
			echo "nombre_de_proc = $p" >> results/proc.txt
			for m in `seq 1 10`; do
				./const.sh 0.00001 0.66666 100 $r $p > config_proc
				./ggen generate-graph --do-dag erdos_gnm 100 $j > proc.dot
				./comp proc.dot config_proc >> results/proc.txt
			done;
		done;
	done;
done


