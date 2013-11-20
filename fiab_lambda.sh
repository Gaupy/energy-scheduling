#!/bin/sh



for j in 10 100 300; do
	echo "nombre d'arretes sur 100 noeuds = $j" >> results/fiab_lambda.txt
	for p in 1 10 50 70; do
		echo "nombre de procs = $p" >> results/fiab_lambda.txt
		for l in $(seq 0.000001 0.000001 0.00001 | sed 's/,/./'); do 
			echo "lambda_0 = $l" >> results/fiab_lambda.txt
			for r in 1.1 1.5 2.5; do 
				echo "ratio = $r" >> results/fiab_lambda.txt
				for m in `seq 1 10`; do
					./const.sh $l 0.66666 100 $r $p > config_fiab_lambda
					./ggen generate-graph --do-dag erdos_gnm 100 $j > fiab_lambda.dot
					./comp fiab_lambda.dot config_fiab_lambda >> results/fiab_lambda.txt
				done;
			done;
		done;
	done;
done


