# Scripts
1. How to run WaSSI_INPUT.R in an interactive way in Zeus
	salloc -p workq --nodes=1 --time=05:00:00
	module load r
	Rscript *.R ~/Work_directory/ inputdata parameter.txt YEAR_START YEAR_END
2. How to run WaSSI_INPUT.R in an interactive way in Magnus
	salloc -p workq --nodes=1 --time=05:00:00
	module load r
	aprun -n 1 R --no-save
3. How to run WaSSI_INPUT.R by SLURM way in Magnus
	need to use submit.sh r-wapper.sh and *.R 
	sbatch submit.sh  # this is for sumitting job
		r-wapper.sh   # define R script and args
		*.R           # Rscript