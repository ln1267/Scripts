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
4. Parallel.R
	an example telling how to use parallel packages
5. GIMMS_NDVI_day2month.R
	transfer GIMMS 16days NDVI data to monthly by using provided flag data
	run:parameters are: [1] location of data; [2] start year of input ENVI data; [3] End year of ENVI data; [4] whether use flag data for monthly data process;
	Rscript GIMMS_NDVI_day2month.R ~/data/ 1982 1994 o/1 (whether use flag data)
6. WaSSI_INPUT1.R
	read ENVI data and produce target WaSSI database
	run:parameters are: [1] location of data; [2] file name of input ENVI data; [3] parameters file for ENVI data; [4] Year start for WaSSI; [5] Year End for WaSSI
	Rscript WaSSI_OUT_read.R ~/data/ WaSSI_AU parameters.txt 2000 2014
7. WaSSI_OUT_read.R
	read WaSSI-C output data
	run:parameters are: [1] location of data; [2] file name of input ENVI data; [3] parameters file for ENVI data; [4] Year start for WaSSI; [5] Year End for WaSSI
	Rscript WaSSI_OUT_read.R ~/data/ 
	
	
