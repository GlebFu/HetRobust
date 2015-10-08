#!/bin/bash
#SBATCH -J LESim          # Job name
#SBATCH -o LESim.o%j      # Name of stdout output file (%j expands to jobId)
#SBATCH -e LESim.o%j      # Name of stderr output file(%j expands to jobId)
#SBATCH -n 80               # Total number of mpi tasks requested
#SBATCH -p normal           # Submit to the 'normal' or 'development' queue
#SBATCH -t 30:00:00          # Run time (hh:mm:ss)
#SBATCH -A Parallel-demonstrati    # Allocation name to charge job against
#SBATCH --mail-user=gleb.furman@gmail.com # specify email address for notifications
#SBATCH --mail-type=begin   # email when job begins
#SBATCH --mail-type=end     # email when job ends
 
# load R module
module load Rstats           
 
# call R code from RMPISNOW
ibrun RMPISNOW < runLErepTACC.R