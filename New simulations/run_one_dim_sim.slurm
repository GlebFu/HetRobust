#!/bin/bash
#SBATCH -J 1DimSim           	  	    	# Job name
#SBATCH -o 1DimSim%j.txt      			    # Name of stdout output file (%j expands to jobId)
#SBATCH -e 1DimSim%j.txt      			    # Name of stderr output file(%j expands to jobId)
#SBATCH -p development       	  		    # Submit to the 'normal' or 'development' queue
#SBATCH -N 1                   			    # Total number of nodes
#SBATCH -n 68                 			    # Total number of mpi tasks requested
#SBATCH -t 2:00:00          		      	# Run time (hh:mm:ss)
#SBATCH -A Parallel-demonstrati      		# Allocation name to charge job against
#SBATCH --mail-user=jepusto@gmail.com 	# specify email address for notifications
#SBATCH --mail-type=begin   		      	# email when job begins
#SBATCH --mail-type=end     			      # email when job ends
 
# load R module
module load Rstats           
 
# call R code from RMPISNOW
ibrun RMPISNOW < one-dim-sims.R