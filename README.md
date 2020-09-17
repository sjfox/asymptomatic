# Asymptomatic estimation with pomp
This is a repository that contains the analyses from this paper...


# Step by step code instructions
The code is run on the Frontera supercomputer at TACC. These steps will allow you to run all of the analyses in a similar fashion, though not directly unless you have access to TACC resources.

1. Run the `get-county-hosp-rate.R` code to get the hospitalization rate summarized for the city or location of your choice. Feel free to follow the examples there to estimate for other regions. The median, lowerbound, and upperbound estimates are the placed in the `create-launcher-script-ar_start.R` as the location specific hospitalization rates. If you add a new location, make sure to include it within the for loop in the script.

1. Run the `create-launcher-script-ar_start.R` script. This creates a script with all of the bash commands that are used to carry out the analyses. The result will be a file in the launcher folder that has a row for every call to `fit-ar-pomp.R` that is needed for the analysis.

1. The launcher file will know to call `fit-ar-pomp.R` with all of the different parameters needed to the analysis. However, the script can be called and debugged locally by following the commented lines within to run sections that are needed to setup parameters locally.

1. Once the scripted R commands are created by the creation script, one simply needs to adjust `parm-fitting.slurm` to fit with the necessary high performacnce computing needs. This basically ensures the right number of processes, nodes, etc are allocated

1. The script will save the output in TACC, and I usually download it locally.

1. The script ms-fig-creator does all the processing to compile results and figures for the manuscript

# Contact
Feel free to reach out if you have any questions or issues.
Spencer Fox
spncrfx@gmail.com 