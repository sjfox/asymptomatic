#!/bin/bash
#
# Simple SLURM script for submitting multiple serial
# commands (e.g. parametric studies) using a script wrapper
# to launch the commands.
#
# To use, change this job script to accommodate
# running your serial application(s) in your WORKDIR
# directory (usually the directory of submission).
# Edit the commands file to specify the executions
# for the launcher (paramrun) to perform.
#-------------------------------------------------------
#-------------------------------------------------------
#
#         <------ Setup Parameters ------>
#SBATCH -J covid-ar-fitting          # Job name
#SBATCH -N 28                    # Total number of nodes (56 cores/node)
#SBATCH -n 1568                  # Total number of tasks
#SBATCH -p normal          # Queue name
#SBATCH -o ../slurm_outputs/ar-fitting.o%j      # Name of stdout output file (%j expands to jobid)
#SBATCH -t 03:00:00            # Run time (hh:mm:ss)
#SBATCH --mail-user=spncrfx@gmail.com
#SBATCH --mail-type=all  # email me all messages
#      <------------ Account String ------------>
#------------------------------------------------------
#
#                         # USING SLURM; plugins defines SLURM env. vars.

module load launcher
module load Rstats/3.6.3
module list
pwd
date

export LAUNCHER_RMI=SLURM
export LAUNCHER_PLUGIN_DIR=$LAUNCHER_DIR/plugins
export LAUNCHER_PPN=56

#                         # JOB_FILE is a list of executions to run

export LAUNCHER_JOB_FILE=start-day-ar-parm-sweep.txt
export LAUNCHER_SCHED=interleaved
export LAUNCHER_WORKDIR=/home1/02958/sjf826/seir_regression/launcher

$LAUNCHER_DIR/paramrun    # will run the executions in the LAUNCHER_JOB_FILE file
                          # "JOB" is a misnomer--these are not slurm jobs
                          # Each line in the commands file is an execution.
