#!/bin/bash

#PBS -N ZSEPREPAREFOLDS
#PBS -l mem=40GB

cd ${PBS_O_WORKDIR}
apptainer run image.sif zse_prepare_ml.R
