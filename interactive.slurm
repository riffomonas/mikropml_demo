#!/bin/bash


srun --account=pschloss1 --partition=standard --time=02:00:00 --ntasks=1 --cpus-per-task=1 --nodes=1 --mem=6GB --pty /bin/bash


