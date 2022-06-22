# Script to run python script used to update satellite data on cyanobacteria

Sys.setenv(RETICULATE_PYTHON = "C:\\Program Files\\ArcGIS\\Pro\\bin\\Python\\envs\\arcgispro-py3\\python.exe")

library(reticulate)   # Bridging R and Python

# Need to point to the ArcPro Python version - Change as needed
use_python("C:\\Program Files\\ArcGIS\\Pro\\bin\\Python\\envs\\arcgispro-py3\\python.exe",
           required = T)

# Runs python script
py_run_file("Package_test.py")
