# CyANconfig.jl

baseurl = "https://oceancolor.gsfc.nasa.gov/CYAN/OLCI/"

# directories
Shiny_dir = "HAB_Shiny_app"
Satellite_dir = "Satellite data"

data_dir = "data"
cyan_dir = "cyan"

# GIS files
zones = "CyAN_Waterbodies.shp"
cyan_gdb = "cyanHAB_deschutes2020.gdb"
themask = "stateline_buffer50albers"
theextent = "stateline_buffer50web"

# tif_file_path need to be updated for the year folder:
#tif_file_path = "\\HAB_Shiny_app\\data\\2023\\*.tif"
#tif_file_dir = "\\HAB_Shiny_app\\data\\2023"
#extract_base = "\\Satellite data\\cyan\\"
#zones = "\\HAB_Shiny_app\\data\\CyAN_Waterbodies.shp"
#dir_Shiny = "\\HAB_Shiny_app\\data"
#themask = "\\Satellite data\\cyanHAB_deschutes2020.gdb\\stateline_buffer50albers"
#theextent = "\\Satellite data\\cyanHAB_deschutes2020.gdb\\stateline_buffer50web"

# When the new data site is used:
# baseurl = "https://oceandata.sci.gsfc.nasa.gov/directaccess/CYAN/L3SMI/"