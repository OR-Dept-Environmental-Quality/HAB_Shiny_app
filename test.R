library(arcgisbinding)
arc.check_product()
Sys.setenv(RETICULATE_PYTHON = "C:\\Program Files\\ArcGIS\\Pro\\bin\\Python\\envs\\arcgispro-py3\\python.exe")
library(reticulate)
library(dplyr)


# Set up R
# You may need to install the httr package.
# install.packages("httr")
library(httr)
netrc_path <- "C:/users/ygrund/.netrc"
cookie_path <- "C:/users/ygrund/.urs_cookies"
downloaded_file_path <- "//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/Satellite data/CyAN_Data_V5/Sentinel-3/CI_cyano/raw/2024/L2024053.L3m_DAY_CYAN_CI_cyano_CYAN_CONUS_300m_1_1.tif"
# Before using the script
#Set up your ~/.netrc file as listed here: https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget
set_config(config(followlocation=1,netrc=1,netrc_file=netrc_path,cookie=cookie_path,cookiefile=cookie_path,cookiejar=cookie_path))
httr::GET(url = "https://oceandata.sci.gsfc.nasa.gov/getfile/L2024053.L3m_DAY_CYAN_CI_cyano_CYAN_CONUS_300m_1_1.tif",
          write_disk(downloaded_file_path, overwrite = TRUE))



Sys.setenv(RETICULATE_PYTHON = "C:\\Program Files\\ArcGIS\\Pro\\bin\\Python\\envs\\arcgispro-py3\\python.exe")

# Load the reticulate library
library(reticulate)

# Install the requests library
py_install("requests")

py_config()
