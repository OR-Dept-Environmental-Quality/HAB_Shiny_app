#= 
Import of CyAN data from NASA
Scripts to download and process daily CyAN data (EPA) from NASA

Originally developed in Python in 10/2020
Modified in Julia 1/2023 

Script functions

This script downloads NASA OLCI imagery from NASA's website, calculates cyanobacteria abundance according to
the US EPA CyAN project protocol, and calculates summary statistics for the 42 resolvable lakes/reservoirs in
Oregon. See specific code chunks for the explanations of the code functions.
This code is specific to Sentinel 3 data from 2016 to the present.
=#

# Install needed packages (skip if already installed)
using Pkg
#Pkg.add("HTTP")
#Pkg.add("Dates")
#Pkg.add("Tar")
#Pkg.add("Downloads")
#Pkg.add("GDAL")
#Pkg.add("FileIO")
#Pkg.add("GeoStats")
#Pkg.add("ArchGDAL")
#Pkg.add("PyPlot")
#Pkg.add("Images")
#Pkg.add("DelimitedFiles")

# Use packages
using HTTP
using Dates
using Tar
using Downloads
#using GDAL
using FileIO
#using GeoStats
using ArchGDAL
using PyPlot
using Images
using DelimitedFiles

# Set working directory
include("CyANwd.jl")
cd(working_dir)

# Import file paths
include("CyANconfig.jl")

# Make master directories as needed
isdir(Shiny_dir) || mkdir(Shiny_dir)
isdir(Satellite_dir) || mkdir(Satellite_dir)

# Make subfolders as needed
isdir(joinpath(Shiny_dir, data_dir, string(year(DateTime(now()))))) || mkdir(joinpath(Shiny_dir, data_dir, string(year(DateTime(now())))))
isdir(joinpath(Satellite_dir, cyan_dir, string(year(DateTime(now()))))) || mkdir(joinpath(Satellite_dir, cyan_dir, string(year(DateTime(now())))))
isdir(joinpath(Satellite_dir, cyan_dir, string(year(DateTime(now()))), "_tif")) || mkdir(joinpath(Satellite_dir, cyan_dir, string(year(DateTime(now()))), "_tif"))
isdir(joinpath(Satellite_dir, cyan_dir, string(year(DateTime(now()))), "_mosiac")) || mkdir(joinpath(Satellite_dir, cyan_dir, string(year(DateTime(now()))), "_mosiac"))

# Define directory paths:
tif_file_dir = joinpath(Shiny_dir, data_dir, string(year(DateTime(now()))))
extract_base = joinpath(Satellite_dir, cyan_dir, string(year(DateTime(now()))))

# Print system version
println("You are using Julia version: $VERSION")

# Test the connection to the website
try
    response = HTTP.get(baseurl)
    #content = String(response.body)
    print("Valid url: $baseurl")
catch e
    print("Invalid url")
end

#= Gets the appropriate date range for the current query
Note that the query can only be done for the current year =#

current_year = year(DateTime(now()))
print("The year is: $current_year")

# Get the start date and end dates for data query
function latest_file(dir)
    files = readdir(dir)
    latest_mod_time = 0
    latest_file = ""

    for file in files
        path = joinpath(dir, file)
        mod_time = stat(path).mtime
        if mod_time > latest_mod_time
            latest_mod_time = mod_time
            latest_file = file
        end
    end

    return latest_file
end

# Get sequence of days to download
hab_start_day = parse(Int64, latest_file(tif_file_dir)[5:7]) + 1
hab_end_day = Dates.dayofyear(Dates.now()) - 1

println("Data from days of year $hab_start_day to $hab_end_day will be downloaded")

# Making a vector of days to download based on three digits
hab_days = [string(x) for x in (collect(hab_start_day:hab_end_day))]
hab_days = [s[1:2] == "0" * s ? s : "0" * s for s in hab_days]
println("day sequence: $hab_days")
println("sequence length: ", length(hab_days))

#= Set archive and filename variables for date range
file = .tgz archive
=#

file = ["L" * string(current_year) * h * ".L3m_DAY_CYAN_CI_cyano_CYAN_CONUS_300m.tgz" for h in hab_days]
file_name = ["L" * string(current_year) * h * ".L3m_DAY_CYAN_CI_cyano_CYAN_CONUS_300m.tgz" for h in hab_days]
file2 = ["L" * string(current_year) * h * ".L3m_DAY_CYAN_CI_cyano_CYAN_CONUS_300m" for h in hab_days]
url = [baseurl * string(current_year) * "/" * h * "/" * f for (h, f) in zip(hab_days, file)]

# Download, extract, and rename imagery for a date range in Oregon (1_1, 1_2, 2_1, 2_2)

# First need to create a vector of tif file 

for i in 1:length(hab_days)
    println(url[i])

    tmp_file_nm = joinpath(extract_base, file[i])
    Downloads.download(url[i], tmp_file_nm)
    run(`tar xzf $tmp_file_nm -C $extract_base`)
    rm(tmp_file_nm)
              
end

# Set up Oregon files
v1 = ["_1", "_2"]
v2 = ["_1.tif", "_2.tif"]

tif_Oregon = [v1[i] * v2[j] for i in 1:length(v1), j in 1:length(v2)]
tif_Oregon = reshape(tif_Oregon, length(tif_Oregon))

# Get file folder names
folder = filter(x -> !(x == "_tif" || x == "_mosiac"), readdir(extract_base))
Oregon_tifs = [folder[f] * tif_Oregon[o] for f in 1:length(folder), o in 1:length(tif_Oregon)]
Oregon_tifs = reshape(Oregon_tifs, length(Oregon_tifs))

# List the contents of the folder and delete non Oregon ones
for i in folder
    contents = readdir(joinpath(extract_base, i))
    # Delete the files that don't match the names in the vector
    [rm(joinpath(joinpath(extract_base, i), j)) for j in contents if !(j in Oregon_tifs)]
end

# Rename Oregon tifs

for i in folder
    workspace = joinpath(extract_base, i)
    rasters = readdir(workspace)

    for raster in rasters
        fileName, tile, fileExtension = split(raster, '.')
        compactFileName = fileName[2:8] * tile[end-3:end] * "." * fileExtension
        println(compactFileName)
        mv(joinpath(workspace, raster), joinpath(workspace, compactFileName), force=true)
    end

end

# read in files each day, combine files, and calculate cell counts
for i in folder
    tmp_file_nm = readdir(joinpath(extract_base, i))
    isdir(joinpath(extract_base, i, "csv")) || mkdir(joinpath(extract_base, i, "csv"))
    for j in tmp_file_nm
        tmp_data = ArchGDAL.read(joinpath(extract_base, i, j))
        tmp_data1 = float(ArchGDAL.getband(tmp_data, 1))
        tmp_data1[tmp_data1 .== 254] .= NaN
        tmp_data1[tmp_data1 .== 255] .= NaN
        out_cells = (10.0 .^ ((3.0 / 250.0) * tmp_data1 .- 4.2)) * 100000000 # Equation to calculate cyanobacteria cells/mL
        writedlm(joinpath(extract_base, i, "csv", j[1:11] * ".csv"), out_cells, ',')
    end
    # Need to get info for writing tif files later
    dvr = ArchGDAL.getdriver(tmp_data)
    wt = ArchGDAL.width(tmp_data)
    ht = ArchGDAL.height(tmp_data)
    ArchGDAL.getgeotransform(tmp_data)
end    

# Create joined matrix files
for i in folder
    #i = "L2023031.L3m_DAY_CYAN_CI_cyano_CYAN_CONUS_300m"
    workfolder = joinpath(extract_base, i, "csv")
    #j = "2023031"
    tmp_file_1_1 = readdlm(joinpath(workfolder, i[2:8] * "_1_1.csv"), ',', Float64)
    tmp_file_1_2 = readdlm(joinpath(workfolder, i[2:8] * "_1_2.csv"), ',', Float64)
    tmp_file_2_1 = readdlm(joinpath(workfolder, i[2:8] * "_2_1.csv"), ',', Float64)
    tmp_file_2_2 = readdlm(joinpath(workfolder, i[2:8] * "_2_2.csv"), ',', Float64)

    tmp_file_1 = vcat(tmp_file_1_1, tmp_file_1_2)
    tmp_file_2 = vcat(tmp_file_2_1, tmp_file_2_2)
    tmp_file = hcat(tmp_file_1, tmp_file_2)
    writedlm(joinpath(extract_base, i, "csv", i[2:8] * ".csv"), tmp_file, ',')
end

# Create mosiac tif files


# Convert to cells/ml , mosaic 4 tiles into one oregon image, calc zonal stats for resolvable lakes
# Setnull and cellsml

# Start here
readdir(joinpath(extract_base, folder[1]))
tmp_data = ArchGDAL.read(joinpath(extract_base, folder[1], "2023031_1_1.tif"))

ArchGDAL.getdriver(tmp_data)
ArchGDAL.nraster(tmp_data)
tmp_data1 = ArchGDAL.getband(tmp_data, 1)
typeof(tmp_data1)

tmp_data2 = ArchGDAL.imread(joinpath(extract_base, folder[1], "2023031_1_2.tif"))

tmp_data3 = float(tmp_data1)



tmp_data3[tmp_data3 .== 255] .= NaN

typeof(tmp_data3)

ArchGDAL.getgeotransform(tmp_data)

crs = ArchGDAL.importEPSG(5070)

crs = ArchGDAL.toWKT(ArchGDAL.importEPSG(5070))

ArchGDAL.create(joinpath(extract_base, folder[1], "cellsml", "2023031_1_1.tif"),
                driver = ArchGDAL.getdriver(tmp_data),
                width = ArchGDAL.width(tmp_data),
                height = ArchGDAL.height(tmp_data),
                nbands = 1,
                dtype = Float64
                ) do dataset
                        ArchGDAL.write!(dataset, tmp_data3, 1)
                        ArchGDAL.setgeotransform!(dataset, ArchGDAL.getgeotransform(tmp_data))
                        ArchGDAL.setproj!(dataset, crs)
                end


tmp_data5 = ArchGDAL.read(joinpath(extract_base, folder[1], "cellsml", "2023031_1_1.tif"))
typeof(tmp_data5)

tmp_data6 = ArchGDAL.getband(tmp_data5, 1)

for i in 1:length(folder)

    # rename oregon images
    workspace = joinpath(extract_base, folder[i])
    
    workfiles = readdir(workspace)
    
    mkdir(joinpath(workspace, "cellsml"))

    cellsml_dir = joinpath(workspace, "cellsml")
    

    for j in workfiles
        # set non cyano values to null
        tmp_data = ArchGDAL.readraster(joinpath(workspace, j))
        data = float(tmp_data)
        data[data .== 255] .= NaN
        data[data .== 254] .= NaN

        # convert CyAN index numbers to cells/ml
        outCellsML = (10.0 .^ ((3.0 / 250.0) * data .- 4.2)) * 100000000

        # save the cells/ml to subdirectory
        final_DIR = joinpath(cellsml_dir, j)
        
    end
end

# Old Python code

# Convert to cells/ml , mosaic 4 tiles into one oregon image, calc zonal stats for resolvable lakes
# Setnull and cellsml
print("done")

for i in range(0, hab_days_length):

    # rename oregon images
    env.workspace = temp_dir[i]
    arcpy.CheckOutExtension("Spatial")

    os.mkdir(os.path.join(temp_dir[i], 'cellsml'))
    cellsml_dir = os.path.join(temp_dir[i], 'cellsml')
    output_DIR = os.path.join('c:', 'hab', 'cyan', year, 'cellsml')
    for raster in arcpy.ListRasters():
        # set non cyano values to null
        outSetNull = SetNull(raster, raster, 'VALUE = 255 or VALUE = 254')
        # 254 = land; 255 = water
        # convert CyAN index numbers to cells/ml
        outCellsML = (Power(10, (3.0 / 250.0 * Int(outSetNull) - 4.2))) * 100000000

        # save the cells/ml to subdirectory
        final_DIR = os.path.join(cellsml_dir, raster)
        outCellsML.save(final_DIR)

        del outSetNull
        del outCellsML

# Make mosaic directory
# os.mkdir(os.path.join(extract_path, 'mosaic')) # Needed for start of new year
mosaicdir = os.path.join(extract_path, 'mosaic')

# Mosaic
for i in range(0, hab_days_length):
    cellsml_dir = os.path.join(temp_dir[i], 'cellsml')
    env.workspace = cellsml_dir

    sr = arcpy.SpatialReference()
    sr.factoryCode = 5070
    sr.create()

    mosaicdict = {}

    for raster in arcpy.ListRasters():
        fileName, fileExtension = os.path.splitext(raster)
        mosaickey = fileName[1:8]
        mosaicfilename = fileName[1:8] + ".tif"
        if mosaickey not in mosaicdict:
            mosaicdict[mosaickey] = []

        if len(mosaicdict[mosaickey]) == 0:
            mosaicdict[mosaickey].append(raster)
        else:
            mosaicdict[mosaickey].append(raster)
            if len(mosaicdict[mosaickey]) == 4:
                print(mosaicfilename)
                arcpy.MosaicToNewRaster_management(mosaicdict[mosaickey], mosaicdir, mosaicfilename, sr,
                                                   "32_BIT_FLOAT", "300", "1", "LAST", "FIRST")

# Need to get a list of file names for zonal statistics
# Need to modify to account for existing files in the directory

mosaicfilename2 = list()
for i, file in enumerate(os.listdir(mosaicdir)):
    if file.endswith(".tif"):
        mosaicfilename2.append(os.path.basename(file))

mosaicfilename2 = mosaicfilename2[(hab_day_start - 1):(hab_day_end + 1)]

# Need to get a list of names for mosaic key
# Need to modify to account for existing files in the directory

mosaickey2 = list()
for i, file in enumerate(os.listdir(mosaicdir)):
    if file.endswith(".tif"):
        mosaickey2.append(os.path.splitext(file)[0])

mosaickey2 = mosaickey2[(hab_day_start - 1):(hab_day_end + 1)]

# Zonal stats
for i in range(0, hab_days_length):
    zonalraster = os.path.join(mosaicdir, mosaicfilename2[i])
    # zones = r"\\deqhq1\wq-share\Harmful Algal Blooms Coordination Team\GIS\cyan\HAB_deschutes2020.gdb\NHDWaterbody_resolvable_lakes"
    stats_dir = os.path.join(extract_path, 'mosaic', 'stats')

    thestatsname = os.path.join(os.path.join(stats_dir, mosaickey2[i] + "_stats.dbf"))
    print(thestatsname)

    # Process: Zonal Statistics as Table
    env.workspace = mosaicdir
    arcpy.gp.ZonalStatisticsAsTable_sa(zones, "GNISIDNAME", zonalraster, thestatsname, "DATA", "ALL")

    arcpy.DeleteField_management(thestatsname, ["ZONE_CODE", "SUM", "MEDIAN", "PCT90"])

    # calcdaydate
    arcpy.AddField_management(thestatsname, 'Day', 'LONG')
    arcpy.AddField_management(thestatsname, 'Year', 'LONG')
    arcpy.AddField_management(thestatsname, 'Date', 'Date')

    # extract theyear and theday
    yeardate = mosaickey2[i]
    theyear = int(yeardate[0:4])
    theday = int(yeardate[4:7])

    # used when working with individual files instead of mosaics - includes 'L'
    # theyear = int(fileName[1:5])
    # theday = int(fileName[5:8])

    # calc day and year
    arcpy.CalculateField_management(thestatsname, 'Day', theday, 'PYTHON')
    arcpy.CalculateField_management(thestatsname, 'Year', theyear, 'PYTHON')

    # calc date
    thedate = datetime.datetime(theyear, 1, 1) + datetime.timedelta(theday - 1)
    # newdate = thedate.strftime('%Y-%m-%d')
    epoch = datetime.datetime(1899, 12, 30)
    days = (thedate - epoch).days
    arcpy.CalculateField_management(thestatsname, 'Date', days, 'PYTHON')

    # deleted temp rasters above and directory removal below now works
    shutil.rmtree(os.path.join(temp_dir[i], 'cellsml'), ignore_errors=True)
    shutil.rmtree(temp_dir[i], ignore_errors=True)
    shutil.rmtree(archive_dir[i], ignore_errors=True)

# Need to get a list of stat files for mosaic key

stats_dir = os.path.join(extract_path, 'mosaic', 'stats')

thestatsname = list()
for i, file in enumerate(os.listdir(stats_dir)):
    if file.endswith(".dbf"):
        thestatsname.append(os.path.basename(file))

thestatsname = thestatsname[(hab_day_start - 1):(hab_day_end + 1)]

# Convert table to df, rename fields, add new field, and append excel file
# for i in range(0, hab_days_length):
#     # test: i = 0
#     dbf = Dbf5(os.path.join(stats_dir, thestatsname[i]))
#     df = dbf.to_dataframe()
# 
#     # add new field and rename existing fields
#     df.insert(3, "PercentArea_Value", '', True)
#     df.rename(columns={"MIN": "MIN_cellsml", "MAX": "MAX_cellsml", "RANGE": "RANGE_cellsml", "MEAN": "MEAN_cellsml",
#                        "STD": "STD_cellsml"})
# 
#     # append new data to exsiting excel spreadhseet
#     # BKK - spreadhseet and worksheet shouldn't have date
#     # dir_Shiny = "\\\\deqhq1\\wq-share\\Harmful Algal Blooms Coordination Team\\HAB_Shiny_app\\data"
#     thetable = os.path.join(dir_Shiny, 'HAB_resolvablelakes_2022.xlsx')
#     writer = pd.ExcelWriter(thetable, engine='openpyxl', mode='a', if_sheet_exists='overlay')
#     writer.book = load_workbook(thetable)
#     writer.sheets = dict((ws.title, ws) for ws in writer.book.worksheets)
#     # BKF - need to make sure that startrow isn't overwriting last row
#     firstrow = writer.book['HAB_resolvable_lake_data'].max_row
# 
#     df.to_excel(writer, sheet_name='HAB_resolvable_lake_data', startrow=firstrow, startcol=0, index=False, header=None)
# 
#     writer.save()
#     #writer.close

# Set workspace and directory for reprojection; manually adjust for now
env.workspace = os.path.join(extract_path, "mosaic")
arcpy.env.compression = "LZW"
arcpy.env.overwriteOutput = True
final_dir = os.path.join(extract_path, "mosaic", "web")
final_dir_Shiny = os.path.join(dir_Shiny, year)

# Set up spatial data for reprojection

arcpy.CheckOutExtension("Spatial")

# themask = r"\\deqhq1\wq-share\Harmful Algal Blooms Coordination Team\GIS\cyan\HAB_deschutes2020.gdb\stateline_buffer50albers"
# theextent = r"\\deqhq1\wq-share\Harmful Algal Blooms Coordination Team\GIS\cyan\HAB_deschutes2020.gdb\stateline_buffer50web"

for raster in mosaicfilename2:
    fileName, fileExtension = os.path.splitext(raster)
    tile = fileName[-8:]
    tmpfile = "temp.tif"
    newfile = (tile + fileExtension)
    print(os.path.join(final_dir_Shiny, tile + fileExtension))

    arcpy.env.extent = theextent
    arcpy.ProjectRaster_management(in_raster=raster, out_raster=os.path.join(final_dir_Shiny, tmpfile),
                                   out_coor_system="PROJCS['WGS_1984_Web_Mercator_Auxiliary_Sphere',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Mercator_Auxiliary_Sphere'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',0.0],PARAMETER['Standard_Parallel_1',0.0],PARAMETER['Auxiliary_Sphere_Type',0.0],UNIT['Meter',1.0]]",
                                   resampling_type="NEAREST", cell_size="300 300",
                                   geographic_transform="WGS_1984_(ITRF00)_To_NAD_1983", Registration_Point="",
                                   in_coor_system="PROJCS['USA_Contiguous_Albers_Equal_Area_Conic_USGS_version',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-96.0],PARAMETER['Standard_Parallel_1',29.5],PARAMETER['Standard_Parallel_2',45.5],PARAMETER['Latitude_Of_Origin',23.0],UNIT['Meter',1.0]]")
    tmpraster = os.path.join(final_dir_Shiny, tmpfile)
    finalraster = os.path.join(final_dir_Shiny, newfile)

    arcpy.Clip_management(in_raster=tmpraster,
                          rectangle="-13975366.498500 5052033.819600 -12850574.046900 5935465.862100",
                          out_raster=finalraster,
                          in_template_dataset=themask, nodata_value="-3.402823e+038",
                          clipping_geometry="ClippingGeometry", maintain_clipping_extent="NO_MAINTAIN_EXTENT")

print("done")
