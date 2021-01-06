## code to prepare `DATASET` dataset goes here

require(ncdf4)
require(tools)

# Make a list of files in the .nc folder
files = list.files("./")
# Get only those files that end in .nc
fileExt = file_ext(files)
files = files[fileExt == "nc"]

# variables
variables = character()

# Loop over each .nc file in this folder - first time round, just capture all variables in each .nc
for(iFile in files){

    # Read in the netCDF file
    netCDFData = nc_open(iFile)

    # Get the variables in this file
    iVars = names(netCDFData$var)

    # Store variables that are not in the variables string
    variables = c(variables,iVars[!(iVars %in% variables)])

}

# DATASET.summary is going to be a data.frame of weather variables
DATASET.summary = data.frame(year=numeric(),latitude=numeric(),longitude=numeric(),index=character())
# Add a column for each variable
for(ivar in variables) DATASET.summary[,ivar] = numeric()
# Initiate the list that will contain the ncdf4 objects
DATASET = list()

# Now... loop through each file, check if any of it is in the data.frame already, and store it
# Let's see how big this gets
for(iFile in 1:length(files)){

    # Read in the file
    netCDFData = nc_open(files[iFile])

    # Get the locations of each
    latitude = ncvar_get(netCDFData,"latitude")
    longitude = ncvar_get(netCDFData,"longitude")

    # Get the years in this data.frame
    # First extract the time array
    nCDFtime = ncvar_get(netCDFData,"time")
    # Convert to a date
    originDate = as.POSIXct("1900-01-01 00:00:00")
    nCDFtime = originDate + nCDFtime * 60 * 60
    # Convert to the years:
    years = unique(as.integer(format(nCDFtime,"%Y")))

    # TODO: Probably ought to ensure that the data encompasses the entire year

    DF = expand.grid(years,latitude,longitude,iFile)
    names(DF) = c("Year","Latitude","Longitude","Index")

    # Check which variables are included
    for(iVar in variables){
        if(iVar %in% attributes(netCDFData$var)$names){ DF[,iVar] = TRUE
        } else DF[,iVar] = FALSE
    }

    DATASET[[iFile]] = netCDFData

    # Add to DATASET.summary
    DATASET.summary = rbind(DATASET.summary,DF)

    nc_close(netCDFData)

}

usethis::use_data(DATASET,DATASET.summary, overwrite = TRUE)
