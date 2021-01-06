#' Get the weather
#'
#' Get the weather at a given site (or the closest downloaded site), in the specified time period.
#' The Copernicus weather data is in hours, but may be aggregated into days.
#'
#' Variables that have been downloaded from the ERA5 dataset include:
#'   d2m - 2-metre dewpoint temperature (K)
#'   t2m - 2-metre temperature (K)
#'   sp - surface pressure (Pa)
#'   tp - total precipitation (m)
#'   ssrd - surface solar radiation downwards (J/m^2)
#'
#'Variables that are extrapolated are:
#'   RH - relative humidity
#'
#'
#' @param longitude
#' @param latitude
#' @param timefrom
#' @param timeto
#' @param unit
#' @param variables
#'
#' @return
#' @export
#'
#' @examples
getWeather<-function(longitude,latitude,timefrom,timeto,unit="hour",variables=NA){

    # Check the format of timefrom and timeto
    if(!(class(timefrom) %in% c("POSIXct", "POSIXlt","Date"))) stop("timefrom needs to be a date (either POSIXct, POSIXlt or Date format)")
    if(!(class(timeto) %in% c("POSIXct", "POSIXlt","Date"))) stop("timefrom needs to be a date (either POSIXct, POSIXlt or Date format)")

    # Check the unit
    if(!(tolower(unit) %in% c("hours","day","days","hour"))) stop("Unit must be a character string, either: 'hour' or 'day'")

    # If variables == NA, should return all available variables

    # Get the starting year
    startYear = as.numeric(format(timefrom,"%Y"))
    endYear = as.numeric(format(timeto,"%Y"))

    # In each site get the appropriate weather data
    for(iYear in startYear:endYear){

        if(iYear == startYear){ DF = getWeatherYear(longitude,latitude,year=iYear,variables)
        } else DF = rbind(DF,getWeatherYear(longitude,latitude,year=iYear,variables))

    }

    # Cut out the bits that aren't needed
    DF = subset(DF,as.Date(DF$DateTime) >= timefrom & as.Date(DF$DateTime) <= timeto)

    # If unit == "day" then aggregate by day
    if(unit == "day" | unit == "days"){
        DF = aggregateWeather(DF)
    }

    return(DF)

}

# A function to provide the weather that is closest to the given longitude, latitude and start and end dates (from downloaded weather data)
getWeatherYear<-function(longitude,latitude,year,variables=NA){

    # Print a warning if the variables supplied are not in the weather data, then stop and send a warning
    if(!is.na(variables)) if(any(!(variables %in% names(DATASET.summary)))) stop(paste("No variables found of that name. Possible variables are:",names(DATASET.summary)[-(1:4)]))

    # Extract the weather in the correct year and with the required variables
    cutDATASET = subset(DATASET.summary,Year==year)
    if(any(!is.na(variables))){
        for(iVar in variables){
            cutDATASET = cutDATASET[cutDATASET[[iVar]]==TRUE,]
        }
    }

    if(nrow(cutDATASET) == 0) stop("Can't find any weather data with the specified variables in the given year")

    # Get the closest weather datapoint
    distance = grandCircleDistance(longitude,latitude,cutDATASET$Longitude,cutDATASET$Latitude)

    # Find the closest weather station
    whichSite = which.min(distance)

    print(paste("Closest weather station is",format(distance[whichSite],digits=2),"km away from site"))

    # Open the appropriate cdf data
    netCDFData = DATASET[[ cutDATASET$Index[whichSite] ]]

    # Make a data.frame with just the time attribute
    DF = data.frame(DateTime = ncdf4::ncvar_get(netCDFData, "time"))
    # Convert to the correct units
    originDate = as.POSIXct("1900-01-01 00:00:00")
    DF$DateTime = originDate + DF$DateTime * 60 * 60

    # Get the index of the latitude and longitude in the netcdf data
    iLon = which.min(abs(ncdf4::ncvar_get(netCDFData,"longitude") - longitude))
    iLat = which.min(abs(ncdf4::ncvar_get(netCDFData,"latitude") - latitude))

    # Add the latitude and longitude to the data.frame
    DF = cbind(DF,Longitude = rep(ncdf4::ncvar_get(netCDFData,"longitude")[iLon],nrow(DF)))
    DF = cbind(DF,Latitude = rep(ncdf4::ncvar_get(netCDFData,"latitude")[iLat],nrow(DF)))

    # Retrieve each of the variables
    for(i in attributes(netCDFData$var)$names){
        DF = cbind(DF,ncdf4::ncvar_get(netCDFData, i)[iLon,iLat,])
        names(DF)[length(names(DF))] = i
    }

    # Rename the variables
    for(iCol in 4:ncol(DF)){
        if(names(DF)[iCol] == "d2m") names(DF)[iCol] = "DewPointTemp"
        if(names(DF)[iCol] == "t2m") names(DF)[iCol] = "Temperature"
        if(names(DF)[iCol] == "sp") names(DF)[iCol] = "SurfacePressure"
        if(names(DF)[iCol] == "tp") names(DF)[iCol] = "Precipitation"
        if(names(DF)[iCol] == "ssrd") names(DF)[iCol] = "SolarRadiation"
    }

    # Convert temperatures from kelvin to celsius
    DF$DewPointTemp = DF$DewPointTemp - 273.15
    DF$Temperature = DF$Temperature - 273.15

    # Calculate further variables:
    # Relative humidity using the August-Roche-Magnus approximation (https://bmcnoldy.rsmas.miami.edu/Humidity.html)
    DF$RH = 100*exp(17.625 * DF$DewPointTemp / (243.04 + DF$DewPointTemp))/exp(17.625 * DF$Temperature / (243.04 + DF$Temperature))

    # Close the file
    ncdf4::nc_close(netCDFData)

    # Return the weather data.frame
    return(DF)

}

# Aggregate weather
aggregateWeather<-function(hourWeather){

    hourWeather$Day = factor(floor(difftime(hourWeather$DateTime,hourWeather$DateTime[1],units="days")))
    hourWeather$Date = as.Date(hourWeather$DateTime)

    # Create a data.frame for each day (doing it this way accounts for leap years)
    dayWeather = data.frame(Day = unique(as.Date(hourWeather$DateTime)))
    # Calculate the average temperature each day (and night)
    dayWeather$mean = tapply(hourWeather$Temperature,hourWeather$Day,mean)
    # Calculate the minimum temperature each day
    dayWeather$minTemp = tapply(hourWeather$Temperature,hourWeather$Day,min)
    # Calculate the maximum temperature each day
    dayWeather$maxTemp = tapply(hourWeather$Temperature,hourWeather$Day,max)
    # Calculate the number of hours that RH >= 90%
    dayWeather$hoursRH.90 = tapply(hourWeather$RH,hourWeather$Day,FUN=function(x) sum(x >= 90.0))
    # Calculate the minimum RH each day
    dayWeather$minRH = tapply(hourWeather$RH,hourWeather$Day,min)
    # Calculate the total precipitation each day
    dayWeather$Precipitation = tapply(hourWeather$Precipitation,hourWeather$Day,sum)

    return(dayWeather)

}
