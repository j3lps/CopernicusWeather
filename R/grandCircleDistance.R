#' Calculate the distance between two points
#'
#' Calculate thet distance between two points, specified by their respective latitude and longitude.
#' This function uses the grand circle distance to calculate the distance accounting for the curvature of the earth.
#' The function can be inaccurate at distance <2km, but this should not affect use with the weather model, since the resolution is not that precise.
#'
#' @param latitude1
#' @param latitude2
#' @param longitude1
#' @param longitude2
#'
#' @return
#' @export
#'
#' @examples
calculateDistance<-function(latitude1,latitude2,longitude1,longitude2){

    return(grandCircleDistance(latitude1,latitude2,longitude1,longitude2))

}

grandCircleDistance<-function(latitude1,latitude2,longitude1,longitude2){

    # Convert longitude and latitude to radians:
    long1.rad = longitude1*pi/180
    lat1.rad = latitude1*pi/180
    long2.rad = longitude2*pi/180
    lat2.rad = latitude2*pi/180

    R <- 6371 # Earth mean radius [km]

    d <- acos(sin(lat1.rad)*sin(lat2.rad) + cos(lat1.rad)*cos(lat2.rad) * cos(long1.rad-long2.rad)) * R

    return(d) # Distance in km

}
