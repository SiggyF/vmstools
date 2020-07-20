
# This function computes the expected number of times, with equal effort,
# fishers will fish in a location, summed up over a period, season, and location.
# The computation is based on a hurdle consisting of 2 spatial GLMMs:
# A Bernoulli GLMM, and a Zero-Truncated Negative Binomial (ZTNB) GLMM.

# Returns: a single number.
 
# Arguments:

# - location: Either a character string, or a numeric vector of length 2.
#     If a character string, should give the 0.01 degrees C-square location
#     (i.e. "1500:124:144:219")
#     If a vector, first number gives the longitude, and the second the latitude.

# - season: a character; either "winter", "spring", "summer", or "autumn".

# - neardist2restrictarea: the distance to the nearest restricted area, in km.

# - allareasclosed: Boolean, indicating if all restricted areas are closed at the moment.

# - maindata: the main dataset.

# - B0, B1: vectors of coefficients for the Bernoulli and ZTNB model, respectively

# - Amat: The spatial mesh matrix for the spatial random effects

# - W: The 2 vectors of random effect coefficients.
#     The first column is for the Bernoulli model,
#     the second column is for the Zero-Truncated Negative Binomial Model.

# - hyperparams: dataframe of the hyperparameters of the ZTNB model

# - timespan: a single number, giving the timespan in years.
#     Defaults to 1, meaning this function predicts the event over the span of 1 year

# - maxdist: when a particula time-location combination is not available in our data,
#     the closest location for which a similar time-location combination exists is found,
#     to approximate computations.
#     This parameter specifies the max allowed distance from the closest grid cell,
#     for such approximations.
#     Set to 0 to remove such approximations.
# 
# Note: There may not be complete data for every single time-location combination.
# For example, some wind data was not present for certain time-location combinations.

# install like this:
# library(devtools)
# install_github('nielshintzen/vmstools')
library(vmstools)

predict.hurdle <- function(Location, Season, Neardist2restrictarea,
                            allareasclosed,
                           B0=NULL, B1=NULL,
                           maindata=NULL, Amat=NULL,
                           W=NULL, hyperparams=NULL,
                           maxdist=5000, timespan=1) {
  
  # Error handling:
  
  checkinput <- c(is.character(Season), is.numeric(Neardist2restrictarea))
  
  
  if(!any(checkinput)) {
    stop("Season must be a character, distance to restricted area must be numeric")
  }
  data.isnull <- c(is.null(maindata), is.null(B0), is.null(B1), is.null(Amat),
                 is.null(W), is.null(hyperparams))
  if(any(data.isnull)) {
    stop("not all data arguments filled in")
  }
  
  # FUNCTION:
  
  # Set season:
  ind <- with(maindata, which(season==Season))
  if(length(ind)<1){
    warning("No data with seasan found", season)
    return(NULL) # if specific season does not exist, return 0.
  }
  maindata <- maindata[ind,]
  Amat <- Amat[ind,]
  
  # Set spatial data:
  Location.numeric <- Location
  Location.character <- Location
  if(is.numeric(Location)){
    Location.character <- vmstools::CSquare(Location[1], Location[2], 0.01)
  }
  if(is.character(Location)){
    Location.numeric <- vmstools::CSquare2LonLat(Location, 0.01)
  }
  ind <- with(maindata, which(Csq==Location.character))
  if(length(ind)<1){
    warning("Location not found; ", Location.character)
    
    # if location does not exist, find closest location:
    x <- distance(maindata$lon, maindata$lat,
                  Location.numeric[1], Location.numeric[2])
    mindist <- min(x)
    x <- x[which(x <= maxdist)]
    ind <- which.min(x)
    warning("Using location at distance: ", mindist)
    
  }
  if(length(ind)<1){
    warning("No data found near location, closest ", mindist, "max distance", maxdist)
    return(NULL) # if still no data exists, return 0.
  }
  maindata <- maindata[ind,]
  Amat <- as.matrix(Amat[ind,])
  
  # set free variables:
  maindata$neardist2restrictarea <- Neardist2restrictarea[1]
  maindata$allareasclosed2017 <- as.numeric(allareasclosed[1])
  maindata$someareasclosed2013 <- ifelse(allareasclosed, 0, 0.5)
  
  # make model matrices:
  Xform0 <- reformulate(as.character(B0$names), intercept=FALSE)
  Xform1 <- reformulate(as.character(B1$names), intercept=FALSE)
  X0 <- model.matrix(Xform0, maindata)
  X1 <- model.matrix(Xform1, maindata)
  
  # check correspondence X and Beta
  correspondence <- all(c(colnames(X0)==B0$names, colnames(X1)==B1$names))
  if(!correspondence){
    stop("X matrix and Beta coefficient do not correspond with each other!")
  }
  
  # Bernoulli calculations:
  linpred.01 <- X0 %*% B0$mean + Amat %*% as.vector(W$w0)
  p.01 <- exp(linpred.01) / (1 + exp(linpred.01))
  
  # ZTNB calculations:
  munb <- exp(X1 %*% B1$mean + Amat %*% as.vector(W$w1))
  disp.size <- hyperparams$mean[1]
  Eztnb <- munb / (1 - pnbinom(0, size = disp.size, mu=munb))
  
  # Hurdle prediction, averaged over time:
  pred <- mean(round(as.vector(p.01 * Eztnb))) * timespan
  
  return(pred)
  
}


