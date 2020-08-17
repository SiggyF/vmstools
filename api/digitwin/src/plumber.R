#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(logger)

source("DigiTwinFunction2.R")

api <- 'https://storage.googleapis.com/hydro-engine-public/wmr/shrimp'


log_info("reading data files")
B0 <- read.csv(file.path(api, 'B0.csv'))
B1 <- read.csv(file.path(api, 'B1.csv'))
maindata <- read.csv(file.path(api, 'maindata.csv'))
Amat <- read.csv(file.path(api, 'Amat.csv'))
W <- read.csv(file.path(api, 'W.csv'))
hyperparams <- read.csv(file.path(api, 'hyperparams.csv'))
log_info("data files loaded")

#* @apiTitle DigitalTwin R functions

#* Welcome message
#* @get /
function(){
    list(msg = "Welcome ot the shrimp model of the digital twin")
}

#* Compute production of shrimp in a point if another area is closed
#* @param location The location where to estimate the production
#* @get /predict/hurdle
#* @post /predict/hurdle
function(location) {
    log_info("location: ", location)
    result <- predict.hurdle(
        Location=location,
        Season='winter',
        Neardist2restrictarea = 5,
        allareasclosed = FALSE,
        B0=B0,
        B1=B1,
        maindata=maindata,
        Amat=Amat,
        W=W,
        hyperparams=hyperparams
    )
    return(result)
}
