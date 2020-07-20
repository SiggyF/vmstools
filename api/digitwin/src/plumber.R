#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)

B0 <- read.csv('~/data/tki/wmr/B0.csv')
B1 <- read.csv('~/data/tki/wmr/B1.csv')
maindata <- read.csv('~/data/tki/wmr/maindata.csv')
Amat <- read.csv('~/data/tki/wmr/Amat.csv')
W <- read.csv('~/data/tki/wmr/W.csv')
hyperparams <- read.csv('~/data/tki/wmr/hyperparams.csv')

#* @apiTitle DigitalTwin R fucnctions

#* Compute production of shrimp in a point if another area is closed
#* @param location The location where to estimate the production
#* @param closed area The location of the closed area
#* @get /predict/hurdle
#* @post /predict/hurdle
function(location) {
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
