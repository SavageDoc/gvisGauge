## Libraries ----
# Naturally, using shiny requires the shiny package!
library(shiny)
# googleVis normally has a message - suppress it.
suppressPackageStartupMessages( library( googleVis ) )
# tidyverse, just because!
library( tidyverse )
# e1071 for SVM
library( e1071 )

## Data ----
# Load regression dataset from Kaggle Ames house price challenge
houseTrainRaw <- readr::read_csv( file='../../Kaggle/kaggleHousePrices/Input/train.csv' )
# Make the names compliant
houseTrain <- houseTrainRaw %>%
  rename( FirstFloorSF=`1stFlrSF`
          , SecondFloorSF=`2ndFlrSF`
          , ThreeSsnPorch=`3SsnPorch` ) %>%
  mutate( logPrice=log( SalePrice ) ) %>%
  mutate_if( is_character, as_factor )

xvars <- setdiff( names( houseTrain ), c('SalePrice', 'logPrice' ) )

## Constants ----
LOW_GREEN_LOG_RMSE = 0
HIGH_GREEN_LOG_RMSE = 0.2
LOW_AMBER_LOG_RMSE = HIGH_GREEN_LOG_RMSE
HIGH_AMBER_LOG_RMSE = 0.4
LOW_RED_LOG_RMSE = HIGH_AMBER_LOG_RMSE
HIGH_RED_LOG_RMSE = 1

LOW_GREEN_MAPE = 0
HIGH_GREEN_MAPE = 0.15
LOW_AMBER_MAPE = HIGH_GREEN_MAPE
HIGH_AMBER_MAPE = 0.33
LOW_RED_MAPE = HIGH_AMBER_MAPE
HIGH_RED_MAPE = 1


## Functions -----
makeFormula <- function( logScale, varSelect )
{
  if( logScale ){
    myFormula <- paste0( 'logPrice ~ ', paste( varSelect, collapse=' + ' ) )
  } else {
    myFormula <- paste0( 'SalePrice ~ ', paste( varSelect, collapse=' + ' ) )
  }
  return( myFormula )
}

makeModel <- function( algChoice, myFormula )
{
  if( algChoice == 'LM' ){
    myModel <- lm( formula=myFormula, data=houseTrain )
  } else if( algChoice == 'SVM' ){
    myFormula <- as.formula( myFormula )
    myModel <- svm( formula=myFormula 
                    , data=houseTrain
                    , type='eps-regression' )
  } else {
    myModel <- lm( myFormula, data=houseTrain )
  }
  
  return( myModel )
}

predValues <- function( model ){
  # Note that fitted is overloaded between LM and SVM...
  y1 <- fitted( model )
  return( y1 )
}

calcRMSE <- function( yObs, yPred ){
  rmse <- sqrt( mean( (yObs-yPred)^2, na.rm=TRUE ) )
  return( rmse )
}

calcMAPE <- function( yObs, yPred ){
  mape <- mean( abs( yObs - yPred )/yObs, na.rm=TRUE )
  return( mape )
}

## Initialise reactiveValues ----
gaugeRV <- reactiveValues( myFormula = NULL
                           , myModel = NULL
                           , predValues =NULL
                           , rmseValue = NULL
                           , mapeValue = NULL)

