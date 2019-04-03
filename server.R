# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent( input$goModel, {
    # Update the formula
    myFormula <- makeFormula( logScale=input$logScale
                              , varSelect=input$varSelect )
    # Build the model
    myModel <- makeModel( input$algChoice, myFormula )
    # Get predictions
    yPred <- predValues( myModel )
    # Note metric is log price, indepdent of what is calculated
    if( !input$logScale ){ yPred <- log( yPred ) }
    # Get observed (for ease)
    yObs <- houseTrain$logPrice
    # Get metrics
    # RMSE (note both are log-price!)
    myRMSE <- calcRMSE( yObs, yPred )
    # MAPE (convert to raw price)
    myMAPE <- calcMAPE( exp( yObs ), exp( yPred ) )
    # Update RV
    gaugeRV$myFormula <- myFormula
    gaugeRV$myModel <- myModel
    gaugeRV$predValues <- yPred
    gaugeRV$rmseValue <- myRMSE
    gaugeRV$mapeValue <- myMAPE
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  ## Build plots ----
  # Histogram of residuals
  output$residPlot <- renderPlot({
    residValues <- data.frame( Residual=gaugeRV$predValues - houseTrain$logPrice )
    residPlot <- ggplot( residValues, aes( x=Residual ) ) +
      geom_histogram( bins=30, colour='blue', fill='blue' ) +
      labs( x='Residual'
            , y='Count'
            , title='Histogram of residuals'
            , subtitle='Log space' )
    residPlot
  })
  # Observed vs Predicted
  output$obsVsPredPlot <- renderPlot({
    modelValues <- data.frame( Predicted = gaugeRV$predValues
                               , Observed=houseTrain$logPrice )
    obsVsPredPlot <- ggplot( modelValues, aes( x=Predicted, y=Observed ) ) +
      geom_abline( slope=1, intercept=0, colour='black', linetype=3, size=2 ) +
      geom_point( colour='blue' ) +
      labs( x='Predicted'
            , y='Observed'
            , title='Comparison of predicted and observed prices'
            , subtitle='Log space' )
    obsVsPredPlot
  })
  # googleVis Gauge for model status
  output$gaugePlot <- renderGvis({
    rmseData <- data.frame( Label='RMSE'
                             , RMSE=gaugeRV$rmseValue )
    
    gaugePlotRMSE <- gvisGauge( data=rmseData
                            , labelvar='Label'
                            , numvar='RMSE' 
                            , options=list( min=LOW_GREEN_LOG_RMSE
                                            , greenFrom=LOW_GREEN_LOG_RMSE
                                            , greenTo=HIGH_GREEN_LOG_RMSE
                                            , yellowFrom=LOW_AMBER_LOG_RMSE
                                            , yellowTo=HIGH_AMBER_LOG_RMSE
                                            , redFrom=LOW_RED_LOG_RMSE
                                            , redTo=HIGH_RED_LOG_RMSE
                                            , max=HIGH_RED_LOG_RMSE ) 
                            ) 
    
    mapeData <- data.frame( Label='MAPE'
                            , MAPE = gaugeRV$mapeValue )
    
    gaugePlotMAPE <- gvisGauge( data=mapeData
                                , labelvar='Label'
                                , numvar='MAPE' 
                                , options=list( min=LOW_GREEN_MAPE
                                                , greenFrom=LOW_GREEN_MAPE
                                                , greenTo=HIGH_GREEN_MAPE
                                                , yellowFrom=LOW_AMBER_MAPE
                                                , yellowTo=HIGH_AMBER_MAPE
                                                , redFrom=LOW_RED_MAPE
                                                , redTo=HIGH_RED_MAPE
                                                , max=HIGH_RED_MAPE ) 
    ) 
    
    gaugePlot <- gvisMerge( gaugePlotRMSE, gaugePlotMAPE, horizontal= FALSE )
    
    gaugePlot
  })
}
