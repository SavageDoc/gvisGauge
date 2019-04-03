# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Demo of googleVis Gauge Charts in shiny")
  , fluidRow( column( width=4, selectInput( 'varSelect'
                                            , label='Select variables:'
                                            , multiple=TRUE
                                            # Select input variables
                                            , choices=xvars
                                            # By default, use baseline regression
                                            , selected=c('MoSold'
                                                         , 'YrSold'
                                                         , 'LotArea'
                                                         , 'BedroomAbvGr')
  ) )
  , column( width=2, checkboxInput( 'logScale', label='Log Scaling', value=FALSE ) )
  , column( width=4, radioButtons( 'algChoice'
                                   , label='Select Algorithm:'
                                   , choices=c('LM', 'SVM')
                                   , inline=TRUE
                                   , selected='LM') )
  )
  , fluidRow( column( width=4
                      , offset=4
                      , actionButton( inputId='goModel', label='Calculate!' ) 
                      ) 
              )
  , splitLayout( 
    plotOutput( 'residPlot' )
    , plotOutput( 'obsVsPredPlot' )
    , htmlOutput( 'gaugePlot' ) )
)

