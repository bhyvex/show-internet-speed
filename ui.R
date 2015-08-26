library(shiny)

shinyUI(fluidPage(
  title='Internet Speed',
  h4(textOutput('lastSpeed')),
  h4(textOutput('lastTime')),
  fluidRow(column(8,offset=1,
                  tabsetPanel(position='above',
                              tabPanel('ping',plotOutput('pingplot')),
                              tabPanel('download',plotOutput('downloadplot')),
                              tabPanel('upload',plotOutput('uploadplot')))
                  )
           ),
  fluidRow(column(3,offset=1,uiOutput('choose_ip')),
           column(4,uiOutput('choose_dates'))
           )
))
