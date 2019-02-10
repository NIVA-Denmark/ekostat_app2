
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)


ui <- 
shinyUI(
  
  
  dashboardPage(skin = "black",title="WATERS Status Assessment Tool",

    dashboardHeader(title = tags$a(tags$img(src='waters_2.gif',height='20',width='204'))
                                  ),
    
    dashboardSidebar(
      sidebarMenuOutput(outputId = "dy_menu")),
    dashboardBody(
    
      
      tags$head(tags$style(HTML("td.small{width:10px;}"))),
 
      
      tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                       Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                       })")),
      
      
      tabItems(
  # tab content
        tabItem(tabName = "waterbody",
                h3("Select Waterbody"),
                fluidRow(
                  column(2,
                         uiOutput("selectWaterType")),
                  column(2,
                         uiOutput("selectLan")),
                  column(2,
                         uiOutput("selectMun")),
                  column(2,
                         uiOutput("selectType")),
                  column(3,
                         uiOutput("selectPeriod"))
                ),
                fluidRow(
                  column(6,
                         ""),
                  column(3,
                         "")#,
                ),
                
                fluidRow(column(10, offset = 0,
                                #box(
                                #  title = "Title 1", width=10,solidHeader = TRUE, status = "primary",
                                  DT::dataTableOutput("dtwb")
                                #),
            )),
            fluidRow(
              column(10, offset = 0,
                     h4(textOutput("wb_info")))
              ),
            fluidRow(
              column(3,offset = 1,
                     uiOutput("buttonWB"))
              )
            
          ),
  
        # tab content
        tabItem(tabName = "indicators",
               h3(textOutput("SelectedWB")),
               fluidRow(column(9,textOutput("SelectedType"))),
               fluidRow(column(4,uiOutput("selectPressure")),
                        column(2,h1(" "),uiOutput("goButton")),
                        column(4,h1(" "),checkboxInput("IgnoreErr", 
                                             "Use all data. For for the selected waterbody, use indicators which have data for fewer than 3 out of 6 years.",
                                              value = TRUE, width = '100%')#,
                               #p("Use all data for the selected waterbody, including indicators which have data for fewer than 3 out of 6 years.")#,
                        #column(1,h2(" "),uiOutput("btnExtrap"))
                        )),
               fluidRow(column(7,h4("Indicator availability")),
                        column(3,h4("WBs for extrapolation:"))
               ),
               #fluidRow(column(3, offset = 0,align="left",
               #                checkboxInput("indAll","Indicators - Select/Deselect All ",
               #                              value = TRUE, width = '100%')),
               #         column(4, offset = 0,align="right",
               #                checkboxInput("extrapAll","Extrapolation - Select/Deselect All ",
               #                              value = TRUE, width = '100%'))
               #),
        
               fluidRow(column(7,DT::dataTableOutput("dtind")
                               ),
                        column(3,DT::dataTableOutput("dtextrap"))
               )
               ),
   
        # tab content
        tabItem(tabName = "status",
                fluidRow(column(width=4,
                                h3(textOutput("SelectedWBStatus")),
                                textOutput("SelectedTypeStatus")
                ),
                column(width=4,h1(" "),uiOutput("download"))
                #column(width=4,h1(" "),downloadButton("downloadButton"))
                ),

                fluidRow(column(width=3,htmlOutput("titleTable1"),DT::dataTableOutput("resTable1")),
                         column(width=4,htmlOutput("titleTable2"),DT::dataTableOutput("resTable2")),
                         column(width=5,htmlOutput("titleTable3"),DT::dataTableOutput("resTable3"))
                                         ),
                fluidRow(column(width=5,offset=7,htmlOutput("titleTable4"),DT::dataTableOutput("resTable4"))
                ),
                fluidRow(column(width=12,htmlOutput("titleTableInd"),
                                DT::dataTableOutput("resTableInd"))
                ),
                fluidRow(column(width=6,h3(textOutput("titleTableObs")))),
                fluidRow(column(width=6,plotOutput("plotObs")),
                         column(width=6,DT::dataTableOutput("resTableObs"))
                )
                         

        ),

        # tab content
        tabItem(tabName = "options",
                h3("Options"),
                h3("Monte Carlo"),
                numericInput("n",
                             label = "Number of simulations", min=1,
                             value = 200),
                 p("Options for Monte Carlo simulations.")#,
                #h3("Indicator List"),
                #checkboxInput("chkClassBnds","Show Class Boundaries", value=FALSE, width=NULL)
               #uiOutput("chkIndicators")
                
        )
        
      )
  
  )
  )
)



