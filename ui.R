
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
    
      
      tags$head(tags$style(HTML("td.small{width:10px;}")),
                tags$script(HTML("
      $('html > head').append('<meta charset='UTF-8'/>');
                                 "))
                ),
 
      
      tabItems(
        
        
          tabItem(tabName = "instructions", 
                  fluidRow(
                   tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                               src="https://www.dropbox.com/s/so9gi1g4s5pxkv2/instruktion_verktyg.pdf?raw=1"))), #instruktion_verktyg_v.5_190326.pdf
          
          
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
                         uiOutput("selectRegion"))
                ),
                fluidRow(
                  column(8,
                         uiOutput("selectPeriod")),
                  column(3,
                         "")
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
                        column(3,h1(" ")),
                        column(2,h1(" "),uiOutput("buttonExtrap")
                        )),
               fluidRow(column(5,h4("Indicator Availability & Selection"),
                               p("OK = indicator based on sufficient data."),
                               p("(OK) = indicator was calculated but strict data requirements not met.")
               ),
                        column(4,h4("Extrapolation Available"),
                               p("List of indicators for which extrapolation can be done."))
                               
               ),
               fluidRow(
                 column(2,uiOutput("toggleIndicators")),
                 column(3,uiOutput("selectDefault")),
                 column(4,uiOutput("toggleExtrapolate"))
               ),
              
               fluidRow(column(5,
                               DT::dataTableOutput("dtind"),
                               checkboxInput("IgnoreErr", 
                                             "Use indicators not meeting strict requirements. E.g. having data for <3 out of 6 years.",
                                              value = TRUE, width = '100%')
                               ),
                        column(4,
                               DT::dataTableOutput("dtindextrap"),
                               textOutput("txtExtrapRows"),
                               textOutput("txtExtrapSelect")
                        ))
               ),
 
        # tab content
        tabItem(tabName = "extrapolation",
                h3(textOutput("SelectedWB2")),
                fluidRow(column(4,""),
                         column(2,h1(" "),uiOutput("goButton")),
                         column(4,h1(" "))
                         ),
                fluidRow(column(4,h4(textOutput("NoticeExtrapolation"))),
                         column(2,h4("WBs for extrapolation:")),
                         column(2,uiOutput("toggleWBs")),
                         column(1,uiOutput("applyWBs"))
                ),
                fluidRow(
                  column(4,""),
                  column(5,"")
                ),                         
                fluidRow(
                  column(4,DT::dataTableOutput("dtextrap")),
                  column(5,DT::dataTableOutput("dtextrapstn"))
                )                         
        ),
        
        # tab content
        tabItem(tabName = "status",
                fluidRow(column(width=4,
                                h3(textOutput("SelectedWBStatus")),
                                textOutput("SelectedTypeStatus")
                ),
                column(width=2,h1(" "),uiOutput("download")),
                column(width=4,h1(" "),uiOutput("viss"),uiOutput("decimalsymbol")),
                column(width=1,h1(" "),"")
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
  
  tabItem(tabName = "disclaimer",
          fluidRow(column(width=4,
          h3("Disclaimer"),
          p("Detta verktyg är ett besluts- och beräkningsstöd framtagen för använding inom Vattenmyndigheternas statusklassning 2019 i enlighet med Havs- och Vattenmyndighetens föreskrift (HVMFS 2013:19) och den vägledning som tagits fram under 2018, samt med hjälp av de övergripande metoder som utvecklades inom forskningsprogrammet WATERS. Verktyget är fortfarande under utveckling och data som hämtats från de nationella datavärdskapen kan uppdateras vid behov."),
          
          p("Verktyget tillhandahåller statusklassningar med klassningsosäkerheter för enskilda indikatorer såväl som för sammanvägda kvalitetsfaktorer, men de automatiskt beräknade klassningarna som presenteras i verktyget måste alltid genomgå en manuell prövning och kan modifieras av speciellt avsedd personal vid vattenmyndigheterna."),
          
          p("Utveckling av verktyget har finansierats via anslag från Havs- och Vattenmyndigheten till Havsmiljöinstitutet, SMHI, SLU, Århus Universitet och NIVA Danmark. Verktyget har utformats i dialog med användare på Vattenmyndigheterna / Länsstyrelserna och support till andra användare ges endast i ringa omfattning. Dessa hänvisas främst till den instruktion som följer med verktyget eller till rapporterna på waters.gu.se."),
          
            p("Mats Lindegarth, projektledare"),
          p(a("mats.lindegarth@havsmiljoinstitutet.se",href="mailto:mats.lindegarth@havsmiljoinstitutet.se"))
          ),
          column(width=4,
                 h3("Disclaimer"),
          p("This is a decision-making and calculation support tool designed for use in the Water Authorities' status classification 2019, in accordance with the Swedish Agency for Marine and Water Management's regulations (HVMFS 2013: 19) and the guidance that was produced in 2018, and with the help of the overall methods developed in the research program WATERS. The tool is still under development and data retrieved from the national data hosts can be updated as needed."),
          p("The tool provides status assessments with classification uncertainties for individual indicators as well as for weighted Quality Elements. The automatically calculated classifications presented in the tool must always undergo a manual test and can be modified by specially designated personnel at the water authorities."),
          p("The development of the tool has been financed via grants from the Swedish Agency for Marine and Water Management to the Marine Environment Institute, SMHI, SLU, Aarhus University and NIVA Denmark. The tool has been designed in dialogue with users at the Water Authorities / County Administrative Boards. Support to other users is limited and they are referred to the instructions that come with the tool or to the reports at waters.gu.se.")
                 
                 )          
          )

          
                    
  ),
  
  

        # tab content
        tabItem(tabName = "options",
                h3("Options"),
                h3("Monte Carlo"),
                numericInput("n",
                             label = "Number of simulations", min=1,
                             value = 1000),
                 p("Options for Monte Carlo simulations.")#,
                #h3("Indicator List"),
                #checkboxInput("chkClassBnds","Show Class Boundaries", value=FALSE, width=NULL)
               #uiOutput("chkIndicators")
                
        )
        
      )
  
  )
  )
)



