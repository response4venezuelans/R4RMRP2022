#### RMRP 2022 Shiny app #####

#### Page 1. Load Data in English #####
dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  skin = "black",
  dashboardBody(
    img(src = "r4v.png", height = 80),
    tabsetPanel(
      tabPanel(title = "1.Data Upload in English",br(),
               p("V1 Released on XX/03/2022, please send any comments to the Regional platform IM team", style="color: #fff; background-color: #672D53"),
               
                 
                 column(8,shinydashboard::box(id="box_2", title = "Please copy paste the data with the header or upload you regional ENG 5W import table or load all the records from the API", solidHeader = T,collapsible = T,collapsed = F,
                                              width = 12,status = "primary",
                                              
                                              
                                              fluidRow(
                                                column(
                                                  width = 6,
                                                  import_copypaste_ui("myid", title = "Paste with the header row."),
                                                
                                                ),

                                                column(width = 5,
                                                       br(),
                                                       p("or Load all the data from Activity Info (API) regional database (2min)"),
                                                       actionButton(inputId = "Run_Script",label = "Load from Regional Database", icon = icon("bone"), width = "300px", style="color: #fff; background-color: #00AAAD"),
                                                ))))),
               br(),
               fluidRow(column(12,shinydashboard::box(id="box_9", title = "Preview data", solidHeader = T,collapsible = T,collapsed = F,
                                                      width = 12,status = "primary",
                                                      DT::dataTableOutput("Preview_Data")
               ))),
               
               
               fluidRow(column(1,shinydashboard::box(id="box_15", title = "Control", solidHeader = T,collapsible = T,collapsed = F,
                                                      width = 12,status = "primary",
               tags$b("Imported data:"),
               verbatimTextOutput(outputId = "status"),
               verbatimTextOutput(outputId = "data")
               ))),
##### Page2. Data Quality Check ############
    tabPanel(title = "2.Error and cleaning script",br(),
             p("Data Quality Check for English version, V1 Updated XX/03/2022, please send any comments to the Regional platform IM team", style="color: #fff; background-color: #672D53"),
             
             fluidRow(
               
               column(3,shinydashboard::box(id="box_2", title = "Error Report and cleaning scripts", solidHeader = T,collapsible = T,collapsed = F,
                                            width = 12,status = "primary",
                                            p("Please select your country and the automatic cleaning script you want to apply and RUN SCRIPT"),
                                            fluidRow(
                                              column(1,selectInput("country_name",label = "Country Name",choices = c("NULL")))),
                                            actionButton("run_err_report",label = "Run Script",icon = icon("black-tie"), style="color: #fff; background-color: #00AAAD"), 
                                            downloadButton("downloadprecleaned", "Download Error report", style="color: #fff; background-color: #672D53"),
                                            )),
               column(3,shinydashboard::box(id="box_3", title = "Summary", solidHeader = T,collapsible = T,collapsed = F,
                                            width = 12,status = "warning",
                                            p("Number or Activities"),
                                            h2(textOutput("Number_of_Activities")),
                                            p("Number of errors Pre cleaning"),
                                            h2(textOutput("Number_of_Errors_Pre")),
                                            p("Percentage of errors"),
                                            h2(textOutput("Percentage_of_Errors")),
               ))))


    )))


##### Page 3. Consolidated report ############


##### Page 4. Upload Spanish data ############

