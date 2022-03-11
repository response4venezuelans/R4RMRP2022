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
               )))))) 

##### Page 2. Data Quality Check ############


##### Page 3. Consolidated report ############


##### Page 4. Upload Spanish data ############

