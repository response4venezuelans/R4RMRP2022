#add upload capacity to shiny to 30MB

options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {
  
  ## Declaring Variables
  Data <- reactiveVal()
  Consolidated <- reactiveVal()
  Error_Download <- reactiveVal()

##### 1. Load Data ######
  
  imported <- import_copypaste_server("myid")
  
  
  output$status <- renderPrint({
    imported$status() })
  
    output$data <- renderPrint({
      source("R/1_1_read_data_local.R")
        
    Data(read_data_2022_local(imported$data()))
      showNotification("Data Processing Complete",duration = 10, type = "error")
      
    updateSelectInput(session,"country_name",choices = unique(Data()$Country))
    updateSelectInput(session,"country_name_agg",choices = unique(Data()$Country))
    
    
    })
    
 
  ## Observe file Input
  observeEvent(
    
    input$data_upload,{
    filename <- tolower(input$data_upload$name)
    
    ## Condition to check file type
    if(!(sub('.*\\.','',filename)) %in% 'xlsx'){
      showNotification('Only XLSX are supported',duration = 5)
      req(F)
    }
    # Skip=2 get rid of the 2 toplines as per the template
    Data(read_excel(input$data_upload$datapath))
    source("R/1_1_read_data_local.R")
    Data(read_data_2022_local(Data()))
    showNotification("Data Processing Complete",duration = 10, type = "error")
    
    # Update the drop down button with Countries
    
    updateSelectInput(session,"country_name",choices = unique(Data()$Country))
    updateSelectInput(session,"country_name_agg",choices = unique(Data()$Country))
  })
  
  ## Data Preview
  output$Preview_Data <- DT::renderDataTable({Data()},extensions = c("Buttons"), options = list(
    dom = 'lfrtip', 
    # add B for button
    paging = TRUE,
    ordering = TRUE,
    lengthChange = TRUE,
    pageLength = 10,
    scrollX = TRUE,
    autowidth = TRUE,
    rownames = TRUE
    # buttons = c('copy', 'csv', 'excel', 'pdf')
  ))
  
  observeEvent(input$Run_Script,{
    source("R/1_read_data.R")
    Data(read_data_2022())
    showNotification("Data Processing Complete",duration = 10, type = "error")
    updateSelectInput(session,"country_name",choices = c("All",unique(Data()$Country)))
    updateSelectInput(session,"country_name_agg",choices = c("All",unique(Data()$Country)))
  })
 
##### 2. Data Quality Check ###### 
  
  observeEvent(input$run_err_report,{
    source("R/2_data_quality_check.R")
    Error_report <- r4v_error_report(data = Data(),
                                     countryname = input$country_name,
                                     write = "yes")
    
    
    Error_Download(Error_report$ErrorReportclean)
    
    # To output number of activities and error 
    
    output$Number_of_Activities <- renderText({nrow(Error_report$ErrorReportclean)})
    output$Number_of_Errors_Pre <- renderText({sum(!is.na(Error_report$ErrorReportclean$Review))})
    output$Percentage_of_Errors <- renderText(round({sum(!is.na(Error_report$ErrorReportclean$Review))}/{nrow(Error_report$ErrorReportclean)}*100, digits = 1))
    
    # PLOTLY section
    
    output$plot <- renderPlotly({
      Error_report$ErrorReportclean %>%
        filter(!is.na(Review)) %>%
        ggplot() +
        aes(x = Appealing_org, size = Review) +
        geom_bar(fill = "#0c4c8a") +
        coord_flip() +
        theme_minimal()})
    
    output$plot2 <- renderPlotly({
      Error_report$ErrorReportclean %>%
        filter(!is.na(Review)) %>%
        ggplot() +
        aes(x = Country, size = Review) +
        geom_bar(fill = "#0c4c8a") +
        coord_flip() +
        theme_minimal()})
    
    
    showNotification("Successful",duration = 10, type = "error")
  })
  
 })