library(shiny)
library(tidyverse)
library(data.table)

# UI
ui <- fluidPage(
  
  navbarPage("Data Pal",
             # PAGE 1 ----
             tabPanel("Import",
                      sidebarLayout(
                        # Input(s) for Introduction ----
                        sidebarPanel(
                          h2("Welcome!"),
                          p("Hey, I am your data pal, let's import some data and start to explore,
                            you can import multiple data files."),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          fileInput("datafile","Choose CSV File",
                                    multiple = TRUE,
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                          ),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: check if the file has header ----
                          checkboxInput("header","Has a header?",TRUE),
                          
                          tags$hr(),
                          
                          # Select particular columns from names(data) ----
                          selectInput("select", "Select columns to display", c('col1', 'col2'), multiple = TRUE),
                          
                          actionButton("update", "Update", class = "btn-primary",style='padding:4px; font-size:100%')
                          
                          # Input: specify values to represent missing values (later version) ----
                          # Input: specify the data format (later version) ----
                          ),
                        
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Table",DT::dataTableOutput(outputId = "table")),
                                      tabPanel("Overall Summary",verbatimTextOutput("summary_all"))
                          )
                        )
                      )
             ),
             # PAGE 2 ----
             tabPanel("Bivariate Plot",
                      sidebarLayout(
                        sidebarPanel(
                          p("Let's explore the relationship between variables!"),
                          tags$hr(),
                          
                          # Select variable for x-axis
                          selectInput(inputId = "xaxis", 
                                      label = "X-axis:",
                                      choices = c('col1', 'col2')),
                                      
                          
                          # Select variable for y-axis
                          selectInput(inputId = "yaxis", 
                                      label = "Y-axis:",
                                      choices = c('col1', 'col2')),
                          
                          # # Select variable for color
                          # selectInput(inputId = "color", 
                          #             label = "Color by:",
                          #             choices = c('col1', 'col2'),
                          #             selected = NULL),
                          
                          # Select variable for facet
                          selectInput(inputId = "facet", 
                                      label = "Facet by:",
                                      choices = c('col1', 'col2'),
                                      selected = NULL),
                          
                          # # Set alpha level
                          # sliderInput(inputId = "alpha", 
                          #             label = "Transparency:", 
                          #             min = 0, max = 1, 
                          #             value = 0.5),
                          
                          # Set point size
                          sliderInput(inputId = "size", 
                                      label = "Point size:", 
                                      min = 0, max = 5, 
                                      value = 2),
                          
                          # Show correlation matrix
                          checkboxInput("corrplotcheckbox", label = "See more correlations?", value = FALSE)
                        ),
                        
                        # Outputs
                        mainPanel(
                          plotOutput(outputId = "scatterplot"),
                          tags$br(),
                          hr(),
                          #verbatimTextOutput("corrcoef")
                          #p("See all correlations:"),
                          plotOutput(outputId = "correlationmatrix")
                        )
                        
                        )
                      ),
             
             # PAGE 3
             tabPanel("Advanced")
  )
)


# Server ------------------------------------------------------
server <- function(input, output, session) {
  
  # PAGE 1: Import data and add an ID for each file (`update_list` function is cool!)----
  # header should be fixed!
  data<-reactive({
    filenumber <- c(1:length(input$datafile$datapath)) #%>% map(list)
    rbindlist(map2(lapply(input$datafile$datapath, fread),filenumber,~ update_list(.x, IAmYourFileId = .y)),
              use.names = input$header, fill = TRUE) %>% as_data_frame()
  })
  
  # PAGE 1: select data ----
  select_data <- eventReactive({
    input$update
    data()
  },{
    req(data())
    if(is.null(input$select) || input$select == "")
      data() else 
        data()[, colnames(data()) %in% input$select]
  })
  
  # PAGE 1: update selectInput ----
  observeEvent(data(), {
    updateSelectInput(session, "select", choices=colnames(data()), selected = colnames(data()))
  })

  # PAGE 2: update selectInput ----
  observeEvent(select_data(), {
    updateSelectInput(session, "xaxis", choices=colnames(select_data()))
  })
  
  observeEvent(select_data(), {
    updateSelectInput(session, "yaxis", choices=colnames(select_data()))
  })
  
  # observeEvent(select_data(), {
  #   updateSelectInput(session, "color", choices=colnames(select_data()))
  # })
  
  observeEvent(select_data(), {
    updateSelectInput(session, "facet", choices=colnames(select_data()))
  })
  
  # PAGE 1: plot selected data table ----
  output$table <- DT::renderDataTable({
    req(input$datafile)
    DT::datatable(data = select_data(), 
                  options = list(pageLength = 10), 
                  rownames = TRUE)
  })
  
 # PAGE 2: plot scatterplot of two variables ---- 
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    req(input$datafile)
    ggplot(data = select_data() , aes_string(x = input$xaxis, y = input$yaxis))+
      geom_point(size = input$size)+
      facet_wrap(as.formula(paste("~", input$facet)),ncol = 2)
  })
  
  # PAGE 2: plot correlation matrix of continuous variables ----
  corrplot_data <- eventReactive({
    input$corrplotcheckbox
    select_data()
  },{
    req(select_data())
    # guess whether a variable is a factor or not
    choose_or_not <- sapply(select_data(),is.numeric)
    select_data()[choose_or_not]
  })
  
  # test if coorplot_data was carefully selected ----
  # output$testtable <- DT::renderDataTable({
  #   req(input$datafile)
  #   DT::datatable(data = corrplot_data(), 
  #                 options = list(pageLength = 10), 
  #                 rownames = TRUE)
  # })

 

  output$correlationmatrix <- renderPlot({
    req(input$datafile)
    if(input$corrplotcheckbox){
      ggcorrplot::ggcorrplot(round(cor(corrplot_data()),2),hc.order = TRUE,
                             type = "lower", lab = TRUE)
    }
  })
  
  # # Create text output stating the correlation between the two ploted 
  # output$correlation <- renderText({
  #   req(input$datafile)
  #   r <- round(cor(select_data()[, input$xaixs], select_data()[, input$yaxis], use = "pairwise"), 3)
  #   paste("Correlation =", r)
  # })


  # Generate a summary of the data (this will show the summary results of all data)----
  output$summary_all <- renderPrint({
    req(input$datafile)
    skimr::skim(select_data())
  })
}  


# Create a Shiny app object
shinyApp(ui = ui, server = server)