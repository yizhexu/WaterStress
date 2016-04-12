shinyUI(
  
  fluidPage(
    
    theme = 'styles.css',
    
    # Application title
    h2("Water Stress Indicator"),
    
    withMathJax(includeMarkdown("./www/desc-general.md")),
    
    tabsetPanel(type = "pills", position = "right", selected = "Data explorer",
                
                tabPanel(title = "Data explorer", 
                         
                         # buttons on top
                         h3("Query Data"),
                         
                         includeMarkdown("./www/desc-token.md"),
                         
                         br(),
                         
                         fluidRow(
                           
                           column(3, 
                                  h4("aWhere Login"), 
                                  textInput(inputId ="api_key", label = "Username", value = "Your Email"),
                                  passwordInput(inputId = "api_code", label = "Password"), 
                                  uiOutput("link"), 
                                  h4("Set Years"), 
                                  numericInput(inputId = "baseline_year", label = "Baseline Year", value = 2014, min = 2008, max = this_year),
                                  numericInput(inputId = "target_year", label = "Target Year", value = 2015, min = 2008, max = this_year),
                                  h4("Access API"),
                                  actionButton(inputId = "fetchdata", label = "Access API")
                           ),
                           column(3, 
                                  h4("Locations"),
                                  numericInput(inputId = "lat", label = "Latitude", value = 0.3138389, min = -90, max = 90, step = NA),
                                  numericInput(inputId = "lng", label = "Longitude", value = 32.5991254, min = -180, max = 180, step = NA),
                                  br(), 
                                  h4("Set Dates"),
                                  dateInput(inputId = "start_date", label = "Start Date (MM-DD)", value = "2015-03-01", format = "mm-dd"), 
                                  dateInput(inputId = "end_date", label = "End Date (MM-DD)", value = "2015-07-15", format = "mm-dd")
                           ), 
                           column(6, 
                                  leafletOutput("map", height = 500)
                           )
                         ), 
                         
                         br(),
                         
                         h3("View Data"),
                         
                         includeMarkdown("./www/desc-view.md"),
                         
                         br(),
                         fluidRow(
                           column(12, 
                                  DT::dataTableOutput("tbl_a"))
                         )
                ), 
                tabPanel(title = "Interactive maps", 
                         fluidRow(
                           column(3, 
                                  h4("Generate Map"),
                                  actionButton(inputId = "generatemap", label = "Generate Map")
                           )
                         ),
                         fluidRow(
                           column(4, 
                                  uiOutput("baseline_sub"),
                                  leafletOutput("baseline", height = 500)
                           ), 
                           column(4, 
                                  uiOutput("target_sub"),
                                  leafletOutput("target", height = 500)
                           ), 
                           column(4, 
                                  uiOutput("diff_sub"),
                                  leafletOutput("diff", height = 500)
                           )
                         )  
                )
    )
  )
)