options(scipen=999)  # turn-off scientific notation like 1e+48

library(tidyverse)
library(elo)
library(shiny)
library(DT)
library(shinydashboard)
library(shinythemes)
library(shiny)
library(dplyr)
library(tidyr)
library(shinyWidgets)
library(ggplot2)

library(ggcorrplot) # for correlation plot
library(shinycssloaders) # to add a loader while graph is populating
library(ggtext) # beautifying text on top of ggplot
library(maps) # for USA states map - boundaries used by ggplot for mapping
library(plotly) # for data visualization and plots using plotly 
library(highcharter)

df <- read.csv("D:\\LED.csv")
xyTable <- read.csv("D:\\LED.csv")

c2 = df %>% 
  select(-"Country", -"Alcohol") %>% 
  names()



# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  skin = "purple",
  
  dashboardHeader(title="SAKE STATS",titleWidth = 200,
                  tags$li(class="dropdown",tags$a(href="https://www.youtube.com/channel/UCS8Sz16mwVynP5jAxDGBadA", icon("youtube"), "My Channel", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/shahzaib-shafiq-6a0ab9125/" ,icon("linkedin"), "My Profile", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://github.com/Ashario/Sake-Stats.git", icon("github"), "Source Code", target="_blank"))
                  
                  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      
      menuItem(" Main Page",tabName = "t1",icon=icon("home")),
      menuItem(" Display Data",tabName = "t2", icon=icon("database")),
      menuItem(" Country Wise Analysis",tabName = "t3", icon=icon("globe")),
      menuItem(" Histograms",tabName = "t4", icon=icon("chart-simple")),
      menuItem(" Contact Us",tabName = "t5", icon=icon("phone")),
      menuItem(" About",tabName = "t6", icon=icon("circle-info"))

      
    )
    
  ),

  dashboardBody(
    
    tabItems(
      ## First tab item
      tabItem(tabName = "t1", 
              tabBox(id="t1", width = 12, 
                              fluidRow(
                                column(width = 8, tags$img(src="SAKE STATS.png", width =300 , height = 300),
                                       tags$br() ,
                                       tags$a("Sake Stats"), align = "center"),
                                column(width = 4, tags$br() ,
                                       tags$p("SAKE STATS aims on providing its users with updated statistical analysis. We wanted to create a resource that could be dipped into, and was constantly being upgraded based on users' feedback, was reasonably priced, and kept up-to-date with new knowledge and statistics software. This makes Sake Stats a constant work in progress, but it also gives us the chance to become the best statistics resources for students, academics and researchers alike. Feel free to get in touch and let us know what you think.")
                                )
                              )

          )
              
      ),
      ## Second tab item
      tabItem(tabName = "t2", 
              tabBox(id="t2", width = 12, 
                     tabPanel("data", withSpinner(DT::dataTableOutput("mytable")), icon = icon("table")),
                     tabPanel("Structure", verbatimTextOutput("structure"), icon=icon("uncharted")),
                     tabPanel("Summary Stats", verbatimTextOutput("summary"), icon=icon("chart-pie"))
                     
              )
              
      ),
      ## Third tab item
      tabItem(tabName = "t3", 
              fluidPage(
                     selectInput("location", "Select Country", choices = df$Country, selected="Afghanistan"),
                     titlePanel("Population"),
                     withSpinner(plotOutput("box")),
                     titlePanel("Life Expectancy in Age"),
                     withSpinner(plotOutput("scatter")),
                     titlePanel("Country GDP"),
                     withSpinner(plotOutput("scatter2")),
                     titlePanel("Summary"),
                     verbatimTextOutput("CountrySummary")
              )

        ),
      ## Fourth tab item
      tabItem(tabName = "t4", 
              fluidPage(
                titlePanel("Life Expectancy"),
                withSpinner(plotOutput("hist1")),
                titlePanel("Alcohol Consumption per Capita (in litres)"),
                withSpinner(plotOutput("hist2")),
                titlePanel("Body Mass Index of entire population"),
                withSpinner(plotOutput("hist3")),
                titlePanel("government expenditure on health in %"),
                withSpinner(plotOutput("hist4")),
                titlePanel("Adult Mortality"),
                withSpinner(plotOutput("hist5")),
                titlePanel("Population"),
                withSpinner(plotOutput("hist6"))
              )
      ),
      ## Fifth tab item
      tabItem(tabName = "t5", 
              tabBox(id="t4", width = 12,
                     h1("Contact us"),
                     h4("We at Sake Stats aims on providing its users with updated statistical analysis. We wanted to create a resource that could be dipped into, and was constantly being upgraded based on users' feedback, was reasonably priced, and kept up-to-date with new knowledge and statistics software. This makes Sake Stats a constant work in progress, but it also gives us the chance to become the best statistics resources for students, academics and researchers alike. Feel free to get in touch and let us know what you think."),
                     h2(" "),
                     h2("Project Members:"),
                     h4("This project was made by: "),
                     h4("20F-0240---Ashar Zafar (LEADER)"),
                     h4("20F-0317---Shahzaib Shafiq"),
                     h4("20F-0312---Ehsan Akhtar"),
                     h4("20F-0350---Abdul Kabir Khan")
              )
      ),
      ## sixth tab item
      tabItem(tabName = "t6", 
              tabBox(id="t6", width = 12,
                     img(src="SAKE STATS.png", width =250 , height = 250, align = "center"),
                     h1("About us"),
                     h2("Introduction"),
                     h4("We at Sake Stats aims on providing its users with updated statistical analysis. We wanted to create a resource that could be dipped into, and was constantly being upgraded based on users' feedback, was reasonably priced, and kept up-to-date with new knowledge and statistics software. This makes Sake Stats a constant work in progress, but it also gives us the chance to become the best statistics resources for students, academics and researchers alike. Feel free to get in touch and let us know what you think."),
                     h2(" "),
                     h2("Sources for this Project"),
                     h4(
                       
                     tags$div(
                       "Dataset taken from ",
                     tags$a(href="https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who", 
                            "KAGGLE")),
                     tags$div(
                       "Icons taken from ",
                       tags$a(href="https://fontawesome.com/search", 
                              "FONT AWESOME")),
                     tags$div(
                       "Help in code taken from ",
                       tags$a(href="https://stackoverflow.com/", 
                              "Stack Overflow")),
                     tags$div(
                       "Help in front end taken from ",
                       tags$a(href="https://www.geeksforgeeks.org/", 
                              "Geeks for Geeks"))
                     ),
                     h2("Other Sources"),
                     h4("We have several sources from where we collect our data. Some of them include: "),
                     h4("1- Price Statistics"),
                     h4("2- Labour Force Statistics"),
                     h4("3- Demographic Statistics"),
                     h4("4- Business and Communication")
                     
                     
                     
              )
      )
      # tabItem(tabName = "t7",
      #         
      # 
      #         verbatimTextOutput("summary2")
      #         
      #         # box(      selectInput("Aspect","Select Variable", choices = c2, selected="2005", width = 250),
      #         #           withSpinner(plotOutput("map_plot")), width = 12)
      #         # 
      #         
      #         ######################
      #         # fluidPage(
      #         #   sidebarPanel(
      #         #     numericInput("a", "Enter Country Name", 1),
      #         #     numericInput("b", "Enter Year", 1),
      #         #     numericInput("c", "Enter Status", 1),
      #         #     numericInput("d", "Enter Life Expectancy", 1),
      #         #     numericInput("e", "Adult Mortality", 1),
      #         #     numericInput("f", "Enter infant death", 1),
      #         #     numericInput("g", "Enter Alcohol consumption", 1),
      #         #     numericInput("h", "Enter % expediture", 1),
      #         #     numericInput("i", "Enter Hepatitis B", 1),
      #         #     numericInput("j", "Enter Measles", 1),
      #         #     numericInput("k", "Enter BMI", 1),
      #         #     numericInput("l", "Enter under-five deaths", 1),
      #         #     numericInput("m", "Enter Polio", 1),
      #         #     numericInput("n", "Enter Total expenditure", 1),
      #         #     numericInput("o", "Enter Diphtheria", 1),
      #         #     numericInput("p", "Enter HIV/AIDS", 1),
      #         #     numericInput("q", "Enter GDP", 1),
      #         #     numericInput("r", "Enter Population", 1),
      #         #     numericInput("s", "Enter thinness 1-19 years", 1),
      #         #     numericInput("t", "Enter thinness 5-9 years", 1),
      #         #     numericInput("u", "Enter Income composition of resources", 1),
      #         #     numericInput("v", "Enter Schooling", 1),
      #         #     
      #         #     actionButton("add_data", "Add Data", width = "100%")
      #         #   ),
      #         #   mainPanel(
      #         #     tableOutput("xy_Table")
      #         #   )
      #         # )
      # 
      # )
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  # xyTable <- reactiveVal(xyTable)
  # 
  # observeEvent(input$add_data, {
  #   xyTable() %>%
  #     add_row(
  #       Country= input$a,
  #       Year = input$b,
  #       Status= input$c,
  #       Life.expectancy = input$d,
  #       Adult.Mortality= input$e,
  #       infant.deaths= input$f,
  #       Alcohol= input$g,
  #       percentage.expenditure= input$h,
  #       Hepatitis.B= input$i,
  #       Measles= input$j,
  #       BMI= input$k,
  #      # under-five.deaths=input$l,
  #       Polio= input$m,
  #       Total.expenditure= input$n,
  #       Diphtheria= input$o,
  #       HIV/AIDS= input$p,
  #       GDP= input$q,
  #       Population= input$r,
  #       #thinness.119.years= input$s,
  #       #thinness.59.years= input$t,
  #       Income.composition.of.resources= input$u,
  #       Schooling= input$v
  #     ) %>%
  #     xyTable()
  # })
  # 
  #output$xy_Table <- renderTable(xyTable())
  #############
  
  #Filter data
  data <- reactive({
    df %>%
      filter(
        Country %in% input$location
      )
  })
  
  output$table <- renderDataTable(iris)
  
  # #For displaying table
  output$mytable = DT::renderDataTable({
    df
  })
  
  #For Summary Output
  output$summary <- renderPrint({
    df %>% 
      summary()
  })
  
  #For Structure output
  output$structure <- renderPrint({
    df %>% 
      str()
  })
  
  #For box plots
  output$box <- renderPlot({
    req(input$location)
    data() %>%
      ggplot(aes(fill=Country, y=Population, x=Year)) +
      geom_boxplot() +
      geom_jitter(position=position_jitter(0.2))
    
  })
  
  #for scatter plot
  output$scatter <- renderPlot({
    req(input$location)
    data() %>%
      ggplot(aes(fill=Country, y=Life.expectancy, x=Year)) +
      geom_point() +
      stat_smooth()
    
  })
  
  #for scatter plot
  output$scatter2 <- renderPlot({
    req(input$location)
    data() %>%
      ggplot(aes(fill=Country, y=GDP, x=Year)) +
      geom_point() +
      geom_smooth(method=lm, se=FALSE)
     
    
  })
  
  #For Country Summary Output
  output$CountrySummary <- renderPrint({
    req(input$location)
    #data() %>%
      summary(data())
  })
  
  ##############################
  
  #for life expectancy histogram
  output$hist1 <- renderPlot({
    ggplot(df, aes(x=Life.expectancy)) + 
      geom_histogram()
  })
  
  #for Alcohol consumption histogram
  output$hist2 <- renderPlot({
    ggplot(df, aes(x=Alcohol)) + 
      geom_histogram()
  })
  
  #for Body Mass Index of entire population histogram
  output$hist3 <- renderPlot({
    ggplot(df, aes(x=BMI)) + 
      geom_histogram()
  })
  
  #for expenditure histogram
  output$hist4 <- renderPlot({
    ggplot(df, aes(x=Total.expenditure)) + 
      geom_histogram()
  })
  
  #for adult mortality histogram
  output$hist5 <- renderPlot({
    ggplot(df, aes(x=Adult.Mortality)) + 
      geom_histogram()
  })
  
  #for population histogram
  output$hist6 <- renderPlot({
    ggplot(df, aes(x=Population)) + 
      geom_histogram()
  })
  
  #######################

}

# Run the application 
shinyApp(ui = ui, server = server)

