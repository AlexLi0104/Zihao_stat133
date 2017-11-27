
# Load packages and source functions
library(shiny)
library(ggvis)
library(dplyr)


# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Grade Visualizer"),
  
  # Sidebar with different widgets depending on the selected tab
  sidebarLayout(
    sidebarPanel(
      
      conditionalPanel(condition = "input.tabselected==1",
                       h3("Grades Distribution"),
                       tableOutput('table')
      ),
      
      conditionalPanel(condition = "input.tabselected==2",
          selectInput("select", label = h4("X-axis variable"), 
              choices = list("HW1" = "HW1", 
                             "HW2" = "HW2",
                             "HW3" = "HW3",
                             "HW4" = "HW4",
                             "HW5" = "HW5",
                             "HW6" = "HW6",
                             "HW7" = "HW7",
                             "HW8" = "HW8",
                             "HW9" = "HW9",
                             "QZ1" = "QZ1",
                             "QZ2" = "QZ2",
                             "QZ3" = "QZ3",
                             "QZ4" = "QZ4",
                             "ATT" = "ATT",
                             "Test1" = "Test1",
                             "Test2" = "Test2",
                             "EX1" = "EX1",
                             "EX2" = "EX2",
                             "Homework" = "Homework",
                             "Quiz" = "Quiz",
                             "Lab" = "Lab",
                             "Overall" = "Overall"), 
                selected = "HW1"),
           sliderInput("width2",
                       "Bin Width",
                       min = 1,
                       max = 10,
                       value = 10)
                       
      ),
      
      conditionalPanel(condition = "input.tabselected==3",
           selectInput("select1", label = h4("X-axis variable"), 
                choices = list("HW1" = "HW1", 
                               "HW2" = "HW2",
                               "HW3" = "HW3",
                               "HW4" = "HW4",
                               "HW5" = "HW5",
                               "HW6" = "HW6",
                               "HW7" = "HW7",
                               "HW8" = "HW8",
                               "HW9" = "HW9",
                               "QZ1" = "QZ1",
                               "QZ2" = "QZ2",
                               "QZ3" = "QZ3",
                               "QZ4" = "QZ4",
                               "ATT" = "ATT",
                               "Test1" = "Test1",
                               "Test2" = "Test2",
                               "Lab" = "Lab",
                               "EX1" = "EX1",
                               "EX2" = "EX2",
                               "Homework" = "Homework",
                               "Quiz" = "Quiz",
                               "Overall" = "Overall"), 
                selected = "Test1"),
            selectInput("select2", label = h4("Y-axis variable"), 
                choices = list("HW1" = "HW1", 
                               "HW2" = "HW2",
                               "HW3" = "HW3",
                               "HW4" = "HW4",
                               "HW5" = "HW5",
                               "HW6" = "HW6",
                               "HW7" = "HW7",
                               "HW8" = "HW8",
                               "HW9" = "HW9",
                               "QZ1" = "QZ1",
                               "QZ2" = "QZ2",
                               "QZ3" = "QZ3",
                               "QZ4" = "QZ4",
                               "ATT" = "ATT",
                               "Test1" = "Test1",
                               "Test2" = "Test2",
                               "Lab" = "Lab",
                               "EX1" = "EX1",
                               "EX2" = "EX2",
                               "Homework" = "Homework",
                               "Quiz" = "Quiz",
                               "Overall" = "Overall"), 
                selected = "Overall"),
            sliderInput("opacity",
                        "Opacity",
                        min = 0,
                        max = 1,
                        value = 0.5),
            radioButtons("scale", label = h4("Show line"),
                 choices = list("none" = 1, 
                                "lm" = 2, 
                                "loess" = 3), 
                 selected = 1)
                       
      )
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
          tabPanel("Barchart", value = 1, 
                ggvisOutput("barchart")),
          tabPanel("Histogram", value = 2, 
                ggvisOutput("histogram"),
                h5("Summary Statistics"),
                verbatimTextOutput("summary")),
           tabPanel("Scatterplot", value = 3, 
                ggvisOutput("scatterplot"),
                h5("Correlation:"),
                verbatimTextOutput("correlation")),
           id = "tabselected")
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # load the cleanscores file and functions
  source('../code/functions.R')
  cleanscores <- read.csv('../data/cleandata/cleanscores.csv')
  
  # Barchart (for 1st tab)
  h <- setNames(data.frame(table(cleanscores$Grade)), c('Grade', 'Freq'))
  h$Prop <- round(h$Freq/sum(h$Freq), digits = 2)

  target <- c('A+','A', 'A-','B+', 'B', 'B-','C+', 'C', 'C-', 'D', 'F')
  h <- h[match(target, h$Grade), ]
  
  output$table <- renderTable(h)
  
  vis_barchart <- reactive({
    
    barchart <- cleanscores %>%
      ggvis(x = ~Grade) %>%
      layer_bars(stroke := 'white', fill:= "629FED",
                 opacity := 0.8) %>%
      scale_ordinal('x', domain = 
          c('A+', 'A', 'A-','B+', 'B', 'B-','C+', 'C', 'C-', 'D', 'F')) %>%
      add_axis('y', title = 'Frequency')
  })
  
  vis_barchart %>% bind_shiny("barchart")
  
  
  # Histogram (for 2nd tab)
  vis_histogram <- reactive({
    
    xvar <- prop("x", as.symbol(input$select))
    
    histogram <- cleanscores %>%
      ggvis(x = xvar) %>%
      layer_histograms(stroke := 'white', width = input$width2, fill:= "gray",
                       opacity := 0.8)
  })
  
  vis_histogram %>% bind_shiny("histogram")
  
  print_stats1 <- function(x) {
    c1 <- names(x)
    c1 <- sprintf("%-9s", c1)
    c2 <- rep(":", length(x))
    a <- sapply(x, function(x) x[[1]])
    for (i in 1:length(x)) {
      cat(paste(c1[i], c2[i], ' ', sprintf("%.4f", a[i]), '\n', sep = ''))
    }
  }
  
  output$summary <- renderPrint({
    print_stats1(summary_stats(cleanscores[,input$select]))
  })
  
  ## Scatterplot (for 3rd tab)
  vis_scatterplot <- reactive({
    
    xvar <- prop("x", as.symbol(input$select1))
    yvar <- prop("y", as.symbol(input$select2))
    
    if (input$scale == 1) {
      scatter <- cleanscores %>%
        ggvis(x = xvar, y = yvar) %>%
        layer_points(stroke := 'gray', opacity := input$opacity, size := 30)
    }
    else if (input$scale == 2) {
      scatter <- cleanscores %>%
        ggvis(x = xvar, y = yvar) %>%
        layer_points(stroke := 'gray', opacity := input$opacity, size := 30) %>%
        layer_model_predictions(model = 'lm', stroke := 'red')
    }
    else if (input$scale == 3) {
      scatter <- cleanscores %>%
        ggvis(x = xvar, y = yvar) %>%
        layer_points(stroke := 'gray', opacity := input$opacity, size := 30) %>%
        layer_model_predictions(model = 'loess', stroke := 'red' )
    }
  })
  
  vis_scatterplot %>% bind_shiny("scatterplot")
  
  output$correlation <- renderPrint({
    a <- cor(cleanscores[,input$select1], 
             cleanscores[,input$select2])
    cat(a)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

