library(tidyverse)
library(stringr)
library(ggplot2)
library(kableExtra)
library(rebus)
library(lubridate)
library(knitr)
library(haven)
library(scales)
library(fs)
library(janitor)
library(moderndive)
library(pspearman)
library(ggpubr)
library(shiny)
library(plotly)
library(scales)

age <- read_rds("age_variable.rds") 
educ <- read_rds("educ_variable.rds") 
race <- read_rds("race_variable.rds")

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("The Relationship between Demographics and Predicted Republican Advantange in the 2018
             Mid-Term Elections"),
  
  # Create 3 seperate tabs to enable the user to explore the data by age,
  # race or education.
  # Allow the user to select between the winning parties, the size of the
  # points on the scatterplot and whether or not a regression line appears
  # for each tab. 
  # In the mainPanel, print the source of the data and create a scatterplot
  # for each variable. 
  tabsetPanel(
    tabPanel(
      title = "Explore Polling Data by Age",
      sidebarPanel(width = 3,
                   selectizeInput("age", 
                                  label = "Select to view final winning party in each district:",
                                  choices = c("Democrat", "Republican", "Undecided"),
                                  multiple = TRUE),
                   sliderInput("size_a", "Select Point Size:",
                               value = 1.5, min = 1, max = 3),
                   checkboxInput("age_lm", label = "Display Regression Line")
      ),
      mainPanel(
        plotOutput("age_scatterplot", width = 1000, height = 600),
        hr(),
        helpText("Prediced data from NYT Upshot/Sienna polls and actual data taken from Mr. Schroeder on Piazza"),
        paste0("Application by Ghada Amer and Cayanne Chachati")
      )
    ),
    tabPanel(
      title = "Explore Polling Data by Education",
      sidebarPanel(width = 3, 
                   selectizeInput("educ", 
                                  label = "Select to view final winning party in each district:",
                                  choices = c("Democrat", "Republican", "Undecided"),
                                  multiple = TRUE),
                   sliderInput("size_b", "Select Point Size:",
                               value = 1.5, min = 1, max = 3),
                   checkboxInput("educ_lm", label = "Display Regression Line")
                   
      ),
      mainPanel(
        plotOutput("educ_scatterplot", width = 1000, height = 600),
        hr(),
        helpText("Prediced data from NYT Upshot/Sienna polls and actual data taken from Mr. Schroeder on Piazza"),
        paste0("Application by Ghada Amer and Cayanne Chachati")
      )
    ),
    tabPanel(
      title = "Explore Polling Data by Race",
      sidebarPanel(width = 3, 
                   selectizeInput("race", 
                                  label = "Select to view final winning party in each district:",
                                  choices = c("Democrat", "Republican", "Undecided"),
                                  multiple = TRUE),
                   sliderInput("size_c", "Select Point Size:",
                               value = 1.5, min = 1, max = 3),
                   checkboxInput("race_lm", label = "Display Regression Line")
                   
      ),
      mainPanel(
        plotOutput("race_scatterplot", width = 1000, height = 600),
        hr(),
        helpText("Prediced data from NYT Upshot/Sienna polls and actual data taken from Mr. Schroeder on Piazza"),
        paste0("Application by Ghada Amer and Cayanne Chachati")
      )
    )
  )
  )

# Define server logic
server <- function(input, output) {
  
  # Create a reactive variable that will filter the data in the age tab 
  # depending on the districts that the user selects. 
  age2 <- reactive({
    age_r <- age
    age_r <- subset(
      age_r
    )
    if (req(input$age) == "Democrat") {
      age_r <- subset(
        age_r,
        win_party %in% c(req(input$age))
      )
    }
    if (req(input$age) == "Republican") {
      age_r <- subset(
        age_r,
        win_party %in% c(req(input$age))
      )
    }
    age_r <- subset(
      age_r,
      win_party %in% c(req(input$age))
    )
  })
  
  # Create a scatterplot that enables the user to examine the relationship
  # between the different age categories and the predicted republican advantage. 
  # Place percentage of respondents on the x axis, predicted republican
  # advantage on the y axis, use color to identify winning party 
  # and facet_wrap to isolate the different age categories. 
  output$age_scatterplot <- renderPlot({
    
    plot_age <- 
      age2() %>%
      ggplot(aes(x = percentage, y = adv_rep)) + 
      geom_jitter(aes(color = win_party), size = input$size_a) +
      facet_wrap(~ager, scales = "free", shrink = TRUE) +
      theme_light() + 
      labs(title = "Percentage of Respondents per Age Group vs. Predicted Republican Advantage",
           subtitle = "At a 95% confidence interval, there is a negative correlation between predicted republican
advantage and the age category of 18 to 34, and a positive correlation with the age 
category of 65 and older. This relationship is observed when all districts are displayed.",
           x ="Percentage of Respondents (%)",
           y = "Predicted Republican Advantage (%)",
           color = "Winning Party") + 
      theme(title = element_text(size = 14, face = "bold"),
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(color = "white", size = 12 , face = "bold"),
            legend.text = element_text(size = 12, colour = "black"),
            legend.title = element_text(face = "bold", size = 12),
            legend.justification = c("right", "top"),
            legend.position = "left",
            panel.spacing = unit(2, "lines"), 
            axis.title = element_text(size = 14)) +
      scale_color_manual("Winning Party", values = c("Democrat" = "blue", 
                                                     "Republican" = "red", 
                                                     "Undecided" = "green"))
    
    # Create a function that will add a line of regression and print the correlation coefficient
    # and p value if the user chooses to do so in the checkboxInput of the UI. 
    if (input$age_lm == TRUE) {
      plot_age + 
        geom_smooth(method = lm, se= FALSE, color = "black", size = 0.75, fullrange = TRUE, linetype = "dashed") +
        stat_cor(method = "pearson", label.x = 7, label.y = 15, size = 4, output.type = "text")
    }
    
    else{
      plot_age
    }
  })
  
  # Repeat the same steps as above for the education tab. 
  educ2 <- reactive({
    educ_r <- educ
    educ_r <- subset(
      educ_r
    )
    if (req(input$educ) == "Democrat") {
      educ_r <- subset(
        educ_r,
        win_party %in% c(req(input$educ))
      )
    }
    if (req(input$educ) == "Republican") {
      educ_r <- subset(
        educ_r,
        win_party %in% c(req(input$educ))
      )
    }
    educ_r <- subset(
      educ_r,
      win_party %in% c(req(input$educ))
    )
  })
  
  output$educ_scatterplot <- renderPlot({
    
    plot_educ <- 
      educ2() %>%
      ggplot(aes(x = percentage, y = adv_rep)) + 
      geom_jitter(aes(color = win_party), size = input$size_b) +
      facet_wrap(~educ, scales = "free", shrink = TRUE, nrow = 3) +
      theme_light() +
      theme(title = element_text(size = 14, face = "bold"),
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(color = "white", size = 12 , face = "bold"),
            legend.text = element_text(size = 12, colour = "black"),
            legend.title = element_text(face = "bold", size = 12),
            legend.justification = c("right", "top"),
            legend.position = "left",
            panel.spacing = unit(2, "lines"),
            axis.title = element_text(size = 14)) + 
      labs(title = "Percentage of Respondents per Educational Level vs. Predicted Republican Advantage",
           subtitle = "At a 95% confidence interval,there is a negative relationship between predicted republican advantage
and educational level at or above a Bachelor's degree, and positive relationship at or below 
Some college or trade school. This general trend varies slightly depending on the districts selected.",
           x = "Percentage of Respondents (%)",
           y = "Predicted Republican Advantage (%)",
           color = "Winning Party") + 
      scale_color_manual("Winning Party", values = c("Democrat" = " blue", 
                                                     "Republican" = "red", 
                                                     "Undecided" = "green"))
    
    if (input$educ_lm == TRUE) {
      plot_educ + 
        geom_smooth(method = lm, se= FALSE, color = "black", size = 0.75, fullrange = TRUE, linetype = "dashed") +
        stat_cor(method = "pearson", label.x = 0, label.y = 11, size = 4, output.type = "text")
    }
    
    else{
      plot_educ
    }
  })
  
  # Repeat the same steps as above for the race tab. 
  race2 <- reactive({
    race_r <- race
    race_r <- subset(
      race_r
    )
    if (req(input$race) == "Democrat") {
      race_r <- subset(
        race_r,
        win_party %in% c(req(input$race))
      )
    }
    if (req(input$race) == "Republican") {
      race_r <- subset(
        race_r,
        win_party %in% c(req(input$race))
      )
    }
    race_r <- subset(
      race_r,
      win_party %in% c(req(input$race))
    )
  })
  
  output$race_scatterplot <- renderPlot({
    
    plot_race <- 
      race2() %>%
      ggplot(aes(x = percentage, y = adv_rep)) + 
      geom_jitter(aes(color = win_party), size = input$size_c) +
      facet_wrap(~race_eth, scales = "free", shrink = TRUE, nrow = 3) +
      theme_light() + 
      labs(title = "Percentage of Respondents per Racial Group vs. Predicted Republican Advantage",
           subtitle = "Overall, at a 95% confidence interval, a consistent relationship between 
racial group and predicted republican advantage does not exist. ",
           x = "Percentage of Respondents (%)",
           y = "Predicted Republican Advantage (%)",
           color = "Winning Party") + 
      theme(title = element_text(size = 14, face = "bold"),
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(color = "white", size = 12 , face = "bold"),
            legend.text = element_text(size = 12, colour = "black"),
            legend.title = element_text(face = "bold", size = 12),
            legend.justification = c("right", "top"),
            legend.position = "left", legend.box = "horizontal",
            panel.spacing = unit(2, "lines"),
            axis.title = element_text(size = 14)) +
      scale_color_manual("Winning Party", values = c("Democrat" = "blue", 
                                                     "Republican" = "red", 
                                                     "Undecided" = " green"))
    
    if (input$race_lm == TRUE) {
      plot_race + 
        geom_smooth(method = lm, se= FALSE, color = "black", size = 0.75, fullrange = TRUE, linetype = "dashed" ) +
        stat_cor(method = "pearson", label.x = 0, label.y = 12, size = 4, output.type = "text")
    }
    
    else{
      plot_race
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)