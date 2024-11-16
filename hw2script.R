#reading packages
library(tidyverse)
library(dplyr)
library(shiny)
library(tidyr)
library(ggplot2)
library(plotly)

#--------------------------------------------------------------------------
#reading data
dataset = read.csv("https://uwmadison.box.com/shared/static/vj7eyp9lk3ofjpi8lkygjga6h0gmyegz.csv")

data <- dataset %>% pivot_longer(cols = c(Smoking_Prevalence, Drug_Experimentation), names_to = "Behavior", values_to = "Rates")

#-----------------------------------------------------------------------
#declaring cols
behavior <- unique(data$Behavior)
factors <- c("Peer_Influence", "Family_Background", "Mental_Health", 
             "Parental_Supervision", "Community_Support", "Media_Influence")
awareness_factors <- c("School_Programs", "Access_to_Counseling", "Substance_Education")
years <- unique(data$Year)
socio_colors <- c("High" = "black", "Medium" = "grey35", "Low" = "grey70")

#-----------------------------------------------------------------------
behavior_lineplot <- function(df, facet = FALSE) {
  plot <- ggplot(df,  aes(x =Year, y=avg_rate, color = Gender, group =Gender) ) +
    geom_line(size = 0.7) +
    geom_point(size = 1) +
    labs(title = "Trends in Youth Substance Abuse by Year",
         x = "Year", 
         y = "Average Rate") +
    scale_color_manual(values = c("Female" = "magenta", "Male" = "blue", "Both" = "gray35")) +
    theme_minimal()
  
  if (facet){plot <- plot + facet_wrap(~ Age_Group)}
  
  plot
}

factor_scatterplot <- function(df, selected_factors) {
  df <- df %>%
    pivot_longer(cols=all_of(selected_factors), names_to="Factor",
                 values_to = "Value") %>%
    group_by(Year,Factor)%>%
    summarise(avg_value= mean(Value,na.rm = TRUE), .groups='drop')
  
  ggplot(df, aes(x =Year, y= avg_value, color=Factor)) +
    geom_point(size=2) +geom_line() +
    labs(title = "Scatter Plot of Average Factors by Years", 
         x = "Year", 
         y = "Average Factor Influence") +theme_minimal()
}

smoking_drug_scatterplot <- function(df_year, socio_colors, year) {
  ggplot(df_year, 
         aes(x = Smoking_Prevalence, y=Drug_Experimentation,
             color=Socioeconomic_Status ) ) +
    geom_jitter(width = 0.25, height =  0.25, size=1, alpha=0.6) +
    scale_color_manual(values = socio_colors) +
    labs(title = paste("Scatter Plot for Year:", year),
         x = "Smoking Prevalence",
         y = "Drug Experimentation",
         color = "Socioeconomic Status") +
    theme_minimal() 
}

#------------------------------------------------------------------

#UI
ui <- fluidPage(
  titlePanel("Youth Substance Abuse Analysis"),
  p("In this application, we analyze the Youth Smoking and Drug Dataset from 
    kaggle, focusing on smoking and drug experimentation across years, gender, socioeconomic status, and various factors that impact substance abuse."),
  
  wellPanel(
    h3("Compare Substance Usage Trends Across Years by Gender"),
    p("Select a behavior to observe trends over years. Use 'Compare Age Groups' to compare plots by age group."),
    sidebarLayout(
      sidebarPanel(
        selectInput("behavior","Select Behaviors:", choices = behavior),
        checkboxInput("facet_age", "Compare Age Groups", value = FALSE)),
      mainPanel(plotOutput("behavior_plot") )),
    
    wellPanel(
      h3("Analyze Trends in Impacting Factors Over Years"),
      p("Select factors to observe how they influence substance use over years."),
      sidebarLayout(sidebarPanel(
        checkboxGroupInput("factors", "Select Factors:", factors, selected = factors)  ),
        mainPanel( plotOutput("factors_plot")) )),
    
    wellPanel(
      h3("Scatter Plot of Smoking vs. Drug Experimentation"),
      p("Select a year and awareness factors to filter the scatter plot. Brush over 
        the plot to see selected points and their factor values in the table below."),
      sidebarLayout(
        sidebarPanel(
          selectInput("year", "Select Year:", choices = years, selected = max(years)),
          checkboxGroupInput("awareness_factors", "Filter by Awareness Factors (Yes):", awareness_factors),
          checkboxGroupInput("factors2", "Select Factors for Display in Table:", factors, selected = factors)
        ),
        mainPanel( plotOutput("scatter_plot", brush = brushOpts(id = "plot_brush", direction = "xy")),
                   tableOutput("table"))))))

#------------------------------------------------------------------------------
#Server
server <- function(input, output) {
  
  filtered_data_beh <-reactive({
    req(input$behavior)
    data %>%filter(Behavior %in% input$behavior) %>% group_by(Year, Behavior,
                                                              Gender, Age_Group) %>%
      summarise(avg_rate = mean(Rates, na.rm = TRUE),
                .groups = 'drop')
  })
  
  filtered_data_year <- reactive({ req(input$year )
    
    df_year <- dataset %>%filter(Year == input$year)
    
    if ("School_Programs" %in% input$awareness_factors) {
      df_year <- df_year[df_year$School_Programs =="Yes", ]}
    if ("Access_to_Counseling" %in% input$awareness_factors) {
      df_year <- df_year[df_year$Access_to_Counseling =="Yes", ]}
    if ("Substance_Education" %in% input$awareness_factors) {
      df_year <- df_year[df_year$Substance_Education== "Yes", ] }
    df_year })
  
  
  output$behavior_plot <- renderPlot({
    
    
    df <- filtered_data_beh()
    
    if (input$facet_age){
      plot <- behavior_lineplot(df, facet = TRUE)
    } else {
      df_no_facet <- df %>% select(-Age_Group) %>%group_by(Year, 
                                            Behavior, Gender) %>%
        summarise(avg_rate = mean(avg_rate, na.rm = TRUE), .groups = 'drop')
      
      
      plot <- behavior_lineplot(df_no_facet, facet = FALSE)
    }
    
    plot
  })
  
  output$factors_plot <- renderPlot({
    req(input$factors)
    plot <- factor_scatterplot(data, input$factors)
    
    plot
  })
  
  output$scatter_plot <- renderPlot({
    df_year <- filtered_data_year()
    plot <- smoking_drug_scatterplot(df_year, socio_colors, input$year)
    
    plot
  })
  
  output$table <- renderTable({
    brush <- input$plot_brush
    brushed_data <- brushedPoints(dataset, brush, xvar = "Smoking_Prevalence", yvar = "Drug_Experimentation")
    selected_cols <- c("Year", "Smoking_Prevalence", "Drug_Experimentation", input$factors2)
    
    brushed_data %>%
      select(all_of(selected_cols))
  })
}

#---------------------------------------------------------------------------------------
#run app
shinyApp(ui, server)
