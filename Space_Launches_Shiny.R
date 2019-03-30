library(tidyverse)
library(shiny)
library(DT)

launches <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/launches.csv")

#Doing some initial modifications to the dataset, e.g renaming columns, separating "launch_date" column and fixing one type in the "Year" column

launches <- launches %>%
    rename(country_code = state_code,
           "Vehicle Types" = type,
           Mission = mission,
           Agency = agency) %>%
    separate(launch_date, c("Year", "Month", "Day"), sep = "-") %>%
    mutate(Year = ifelse(Year == 2918, 2018, as.numeric(Year))) %>%
    select(-launch_year)

ui <- fluidPage(
  titlePanel("Space Launches"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Allows user to select a range of years if they want to "zoom in" on the data
      sliderInput(inputId = "year.range", 
                  label = "Year Range",
                  min = min(launches$Year, na.rm = TRUE), 
                  max = max(launches$Year, na.rm = TRUE), 
                  value = c(1957, 2018)),
      
      # Allows user to select a country code to view frequency of space launches over the years by country
      # Only the top 7 countries are included here as they have the most significant number
      selectInput(inputId = "country",
                  label = "Country Code",
                  choices = c("F", "US", "RU", "CN", "I", "IN", "J")),
      
      # Allows user to choose a category to view space launches by
      radioButtons(inputId = "category",
                   label = "Space launches by:",
                   choices = c("Country Code" = "country_code", "Agency"),
                   selected = "country_code"),
      
      # Allows user to view the full list of vehicle types and missions in the space launches
      radioButtons(inputId = "info",
                   label = "List of:",
                   choices = c("Vehicle Types", "Mission"),
                   selected = "Vehicle Types"),
      
      #Allows user to view monthly frequency of space launches for a specific year
      selectInput(inputId = "year",
                  label = "Select year to view seasonal trend:",
                  choices = sort(unique(launches$Year)))
      ),
    mainPanel(
      
      # Multiple tabs to make the main panel more tidy instead of displaying all graphs in one page
      tabsetPanel(type = "pills",
                  tabPanel("General Activity", plotOutput(outputId = "summary")),
                  tabPanel("Countries", plotOutput(outputId = "countries")),
                  tabPanel("Categories", plotOutput(outputId = "category")),
                  tabPanel("Vehicle/mission info", dataTableOutput(outputId = "infotable")),
                  tabPanel("Seasons", plotOutput(outputId = "season")),
                  tabPanel("Agency types", plotOutput(outputId = "agencytypes"))
      )
    )
  )
)


server <- function(input, output){
          
  output$summary <- renderPlot({
    
    launches %>%
      drop_na() %>%
      group_by(Year) %>%
      summarise(count = n()) %>% 
      filter(Year >= input$year.range[1],    
             Year <= input$year.range[2]) %>%
      ggplot(aes(x = factor(Year), y = count, group = 1)) +
      geom_line() +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 20, face = "bold")) +
      labs(x = "Year",
           y = "Number of launches",
           title = "Space launches over the years from all countries")
})
  
  
  output$agencytypes <- renderPlot({
    
    launches %>%
      group_by(agency_type) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      mutate(percentage = floor(count/sum(count)*100)) %>%
      ggplot(aes(x = reorder(agency_type, - count), y = count, fill = agency_type)) +
        geom_col() +
        geom_text(aes(label = paste0(percentage, "%")), nudge_y = 150) +
        theme_classic() +
        theme(panel.grid = element_blank(),
            legend.position = "none",
            plot.title = element_text(size = 20, face = "bold")) +
        labs(x = "Agency type",
            y = "Number of launches",
            fill = "Agency type",
            title = "Space launches by type of agency")
  })
  
  
  output$countries <- renderPlot({
    
    launches %>%
      drop_na() %>%
      filter(country_code == input$country) %>%
      group_by(Year) %>%
      summarise(count = n()) %>%
      filter(Year >= input$year.range[1],
             Year <= input$year.range[2]) %>%
      ggplot(aes(x = factor(Year), y = count, group = 1)) +
      geom_line() +
      theme_classic() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 20, face = "bold")) +
      labs(x = "Year",
           y = "Number of launches",
           title = "Space launches over the years in leading countries")
  })
  
  output$category <- renderPlot({
    
    x <- sym(input$category)

    launches %>%
      mutate("Country Code" = country_code) %>%
      group_by(!!x) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      drop_na() %>%
      ggplot(aes(x = reorder(!!x, -count), y = count, fill = !!x)) +
        geom_col() +
        theme_classic() +
        theme(panel.grid = element_blank(),
              axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
              legend.position = "none",
              plot.title = element_text(size = 20, face = "bold")) +
        labs(x = input$category,
             y = "Number of launches")
    
  })

  
  output$infotable <- renderDataTable({
   
   launches %>%
        distinct(!!sym(input$info), Year) %>%
        datatable(options = list(pageLength = 15))
    
  })
  
  output$season <- renderPlot({
    
    launches %>%
        filter(Year == input$year) %>%
        group_by(Month) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        ggplot(aes(x = factor(Month), y = count, group = 1)) +
        geom_line(na.rm = TRUE) +
        theme_light() +
        theme(plot.title = element_text(size = 20, face = "bold")) +
        labs(x = "Month",
             y = "Number of launches",
             title = "Seasonal trend in space launches")
  })
  
}

shinyApp(ui = ui, server = server)
