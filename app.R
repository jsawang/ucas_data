library(shiny)
library(tidyverse)
library(plotly)

apps_002_1 <- read_csv("data/EOC_data_resource_2021_002_1.csv",
                       skip = 15, col_select = c("Year",
                                                 "Age group",
                                                 "Individual age",
                                                 "Domicile",
                                                 "Gender",
                                                 "Applicant type",
                                                 "Applicants"  ))
apps_agegp <- apps_002_1[apps_002_1$Domicile == "All" &
                           apps_002_1$`Applicant type` == "All" &
                           apps_002_1$`Individual age` == "All" &
                           apps_002_1$`Age group` != "All",]

apps_indage <- apps_002_1[apps_002_1$Domicile == "All" &
                            apps_002_1$Gender != "All" &
                            apps_002_1$`Applicant type` == "All" &
                            apps_002_1$`Individual age` != "All" &
                            apps_002_1$`Age group` != "All",]
apps_indage$`Individual age` <- as.factor(apps_indage$`Individual age`)
apps_indage <- apps_indage %>%
  select(everything()) %>%
  mutate(
    Gender_selector = paste(Gender, "All")
  )

ui <- fluidPage(
  titlePanel("UCAS End of Cycle 2021 data explorer"),
  fluidRow(
    column(5,
           plotlyOutput("byAge")),
    column(5,
           plotlyOutput("byIndAge")),
    column(2,
      selectInput("gender","Gender",c("All","Men","Women"))),
  
    
  )
)

server <- function(input, output){
  ageData <- reactiveValues(data = data.frame(), name = "empty")
  indAgeData <- reactiveValues(data = data.frame(), name = "empty")
  
  observeEvent(input$gender, 
               {filterGender <- input$gender
               
               ageData$data <- apps_agegp %>% 
                 filter(Gender == filterGender)
               indAgeData$data <- apps_indage %>%
                 filter(grepl(filterGender, Gender_selector))}
               )
  
  output$byAge <- renderPlotly({
    ggplot(data = ageData$data,
           aes(x = Year, 
               y = Applicants, 
               color = `Age group`)
    ) +
      geom_line() +
      scale_y_continuous(labels = scales::number) +
      scale_x_continuous(breaks=seq(2006,2022,1)) +
      guides(x = guide_axis(n.dodge = 2)) +
      labs(title = "Applicants by age")
  })
  
  output$byIndAge <- renderPlotly({
    ggplot(data = indAgeData$data,
           aes(y = `Individual age`,
               x = Applicants,
               fill = Gender)
    ) +
      geom_bar(stat = 'identity', position = 'dodge') +
      labs(title = "Applicants by individual age and gender") +
      scale_y_discrete(limits = rev(levels(apps_indage$`Individual age`))) +
      scale_x_continuous(labels = scales::number)
  })
  
}

shinyApp(ui = ui, server = server)