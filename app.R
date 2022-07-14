library(shiny)
library(tidyverse)

apps_agegp <- read_csv("data/EOC_data_resource_2021_002_1.csv",
                       skip = 15, col_select = c("Year",
                                                 "Age group",
                                                 "Individual age",
                                                 "Domicile",
                                                 "Gender",
                                                 "Applicant type",
                                                 "Applicants"  ))
apps_agegp <- apps_agegp[apps_agegp$Domicile == "All" &
                           apps_agegp$`Applicant type` == "All" &
                           apps_agegp$`Individual age` == "All" &
                           apps_agegp$`Age group` != "All",]

ui <- fluidPage(
  selectInput("gender","Gender",c("All","Men","Women")),
  
  plotOutput("byAge")
)

server <- function(input, output){
  ageData <- reactiveValues(data = data.frame(), name = "empty")
  
  observeEvent(input$gender, 
               {filterGender <- input$gender
               
               ageData$data <- apps_agegp %>% 
                 filter(Gender == filterGender)}
               )
  
  output$byAge <- renderPlot({
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
  
}

shinyApp(ui = ui, server = server)