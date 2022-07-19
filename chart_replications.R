library(tidyverse)

apps_agegp <- read_csv("data/EOC_data_resource_2021_002_1.csv",
                       skip = 15, col_select = c("Year",
                                                 "Age group",
                                                 "Individual age",
                                                 "Domicile",
                                                 "Gender",
                                                 "Applicant type",
                                                 "Applicants"  ))

filter_apptype <- "All"
filter_gender <- "All"
filter_domicile <- "All"
filter_agegp <- "All"
filter_indivage <- "All"

apps_filter <- apps_agegp[apps_agegp$Domicile == filter_apptype &
                            apps_agegp$Gender == filter_gender &
                            apps_agegp$`Applicant type` == filter_apptype &
                            apps_agegp$`Individual age` == filter_indivage &
                            apps_agegp$`Age group` != filter_agegp,]

ggplot(data = apps_filter,
       aes(x = Year, 
           y = Applicants, 
           color = `Age group`)
       ) +
  geom_line() +
  scale_y_continuous(labels = scales::number) +
  scale_x_continuous(breaks=seq(2006,2022,1)) +
  guides(x = guide_axis(n.dodge = 2)) +
  labs(title = "Applicants by age")

apps_indage <- apps_agegp[apps_agegp$Domicile == filter_apptype &
                            apps_agegp$Gender != filter_gender &
                            apps_agegp$`Applicant type` == filter_apptype &
                            apps_agegp$`Individual age` != filter_indivage &
                            apps_agegp$`Age group` != filter_agegp,]

apps_indage$`Individual age` <- as.factor(apps_indage$`Individual age`)

apps_indage <- apps_indage %>%
  select(everything()) %>%
  mutate(
    Gender_selector = paste(Gender, "All")
  )
indAgeData <- apps_indage %>%
  filter(grepl("Women", Gender_selector))

ggplot(data = apps_indage,
       aes(y = `Individual age`,
           x = Applicants,
           fill = Gender)
) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(title = "Applicants by individual age and gender") +
  scale_y_discrete(limits = rev(levels(apps_indage$`Individual age`))) +
  scale_x_continuous(labels = scales::number)