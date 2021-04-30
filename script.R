## Importing packages
library(FreqProf)
library(tidyverse)

source("R/build_ono_table.R")

## Importing data
data <- read_csv(file = "data/data_bb.csv", col_types = 
  cols(
    Date_Time_Absolute_dmy_hmsf = col_datetime(format = "%d-%m-%Y %H:%M:%OS"),
    Date_dmy = col_date(format = "%d-%m-%Y"),
    Time_Absolute_hms = col_time(format = "%H:%M:%S"),
    Time_Absolute_f = col_integer(),
    Time_Relative_hmsf = col_time(format = "%H:%M:%OS"),
    Time_Relative_hms = col_time(format = "%H:%M:%S"),
    Time_Relative_f = col_integer(),
    Time_Relative_sf = col_character(),
    Duration_sf = col_double(),
    Result_Container = col_character(),
    Observation = col_factor(),
    Event_Log = col_character(),
    Subject = col_factor(),
    Behavior = col_factor(),
    #Modifier_1 = col_character(),
    #Modifier_2 = col_character(),
    #Modifier_3 = col_character(),
    #Modifier_4 = col_character(),
    #Modifier_5 = col_character(),
    #Modifier_6 = col_character(),
    #Modifier_7 = col_character(),
    Event_Type = col_factor(),
    Comment = col_character())
  )

## Transforming data into start - stop data
result <- data %>%
  arrange(Observation, Subject, Behavior) %>%
  group_by(row = ceiling(row_number()/2)) %>%
  pivot_wider(names_from = Event_Type,
              values_from = c(Date_Time_Absolute_dmy_hmsf,
                              Date_dmy, 
                              Time_Absolute_hms,
                              Time_Absolute_f,
                              Time_Relative_hmsf,
                              Time_Relative_hms,
                              Time_Relative_f,
                              Time_Relative_sf,
                              Duration_sf,
                              #Result_Container,
                              #Observation,
                              #Event_Log,
                              #Subject,
                              #Behavior,
                              #Modifier_1,
                              #Modifier_2,
                              #Modifier_3,
                              #Modifier_4,
                              #Modifier_5,
                              #Modifier_6,
                              #Modifier_7,
                              #Event_Type,
                              Comment
                              )) %>%
  ungroup() %>%
  select(starts_with("Date_Time_Absolute_dmy_hmsf"),
         starts_with("Date_dmy"),
         starts_with("Time_Absolute_hms "),
         starts_with("Time_Absolute_f"),
         starts_with("Time_Relative_hmsf"),
         starts_with("Time_Relative_hms Time_Relative_f"),
         starts_with("Time_Relative_sf"),
         starts_with("Duration_sf"),
         Result_Container,
         Observation,
         Event_Log,
         Subject,
         Behavior,
         Modifier_1,
         #Modifier_2,
         #Modifier_3,
         #Modifier_4,
         #Modifier_5,
         #Modifier_6, 
         #Modifier_7,
         starts_with("Comment"),
         -row)

## Transforming column type
result$`Time_Relative_sf_State start` <- 
  as.integer(result$`Time_Relative_sf_State start`)

result$`Time_Relative_sf_State stop` <- 
  as.integer(result$`Time_Relative_sf_State stop`)

result$Behavior <- 
  as.character(result$Behavior)

## finding all groups to create a vector
groups <- c()
for(b in data$Observation){
  if((b %in% groups)==0){
    groups <- c(groups, b)
  }
}

subjects <- c()
for(s in data$Subject){
  if((s %in% subjects)==0){
    subjects <- c(subjects, s)
  }
}


### for each pupils in each groups generate a .csv file
for (group in groups) {

  for (subject in subjects) {
      
      possibleError <- tryCatch({
          result %>%
            filter(Observation == group) %>%
            filter(Subject == subject) %>%
            build_ono_data(df = as.data.frame(.),
                           start = "Time_Relative_sf_State start",
                           end = "Time_Relative_sf_State stop",
                           behavior_column_name = "Behavior",
                           select_behavior = c("all")) %>%
            write.csv(., paste0("results/csv/fp-",
                                group,
                                subject,
                                ".csv"))
          },
            error=function(e) {
              print(e)
          },
            warning=function(e){
              print(e)
          })
      if(inherits(possibleError, "error")) next
      if(inherits(possibleError, "warning")) next
    }
}

