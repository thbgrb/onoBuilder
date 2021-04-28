## Importing packages
library(FreqProf)
library(tidyverse)

source("R/build_ono_table.R")

## Importing data
data <- read_csv(file = "data/data.csv", col_types = 
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
    Modifier_1 = col_character(),
    Modifier_2 = col_character(),
    Modifier_3 = col_character(),
    Modifier_4 = col_character(),
    Modifier_5 = col_character(),
    Modifier_6 = col_character(),
    Modifier_7 = col_character(),
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
         Modifier_2,
         Modifier_3,
         Modifier_4,
         Modifier_5,
         Modifier_6, 
         Modifier_7,
         starts_with("Comment"),
         -row)

## Transforming column type
result$`Time_Relative_sf_State start` <- 
  as.integer(result$`Time_Relative_sf_State start`)

result$`Time_Relative_sf_State stop` <- 
  as.integer(result$`Time_Relative_sf_State stop`)

result$Behavior <- 
  as.character(result$Behavior)


## Choosing only the group 2
result <- filter(result,
                 Observation == "A1G2" | 
                   Observation == "A2G2" | 
                   Observation == "A3G2" | 
                   Observation == "A4G2")

## Transforming value in Observation column
result <- result %>%
  mutate(Observation = recode(Observation, 
                              "A1G2" = "A1G2 (WIMP)",
                              "A2G2" = "A2G2 (Paper)",
                              "A3G2" = "A3G2 (Hybrid)",
                              "A4G2" = "A4G2 (MTT)"))

## Analyses -----
# The data are composed of :
#  - Groups,
#  - Pupils,
#  - Behaviors,

groups <- c("A1G2 (WIMP)",
            "A2G2 (Paper)",
            "A3G2 (Hybrid)",
            "A4G2 (MTT)")

pupils <- c("e1",
            "e2",
            "e3",
            "e4",
            "e5",
            "e6")

behaviors <- c("Learning goal oriented",
               "Desengagement",
               "Passing an object",
               "Impatient behavior",
               "Removing Sth from other's hands",
               "Verbal",
               "Leave the activity",
               "Physical",
               "Sabotage",
               "Artifact Monopolization"
               )

#### Individual analyses ----
###### Each pupil in each group for each behavior ----
for (group in groups) {

  for (pupil in pupils) {
    
    for (behavior in behaviors) {
      
      possibleError <- tryCatch({
          result %>%
            filter(Observation == group) %>%
            filter(Subject == pupil) %>%
            filter(Behavior == behavior) %>%
            build_ono_data(df = as.data.frame(.),
                           start = "Time_Relative_sf_State start",
                           end = "Time_Relative_sf_State stop",
                           behavior_column_name = "Behavior",
                           select_behavior = c("all")) %>%
            freqprof() %>%
            plot_freqprof(gg = T)
        
            ggsave(paste0("results/test1/fp-",
                          group,
                          pupil,
                          behavior,
                          ".pdf"),
                 last_plot(),
                 device = "pdf")
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
}

###### Each pupil in each group with all behaviors ----
for (group in groups) {
  
  for (pupil in pupils) {
      
      possibleError <- tryCatch({
        result %>%
          filter(Observation == group) %>%
          filter(Subject == pupil) %>%
          build_ono_data(df = as.data.frame(.),
                         start = "Time_Relative_sf_State start",
                         end = "Time_Relative_sf_State stop",
                         behavior_column_name = "Behavior",
                         select_behavior = c("all")) %>%
          freqprof() %>%
          plot_freqprof(gg = T)
        
        ggsave(paste0("results/test2/fp-",
                      group,
                      pupil,
                      ".pdf"),
               last_plot(),
               device = "pdf")
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


#### Group analyses ----
###### Grouped by group and behaviors -----
for (group in groups) {
    
    for (behavior in behaviors) {
      
      possibleError <- tryCatch({
        result %>%
          filter(Observation == group) %>%
          filter(Behavior == behavior) %>%
          build_ono_data(df = as.data.frame(.),
                         start = "Time_Relative_sf_State start",
                         end = "Time_Relative_sf_State stop",
                         behavior_column_name = "Behavior",
                         select_behavior = c("all")) %>%
          freqprof() %>%
          plot_freqprof(gg = T)
        
        ggsave(paste0("results/test3/fp-",
                      group,
                      behavior,
                      ".pdf"),
               last_plot(),
               device = "pdf")
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

###### Grouped by group ----
for (group in groups) {
  
  for (behavior in behaviors) {
    
    possibleError <- tryCatch({
      result %>%
        filter(Observation == group) %>%
        build_ono_data(df = as.data.frame(.),
                       start = "Time_Relative_sf_State start",
                       end = "Time_Relative_sf_State stop",
                       behavior_column_name = "Behavior",
                       select_behavior = c("all")) %>%
        freqprof() %>%
        plot_freqprof(gg = T)
      
      ggsave(paste0("results/test4/fp-",
                    group,
                    ".pdf"),
             last_plot(),
             device = "pdf")
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

###### Grouped by behaviors
for (behavior in behaviors) {
    
    possibleError <- tryCatch({
      result %>%
        filter(Behavior == behavior) %>%
        unite(Behavior, c("Observation", "Behavior")) %>%
        build_ono_data(df = as.data.frame(.),
                       start = "Time_Relative_sf_State start",
                       end = "Time_Relative_sf_State stop",
                       behavior_column_name = "Behavior",
                       select_behavior = c("all")) %>%
        freqprof() %>%
        plot_freqprof(gg = T)
      
      ggsave(paste0("results/test5/fp-",
                    behavior,
                    ".pdf"),
             last_plot(),
             device = "pdf")
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
