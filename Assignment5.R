### Assignment 5 - Michael Pincus

library(tidyverse)
library(readxl)


library(ggplot2)
library(showtext)

font_add_google("Roboto", "roboto")

showtext_auto()




hh <- readr::read_csv("Household travel survey/1_Household_Public.csv", locale = readr::locale(encoding = "latin1"))
per <- readr::read_csv("Household travel survey/2_Person_Public.csv", locale = readr::locale(encoding = "latin1"))
veh <- readr::read_csv("Household travel survey/3_Vehicle_Public.csv", locale = readr::locale(encoding = "latin1"))
trip <- readr::read_csv("Household travel survey/4_Trip_Public.csv", locale = readr::locale(encoding = "latin1"))

hh_dict <- read_excel('dd.xlsx', sheet='1 Household')
per_dict <- read_excel('dd.xlsx', sheet='2 Person')
veh_dict <- read_excel('dd.xlsx', sheet='3 Vehicle')
trip_dict <- read_excel('dd.xlsx', sheet='4 Trip')

# Question 1 

q1_hh <- hh |> 
  filter(HH_ID==388636)

q1_per <- per |> 
  filter(HH_ID==388636)

q1_veh <- veh |> 
  filter(HH_ID==388636)

q1_trip <- trip |> 
  filter(HH_ID==388636)

# Question 2 

bike_df <- trip |> 
  filter(MODE_AGG == 02)

total_bike_trips <- sum(bike_df$P_WEIGHT)

# Question 3 

total_travel_time <- sum(trip$Model_TravTime * trip$P_WEIGHT, na.rm = TRUE)

total_persons <- sum(per$P_WEIGHT, na.rm = TRUE)
avg_tt_per_person <- total_travel_time / total_persons

# Question 4 

bike_df <- trip |> filter(MODE_AGG == 2)

# Total daily bicycling time (minutes and hours)
total_bike_time     <- sum(bike_df$Model_TravTime * bike_df$P_WEIGHT, na.rm = TRUE)
total_bike_time_hrs <- total_bike_time / 60

# Bicycle share of total travel time
bike_tt_pct <- total_bike_time / total_travel_time * 100

# Bicycle share of trips
total_bike_trips <- sum(bike_df$P_WEIGHT, na.rm = TRUE)
bike_trips_pct   <- total_bike_trips / sum(trip$P_WEIGHT, na.rm = TRUE) * 100

# Question 5 

wage <- 35  # dollars per hour

# Time value of all cycling trips (weighted, minutes -> hours -> dollars)
total_bike_hrs_value <- (total_bike_time / 60) * wage

# Time value of all trips in the region
total_trips_hrs_value <- (total_travel_time / 60) * wage


# Question 7 

q7_hh <- hh |> x
  filter(H_COUNTY == 34015)

q7_trips <- trip |> 
  filter(HH_ID %in% q7_hh$HH_ID)

q7_trips |> 
  count(MODE_AGG, sort=TRUE)

# Question 8 

veh <- veh |> 
  mutate(YEAR = if_else(YEAR %in% c(9998, 9999), NA_real_, YEAR)) |> 
  mutate(VEH_AGE = 2012 - YEAR)

income <- hh |> 
  select(HH_ID, INCOME)

income <- income |> 
  mutate(INCOME = if_else(INCOME %in% c(98, 99), NA_real_, INCOME))

veh_joined <- left_join(veh, income, by ='HH_ID')

veh_joined <- veh_joined |> 
  filter(!is.na(INCOME))

veh_joined <- veh_joined |> 
  mutate(INCOME_F = factor(INCOME,
                              levels = 1:10,
                               labels = c("$0-$9K", "$10K-$24K",
                                          "$25K-$34K", "$35K-$49K",
                                          "$50K-$74K", "$75K-$99K",
                                          "$100K-$149K", "$150K-$199K",
                                          "$200K-$249K", "$250K or more"),
                              ordered = TRUE))

ggplot(veh_joined, aes(x = INCOME_F, y = VEH_AGE)) +
  geom_boxplot(na.rm = TRUE, outlier.alpha = 0.2) +
  coord_flip() +
  labs(x = "Household income (rounded)", y = "Vehicle age (years)") +
  theme_minimal(base_family='roboto')

# Question 9 

trip_joined <- left_join(trip, income, by = 'HH_ID')

trip_joined$MODE_F <- factor(
  trip_joined$MODE_AGG,
  levels = 1:7,
  labels = c("Walk", "Bike", "Private Vehicle", "Private Transit",
             "Public Transit", "School Bus", "Other")
)

trip_joined <- trip_joined |> 
  mutate(INCOME_F = factor(INCOME,
                           levels = 1:10,
                           labels = c("$0-$9K", "$10K-$24K",
                                      "$25K-$34K", "$35K-$49K",
                                      "$50K-$74K", "$75K-$99K",
                                      "$100K-$149K", "$150K-$199K",
                                      "$200K-$249K", "$250K or more"),
                           ordered = TRUE))

library(scales)

ggplot(subset(trip_joined, !is.na(INCOME_F) & !is.na(MODE_F)),
       aes(x = INCOME_F, fill = MODE_F)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(x = "Household income (rounded)", y = "Share of trips", fill = "Mode") +
  theme_minimal(base_family='roboto') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



