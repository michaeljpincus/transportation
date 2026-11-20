### Assignment 5 

library(tidyverse)
library(readxl)
hh <- readr::read_csv("Household travel survey/1_Household_Public.csv", locale = readr::locale(encoding = "latin1"))
per <- readr::read_csv("Household travel survey/2_Person_Public.csv", locale = readr::locale(encoding = "latin1"))
veh <- readr::read_csv("Household travel survey/3_Vehicle_Public.csv", locale = readr::locale(encoding = "latin1"))
trip <- readr::read_csv("Household travel survey/4_Trip_Public.csv", locale = readr::locale(encoding = "latin1"))

hh_dict <- read_excel('dd.xlsx', sheet='1 Household')
per_dict <- read_excel('dd.xlsx', sheet='2 Person')
veh_dict <- read_excel('dd.xlsx', sheet='3 Vehicle')
trip_dict <- read_excel('dd.xlsx', sheet='4 Trip')