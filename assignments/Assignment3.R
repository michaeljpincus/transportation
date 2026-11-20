# Assignment 3: Traffic Impact Analysis

library(readxl)
library(ggplot2)

appA <- read_excel('a3data.xlsx', sheet='A')
appB <- read_excel('a3data.xlsx', sheet='B')
appC <- read_excel('a3data.xlsx', sheet='c')

ggplot(appA) + 
  geom_point(aes(x=flow, y=speed, color=los, size=demand)) + 
  scale_color_brewer(palette='Spectral', direction=-1) + 
  theme_bw() + 
  labs(x='Flow', y='Speed (mph)', color='Level of Service', size='Demand', title='Appendix A', subtitle='Simpified speed-flow relationship (per lane)')

### Question 1

ne_am_los <- appA[[which(appA$flow == 10800/6 & appA$speed == 52), "los"]]
ne_pm_los <- appA[[which(appA$flow == 10200/6 & appA$speed == 53), "los"]]

sw_am_los <- appA[[which(appA$flow == 6000/4 & appA$speed == 35), "los"]]
sw_pm_los <- appA[[which(appA$flow == 6400/4 & appA$speed == 39), "los"]]

nw_am_los <- appA[[which(appA$flow == 8000/5 & appA$speed == 54), "los"]]
nw_pm_los <- appA[[which(appA$flow == 9000/5 & appA$speed == 52), "los"]]

### Question 2 

am_new_trips_generated_total <- 800 * 1.45 * 0.87
pm_new_trips_generated_total <- 800 * 1.50 * 0.85

### Question 3

ne_am_newtrips <- am_new_trips_generated_total * appC[[1, "ne_share"]]
ne_pm_newtrips <- pm_new_trips_generated_total * appC[[2, "ne_share"]]

sw_am_newtrips <- am_new_trips_generated_total * appC[[1, "sw_share"]]
sw_pm_newtrips <- pm_new_trips_generated_total * appC[[2, "sw_share"]]

nw_am_newtrips <- am_new_trips_generated_total * appC[[1, "nw_share"]]
nw_pm_newtrips <- pm_new_trips_generated_total * appC[[2, "nw_share"]]

### Question 4 

# Calculating total existing demand 

ne_am_existingdemand_perlane <- (appA[[which(appA$flow == 10800/6 & appA$speed == 52), "demand"]])
ne_pm_existingdemand_perlane <- (appA[[which(appA$flow == 10200/6 & appA$speed == 53), "demand"]])

sw_am_existingdemand_perlane <- (appA[[which(appA$flow == 6000/4 & appA$speed == 35), "demand"]])
sw_pm_existingdemand_perlane <- (appA[[which(appA$flow == 6400/4 & appA$speed == 39), "demand"]])

nw_am_existingdemand_perlane <- (appA[[which(appA$flow == 8000/5 & appA$speed == 54), "demand"]])
nw_pm_existingdemand_perlane <- (appA[[which(appA$flow == 9000/5 & appA$speed == 52), "demand"]])

# Calculating new demand per lane

ne_am_newdemand_perlane <- round(ne_am_existingdemand_perlane + (ne_am_newtrips/6), digits=-2)
ne_pm_newdemand_perlane <- round(ne_pm_existingdemand_perlane + (ne_pm_newtrips/6), digits=-2)

sw_am_newdemand_perlane <- round(sw_am_existingdemand_perlane + (sw_am_newtrips/4), digits=-2)
sw_pm_newdemand_perlane <- round(sw_pm_existingdemand_perlane + (sw_pm_newtrips/4), digits=-2)

nw_am_newdemand_perlane <- round(nw_am_existingdemand_perlane + (nw_am_newtrips/5), digits=-2)
nw_pm_newdemand_perlane <- round(nw_pm_existingdemand_perlane + (nw_pm_newtrips/5), digits=-2)

# Calculating new speed 

ne_am_newspeed <- appA[[which(appA$demand == ne_am_newdemand_perlane), 'speed']]
ne_pm_newspeed <- appA[[which(appA$demand == ne_pm_newdemand_perlane), 'speed']]

sw_am_newspeed <- appA[[which(appA$demand == sw_am_newdemand_perlane), 'speed']]
sw_pm_newspeed <- appA[[which(appA$demand == sw_pm_newdemand_perlane), 'speed']]

nw_am_newspeed <- appA[[which(appA$demand == nw_am_newdemand_perlane), 'speed']]
nw_pm_newspeed <- appA[[which(appA$demand == nw_pm_newdemand_perlane), 'speed']]

# Calculating new flow

ne_am_newflow <- appA[[which(appA$demand == ne_am_newdemand_perlane & appA$speed == ne_am_newspeed), 'flow']]
ne_pm_newflow <- appA[[which(appA$demand == ne_pm_newdemand_perlane & appA$speed == ne_pm_newspeed), 'flow']]
ne_am_newflow_total <- ne_am_newflow * 6
ne_pm_newflow_total <- ne_pm_newflow * 6

sw_am_newflow <- appA[[which(appA$demand == sw_am_newdemand_perlane & appA$speed == sw_am_newspeed), 'flow']]
sw_pm_newflow <- appA[[which(appA$demand == sw_pm_newdemand_perlane & appA$speed == sw_pm_newspeed), 'flow']]
sw_am_newflow_total <- sw_am_newflow * 4
sw_pm_newflow_total <- sw_pm_newflow * 4

nw_am_newflow <- appA[[which(appA$demand == nw_am_newdemand_perlane & appA$speed == nw_am_newspeed), 'flow']]
nw_pm_newflow <- appA[[which(appA$demand == nw_pm_newdemand_perlane & appA$speed == nw_pm_newspeed), 'flow']]
nw_am_newflow_total <- nw_am_newflow * 5
nw_pm_newflow_total <- nw_pm_newflow * 5

# Calculating new LOS 

ne_am_newlos <- appA[[which(appA$flow == ne_am_newflow & appA$speed == ne_am_newspeed), 'los']]
ne_pm_newlos <- appA[[which(appA$flow == ne_pm_newflow & appA$speed == ne_pm_newspeed), 'los']]

sw_am_newlos <- appA[[which(appA$flow == sw_am_newflow & appA$speed == sw_am_newspeed), 'los']]
sw_pm_newlos <- appA[[which(appA$flow == sw_pm_newflow & appA$speed == sw_pm_newspeed), 'los']]

nw_am_newlos <- appA[[which(appA$flow == nw_am_newflow & appA$speed == nw_am_newspeed), 'los']]
nw_pm_newlos <- appA[[which(appA$flow == nw_pm_newflow & appA$speed == nw_pm_newspeed), 'los']]


### Question 7

# Calculating expanded highway demand 

expanded_am_newdemand_perlane <- ((sw_am_newdemand_perlane * 4)/5)
expanded_pm_newdemand_perlane <- ((sw_pm_newdemand_perlane * 4)/5)

rounded_expanded_am_newdemand_perlane <- round(expanded_am_newdemand_perlane, digits=-2)

# Calculating expanded highway speed 

expanded_am_speed <- appA[[which(appA$demand == rounded_expanded_am_newdemand_perlane), 'speed']]
expanded_pm_speed <- appA[[which(appA$demand == expanded_pm_newdemand_perlane), 'speed']]

# Calculated total expanded highway flows 

expanded_am_total_flow <- (5*appA[[which(appA$demand==rounded_expanded_am_newdemand_perlane & appA$speed==expanded_am_speed), 'flow']])
expanded_pm_total_flow <- (5*appA[[which(appA$demand==expanded_pm_newdemand_perlane & appA$speed==expanded_pm_speed), 'flow']])

### Question 8 

# Two hours of peak travel time in both the AM and PM, all trips on SW highway are 8 miles.
# What are total vehicle hours of travel time saved per day? 

peak_hours <- 2 
trip_miles <- 8
speed_old_am <- 32
speed_old_pm <- 35 
speed_new_am <- expanded_am_speed
speed_new_pm <- expanded_pm_speed

expanded_demand_am_total <- expanded_am_newdemand_perlane * 5
expanded_demand_pm_total <- expanded_pm_newdemand_perlane * 5 

vehicles_am <- expanded_demand_am_total * peak_hours
vehicles_pm <- expanded_demand_pm_total * peak_hours 

tt_old_am <- trip_miles / speed_old_am
tt_old_pm <- trip_miles / speed_old_pm

tt_new_am <- trip_miles / speed_new_am 
tt_new_pm <- trip_miles / speed_new_pm

saved_am <- round(vehicles_am * (tt_old_am - tt_new_am))
saved_pm <- round(vehicles_pm * (tt_old_pm - tt_new_pm))
saved_total <- round(saved_am + saved_pm)

### Question 9

annual_hours <- saved_total * 350
annual_value <- annual_hours * 30

### Question 10 

break_even_cost <- (annual_value * 10) / 8 








