## Assignment 2
## Nathan Leroy and Michael Pincus

install.packages("readxl")
library(readxl)

jobs <- read_excel('a2data.xlsx', sheet = 'Jobs')
car_time <- read_excel('a2data.xlsx', sheet = 'car travel time')
transit_time <- read_excel('a2data.xlsx', sheet = 'transit travel time')

## Using the following formula:
## Ai = sum_j( O_j * d_ij^(-b) )

## 5a.
b <- 0.1
weight_b <- 45^(-b)
weight_b

## 5b.
b <- 0.2
weight_b <- 45^(-b)
weight_b

## 5c.
b <- 0.2
weight_b <- 30^(-b)
weight_b

## 6.
## Setting up the matrices:

# Cutting the matrix in half here:
car_time[car_time == "X"] <- NA
transit_time[transit_time == "X"] <- NA

# Getting rid of the row labels:
car_time <- car_time[, -1]
transit_time <- transit_time[, -1]

# Making matrices:
car <- as.matrix(sapply(car_time, as.numeric))
transit <- as.matrix(sapply(transit_time, as.numeric))

## Calculating the ratio:
ratio <- transit / car
mean(ratio, na.rm = TRUE)

## On average, a transit trip takes 2.74 times longer than a car trip.

## a.
O <- jobs[, 2]
b <- 0.1
d <- transit[, 1]
Ai <- sum(O * d^(-b))
Ai

## b.
b <- 0.2
d <- transit[, 1]
Ai <- sum(O * d^(-b))
Ai

## c.
b <- 0.2
d_car_a <- car[, 1]
d_transit_a <- transit[, 1]
Ai_car_a <- sum(O * d_car_a^(-b), na.rm = TRUE)
Ai_transit_a <- sum(O * d_transit_a^(-b), na.rm = TRUE)
relative_a <- Ai_car_a / Ai_transit_a
relative_a

## d.
b <- 0.2
d_car_j <- car[10, ]
d_transit_j <- transit[10, ]
Ai_car_j <- sum(O * d_car_j^(-b), na.rm = TRUE)
Ai_transit_j <- sum(O * d_transit_j^(-b), na.rm = TRUE)
relative_j <- Ai_car_j / Ai_transit_j
relative_j