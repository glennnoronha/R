# Problem 1
setwd("C:\Users\glenn\Downloads")
brfss <- read.csv("brfss.csv")
head(brfss)
cleanBRFSSFrame <- function(df){
  df$sex <- NULL
  df <- na.omit(df)
  return(df)
}

clean_brfss <- cleanBRFSSFrame(brfss)
install.packages("psych")
library("psych")
describe(clean_brfss$weight2)

median_age <- median(clean_brfss$age)
median_age
mean_age <- mean(clean_brfss$age)
mean_age
table(clean_brfss$age)
get_mode <- function (df){
  mode_val <- names(which.max(table(df$age)))
  return(mode_val)
}
get_mode(clean_brfss)

# Problem 2
weekday <- function(day, month, year){
  if (month == 1){
    m <- 11
    year <- year - 1
  }
  else if (month == 2){
    m <- 12
    year <- year - 1
  }
  else {
  m <- month - 2
  }
  
  k <- day
  y <- year %% 100
  c <- year %/% 100
  
  f <- (floor(2.6 * m - 0.2) + k + y + floor(y / 4) + floor(c / 4) - 2 * c) %% 7
  
  days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  return(days[f + 1])
}

weekday(13,11,2002)

# Problem 3
install.packages("gapminder")
library("gapminder")
head(gapminder)

less_29 <- gapminder[gapminder$lifeExp < 29, ]
less_29

rwanda <- gapminder[gapminder$country == "Rwanda" & gapminder$year > 1979, ]
rwanda

rwanda_afghan <- gapminder[gapminder$country == "Rwanda" | gapminder$country == "Afghanistan", ]
rwanda_afghan

cambodia <- gapminder[gapminder$country == "Cambodia",c("year","lifeExp")]
cambodia

ordered_df <- gapminder[order(gapminder$year, gapminder$country), ]
head(ordered_df)

names(ordered_df)
names(ordered_df)[6] = "gdp_percap"
names(ordered_df)
head(ordered_df)

table(gapminder$continent)
table(gapminder$country)

# Problem 4
mean_pm <- 0.25
sd_pm <- 0.02
k <- ((.30 - mean_pm) / sd_pm)
k
chebyshev <- 1 - (1 / (k^2))
cat("minimum percentage:", chebyshev, "%\n")

# Problem 5 
p_1A <- 4 / 52
p_2A <- 3 / 51
p_both <- p_1A * p_2A
cat("probabilty of drawing two aces without replacment:", p_both)

# Problem 6
p_A_J <- (4 / 52)* (4 / 51)
p_J_A <- (4 / 52) * (4 / 51)
p_both <- p_A_J + p_J_A
cat("probabilty of drawing an ace and a jack without replacment:", p_both)

# Problem 7
leg_mean <- 5 * 12
leg_sd <- 3
val <- 62
z_score_leg <- (val - leg_mean) / leg_sd
cat("z-score of length 62 =", z_score_leg)

# Problem 8
mean_score <- 85
sd_score <- 4.5
k_75 <- 2 
k_88.89 <- 3
range_75 <- c(mean_score - k_75 * sd_score, mean_score + k_75 * sd_score)
range_889 <- c(mean_score - k_88.89 * sd_score, mean_score + k_88.89 * sd_score)
cat("75% students scored between:", range_75[1], "and", range_75[2], "\n")
cat("88.89% students scored between:", range_889[1], "and", range_889[2], "\n")

# Problem 9
x <- c(0,10,20)
p <- c(.35,.25,.40)
exp_val <- sum(x * p)
cat("expected value =", exp_val)

# Problem 10
cost_heads <- 300 * (1 - .40)
cost_tails <- 300
exp_cost <- (cost_heads * 0.5) + (cost_tails * 0.5)
cat("expected Cost = $", exp_cost)

# Problem 11
p_red <- 18 / 38
p_not_red <- 20 / 38
exp_gain <- 1 * p_red + (-1) * p_not_red
cat("expected net gain:", exp_gain, "\n")

# PRoblem 12
x <- c(20-12, -12)
p <- c(0.9, 0.1)
mean_x <- sum(x * p)
ex2 <- sum((x^2) * p)
var_x <- ex2 - mean_x^2
sd_x <- sqrt(var_x)

cat("standard deviation:", sd_x, "\n")

# Problem 13
n <- 2
p <- 0.7
q <- 1 - p
sd_jack <- sqrt(n * p * q)
sd_jack  

# Problem 14
stress <- c(11, 25, 19, 7, 23, 6, 11, 22, 25, 10)
satisfaction <- c(7, 1, 4, 9, 2, 8, 8, 3, 3, 6)
# a) 
plot(stress, satisfaction, main = "Stress vs Life Satisfaction", 
     xlab = "Stress", ylab = "Life Satisfaction", pch = 19)
# b)
correlation <- cor(stress, satisfaction)
correlation  
# c)
# The correlation is negative which means there is an inverse relationship
# as stress increases life satisfaction decreases

# d)
# can't say that being more stressed causes lower life satisfaction just because
# there’s a strong negative correlation. There is a relationship but it doesn’t
# prove one causes the other.

# Problem 15
# r = 0.74: plot D, strong positive
# r = 0.39: plot B, moderate positive
# r = -0.85: plot A, strong negative
# r = -0.69: plot C, moderate negative

# Problem 16
room <- c(220, 727, 285, 273, 145, 213, 398, 343, 250, 414, 400, 700)
cost <- c(499, 340, 585, 495, 495, 279, 279, 455, 595, 367, 675, 420)


# a)
mean_rooms <- mean(room)
cat("mean rooms:", mean_rooms)

# b)
mean_cost <- mean(cost)
cat("mean cost for a double room: $", mean_cost)

# c)
plot(room, cost, main = "Rooms vs Cost per Night", 
     xlab = "Number of Rooms", ylab = "Cost per Night", pch = 19)

# d) 
corr_hotels <- cor(room, cost)
corr_hotels
# a weak negative relationship between rooms and cost per night
# hotels with more rooms usually have cheaper room prices
# there are probably other factors that affect price as wel.
print(cost)

