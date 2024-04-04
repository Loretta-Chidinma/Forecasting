# Forecasting
Forecasting the strategic placement of products on shelves in retail stores
---
# Forecasting for the Optimization of Daily Sales of Products at the Supermarket.

---
# installing needed package
  
install.packages("openxlsx")

# {Load packages}

library(tidyverse)
library(openxlsx)
library(ggplot2)

# {Load file} PRODUCT (1) Potato Chips Data

potato_chips <- read.xlsx("/Users/chidinma/Library/Mobile Documents/com~apple~CloudDocs/R Programming/Nestle_Nesquik.xlsx")

glimpse(potato_chips)

str(potato_chips)
View(potato_chips)

# Data Visualization
# Scatter plot with ggplot2
# Plot from data "potato_chips"

ggplot(potato_chips, aes(quantity_sold, height_in_feet)) +
  geom_point(color = "red") +
  labs(title = "Potato Chips Data", quantity_sold = "X-axis", height_in_feet = "Y-axis")

# {Building Linear Regression Model}

# Total quantities sold is dependent on the Height in feet (shelves)

potato_chips_lm <- lm(quantity_sold ~ factor(height_in_feet), data = potato_chips) 
View(potato_chips_lm)

# Summarizing the regression model to interpret result

# Print model summary
potato_chips_lm %>% summary()

# Analysis


# A positive coefficient indicates that as the value of the independent variable 
# increases (height), the mean of the dependent variable (sales) also tends to increase.

# At 5 feet sales are 26. Now we interpret 6 and 7 feet in relation to the 5 feet 
# shelf location.

# At 6 feet sales increase by 41 units, but at 7 feet sales are only increased by 
# 21 units in relation to 5 feet. Overall, 6 feet is a better location for the potato chips.

# p-values are <.05 for both independent variables


# Equation for manual if we want to use this linear model to forecast sales?
  
  # quantity_sold = 26 + 41*(6 feet) + 21*(7 feet)

# Luckily, for us we can use R to calculate the math.


# Creating a new dataframe for the forecast

potato_chips_sales <- tibble(potato_chips)

head(potato_chips_sales)

# {Predicting potato chips sales}

prediction <- predict (potato_chips_lm, potato_chips_sales = potato_chips_sales)

View(prediction)

# Creating data frame with the predictions

# Put predictions inside a data frame

prediction_potato_chips <- potato_chips_sales  %>% 
  mutate(forecast_quantity_sold = predict(potato_chips_lm, potato_chips_sales))

#Adding residuals as a column

prediction_potato_chips <- prediction_potato_chips %>% 
  mutate(errors = forecast_quantity_sold - quantity_sold)

# Checking results
head(prediction_potato_chips)

# Summarizing Potato Chips sales data on our forecast}
prediction_potato_chips %>% 
  group_by(height_in_feet) %>% 
  summarize(avg_quantity_sold = mean(forecast_quantity_sold),
            sum_quantity_sold = sum(forecast_quantity_sold))



# {Load file} PRODUCT (2) Nestle Nesquik Data

nestle_nesquik <- read.xlsx("/Users/chidinma/Library/Mobile Documents/com~apple~CloudDocs/R Programming/Nestle_Nesquik.xlsx")

glimpse(nestle_nesquik)

str(nestle_nesquik)
View(nestle_nesquik)

# Data Visualization
# Scatter plot with ggplot2
# Plot from data "potato_chips"

ggplot(nestle_nesquik, aes(quantity_sold, height_in_feet)) +
  geom_point(color = "red") +
  labs(title = "Nestle Nesquik Data", quantity_sold = "X-axis", height_in_feet = "Y-axis")

# {Building Linear Regression Model}

# Total quantities sold is dependent on the Height in feet (shelves)

nestle_nesquik_lm <-  lm(quantity_sold ~ factor(height_in_feet), data = nestle_nesquik) 
View(potato_chips_lm)

# Summarizing the regression model to interpret result

# Print model summary
nestle_nesquik_lm %>% summary()

# Analysis


# A positive coefficient indicates that as the value of the independent variable 
# increases (height_in_feet), the mean of the dependent variable (quantity_sold) also tends to increase.

# At 2 feet sales are 24.3 units. Now we interpret 3 and 4 feet in relation to the 2 feet 
# shelf location.

# At 3 feet sales decreased to 20.7  units, but at 4 feet sales increased by 
# 40.7 units in relation to 2 feet. Overall, 4 feet is a better location for the nestle nesquik.

# p-values are <.05 for both independent variables


# Equation for manual if we want to use this linear model to forecast sales?

# quantity_sold = 24 + 21*(3 feet) + 41*(4 feet)

# Luckily, for us we can use R to calculate the math.


# Creating a new dataframe for the forecast

nestle_nesquik_sales <- tibble(nestle_nesquik)

head(nestle_nesquik_sales)

# {Predicting potato chips sales}

prediction <- predict (nestle_nesquik_lm, nestle_nesquik_sales = nestle_nesquik_sales)

View(prediction)

# Creating data frame with the predictions

# Put predictions inside a data frame

prediction_nestle_nesquik <- nestle_nesquik_sales  %>% 
  mutate(forecast_quantity_sold = predict(nestle_nesquik_lm, nestle_nesquik_sales))

#Adding residuals as a column

prediction_nestle_nesquik <- prediction_nestle_nesquik %>% 
  mutate(errors = forecast_quantity_sold - quantity_sold)

# Checking results
head(prediction_nestle_nesquik)

# Summarizing sales data on our forecast}
prediction_nestle_nesquik %>% 
  group_by(height_in_feet) %>% 
  summarize(avg_quantity_sold = mean(forecast_quantity_sold),
            sum_quantity_sold = sum(forecast_quantity_sold))

# {Load file} PRODUCT (3) Multi-Fruit Data

multi_fruit <- read.xlsx("/Users/chidinma/Library/Mobile Documents/com~apple~CloudDocs/R Programming/Multi_Fruit.xlsx")

glimpse(multi_fruit)

str(multi_fruit)
View(multi_fruit)

# Data Visualization
# Scatter plot with ggplot2
# Plot from data "potato_chips"

ggplot(multi_fruit, aes(quantity_sold, height_in_feet)) +
  geom_point(color = "red") +
  labs(title = "Multifruit Data", quantity_sold = "X-axis", height_in_feet = "Y-axis")

# {Building Linear Regression Model}

# Total quantities sold is dependent on the Height in feet (shelves)

multi_fruit_lm <-  lm(quantity_sold ~ factor(height_in_feet), data = multi_fruit) 
View(multi_fruit_lm)

# Summarizing the regression model to interpret result

# Print model summary
multi_fruit_lm %>% summary()

# Analysis


# A positive coefficient indicates that as the value of the independent variable 
# increases (height), the mean of the dependent variable (sales) also tends to increase.

# At 5 feet sales are 26. Now we interpret 6 and 7 feet in relation to the 5 feet 
# shelf location.

# At 6 feet sales increase by 41 units, but at 7 feet sales are only increased by 
# 21 units in relation to 5 feet. Overall, 6 feet is a better location for the potato chips.

# p-values are <.05 for both independent variables


# Equation for manual if we want to use this linear model to forecast sales?

# quantity_sold = 26 + 41*(6 feet) + 21*(7 feet)

# Luckily, for us we can use R to calculate the math.


# Creating a new dataframe for the forecast

multi_fruit_sales <- tibble(multi_fruit)

head(multi_fruit_sales)

# {Predicting potato chips sales}

prediction <- predict (multi_fruit_lm, multi_fruit_sales = multi_fruit_sales)

View(prediction)

# Creating data frame with the predictions

# Put predictions inside a data frame

prediction_multi_fruit <- multi_fruit_sales  %>% 
  mutate(forecast_quantity_sold = predict(multi_fruit_lm, multi_fruit_sales))

#Adding residuals as a column

prediction_multi_fruit <- prediction_multi_fruit %>% 
  mutate(errors = forecast_quantity_sold - quantity_sold)

# Checking results
head(prediction_multi_fruit)

# Summarizing sales data on our forecast}
prediction_multi_fruit %>% 
  group_by(height_in_feet) %>% 
  summarize(avg_quantity_sold = mean(forecast_quantity_sold),
            sum_quantity_sold = sum(forecast_quantity_sold))


# {Load file} PRODUCT (4) Sardine Data

sardine <- read.xlsx("/Users/chidinma/Library/Mobile Documents/com~apple~CloudDocs/R Programming/Sardine.xlsx")

glimpse(sardine)

str(sardine)
View(sardine)

# Data Visualization
# Scatter plot with ggplot2
# Plot from data "potato_chips"

ggplot(sardine, aes(quantity_sold, height_in_feet)) +
  geom_point(color = "red") +
  labs(title = "Sardine Data", quantity_sold = "X-axis", height_in_feet = "Y-axis")

# {Building Linear Regression Model}

# Total quantities sold are dependent on the Height in feet (shelves)

sardine_lm <-  lm(quantity_sold ~ factor(height_in_feet), data = sardine) 
View(sardine_lm)

# Summarizing the regression model to interpret the result

# Print model summary
sardine_lm %>% summary()

# Analysis


# A positive coefficient indicates that the value of the independent variable 
# increases (height), the mean of the dependent variable (sales) also tends to increase.

# At 5 feet sales are 26. Now we interpret 6 and 7 feet in relation to the 5 feet 
# shelf location.

# At 6 feet sales increased by 41 units, but at 7 feet sales only increased by 
# 21 units in relation to 5 feet. Overall, 6 feet is a better location for the potato chips.

# p-values are <.05 for both independent variables


# Equation for a manual if we want to use this linear model to forecast sales?

# quantity_sold = 26 + 41*(6 feet) + 21*(7 feet)

# Luckily, for us we can use R to calculate the math.


# Creating a new data frame for the forecast

sardine_sales <- tibble(sardine)

head(sardine_sales)

# {Predicting potato chips sales}

prediction <- predict (sardine_lm, sardine_sales = sardine_sales)

View(prediction)

# Creating data frame with the predictions

# Put predictions inside a data frame

prediction_sardine <- sardine_sales  %>% 
  mutate(forecast_quantity_sold = predict(sardine_lm, sardine_sales))

#Adding residuals as a column

prediction_sardine <- prediction_sardine %>% 
  mutate(errors = forecast_quantity_sold - quantity_sold)

# Checking results
head(prediction_sardine)

# Summarizing sales data on our forecast}
prediction_sardine %>% 
  group_by(height_in_feet) %>% 
  summarize(avg_quantity_sold = mean(forecast_quantity_sold),
            sum_quantity_sold = sum(forecast_quantity_sold))
