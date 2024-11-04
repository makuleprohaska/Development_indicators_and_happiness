library(readxl)

#reading in economic data
data1 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Annual_GDP_Growth.xls", skip = 3)
data2 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Current_Account.xls", skip = 3)
data3 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/GDP_percapita.xls", skip = 3)
data4 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Prevalence_Undernourishment.xls", skip = 3)
data5 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Extreme_Poverty.xls", skip = 3)
data6 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Life_Expectancy.xls", skip = 3)
data7 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Inflation.xls", skip = 3)
data8 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Literacy_rate.xls", skip = 3)
data9 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Mortality_Rate.xls", skip = 3)
data10 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Unemployment.xls", skip = 3)

#reading in happiness data

data13 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Happiness_2016.xlsx")
data14 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Happiness_2017.xls")
data15 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Happiness_2018.xls")
data16 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Happines_2019.xls")
data17 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Happiness_2020.xls")
data18 <- read_excel("/Users/matteobruno/Documents/Bocconi/Second Year/Statistics/Happiness_2021.xls")


# libraries necessary for next step
library(dplyr)
library(tidyr)

#reformat data to only include country names and values for years 2010-22
years <- as.character(2016:2021)

# Select the relevant year columns for each dataset
data1_selected <- data1 %>% select(`Country Name`, all_of(years))
data2_selected <- data2 %>% select('Country Name', all_of(years))
data3_selected <- data3 %>% select('Country Name', all_of(years))
data4_selected <- data4 %>% select('Country Name', all_of(years))
data5_selected <- data5 %>% select('Country Name', all_of(years))
data6_selected <- data6 %>% select('Country Name', all_of(years))
data7_selected <- data7 %>% select('Country Name', all_of(years))
data8_selected <- data8 %>% select('Country Name', all_of(years))
data9_selected <- data9 %>% select('Country Name', all_of(years))
data10_selected <- data10 %>% select('Country Name', all_of(years))

#Filter relevant happiness data
data13_selected <- data13 %>% select('Country', 'Happiness score')
data13_selected <- rename(data13_selected, `Country Name` = 'Country', `2016` = 'Happiness score')
data14_selected <- data14 %>% select('Country', 'Happiness score')
data14_selected <- rename(data14_selected, `Country Name` = 'Country', `2017` = 'Happiness score')
data15_selected <- data15 %>% select('Country', 'Happiness score')
data15_selected <- rename(data15_selected, `Country Name` = 'Country', `2018` = 'Happiness score')
data16_selected <- data16 %>% select('Country name', 'Ladder score')
data16_selected <- rename(data16_selected, `Country Name` = 'Country name', `2019` = 'Ladder score')
data17_selected <- data17 %>% select('Country name', 'Ladder score')
data17_selected <- rename(data17_selected, `Country Name` = 'Country name', `2020` = 'Ladder score')
data18_selected <- data18 %>% select('Country', 'Happiness score')
data18_selected <- rename(data18_selected, `Country Name` = 'Country', `2021` = 'Happiness score')

#recursive algorithm for merging the data
merge_datasets_recursive <- function(dataset_list) {
  
  if (length(dataset_list) == 1) {
    return(dataset_list[[1]])
  } else {
  
    merged <- inner_join(dataset_list[[1]], dataset_list[[2]], by = c("Country Name"))
    
    new_list <- c(list(merged), dataset_list[-(1:2)])
    
    return(merge_datasets_recursive(new_list))
  }
}



#merge economic data
dataset_list <- list(data1_selected, data2_selected, data3_selected, data4_selected, data5_selected, data6_selected, data7_selected, data8_selected, data9_selected, data10_selected)
final_merged_dataset <- merge_datasets_recursive(dataset_list)

#merge happiness data
happines_list <- list(data13_selected, data14_selected, data15_selected, data16_selected, data17_selected, data18_selected)
final_happiness <- merge_datasets_recursive(happines_list)


# Removing columns containing "x.x.x" or "y.y.y.y" but not "x.x.x.x", "x.x.x.x.x", or "y.y.y.y.y"
clean_merged_dataset_1 <- final_merged_dataset

for(col_name in names(final_merged_dataset)){
  if ((grepl("x.x.x", col_name) || grepl("y.y.y.y", col_name)) && 
    (!grepl("x.x.x.x", col_name) && !grepl("x.x.x.x.x", col_name) && !grepl("y.y.y.y.y", col_name))){
    
    clean_merged_dataset_1 <- clean_merged_dataset_1[, -which(names(clean_merged_dataset_1) == col_name)]
    
  }
  
  
}
#removing rows with missing data
clean_merged_dataset <- na.omit(clean_merged_dataset_1)

#NORMALIZING THE DATA
library(caret)

ss <- preProcess(as.data.frame(clean_merged_dataset), method=c("range"))
normalized_dataset <- predict(ss, as.data.frame(clean_merged_dataset))

#mergingthedata
final <- inner_join(normalized_dataset, final_happiness, by = c("Country Name"))


#changing the name of the columns with happiness data
names(final)[names(final) == "2016"] <- "Happiness_2016"
names(final)[names(final) == "2017"] <- "Happiness_2017"
names(final)[names(final) == "2018"] <- "Happiness_2018"
names(final)[names(final) == "2019"] <- "Happiness_2019"
names(final)[names(final) == "2020"] <- "Happiness_2020"
names(final)[names(final) == "2021"] <- "Happiness_2021"

#renaming the columns 
#changing the name of .x in GDP_growth
for(col_name in names(final)){
  if (grepl(".x", col_name)  && 
      (!grepl(".y", col_name) && !grepl(".x.x", col_name) && !grepl(".y.y", col_name)  && !grepl(".y.y.y", col_name)
       && !grepl(".x.x.x.x", col_name)  && !grepl(".x.x.x.x.x", col_name) && !grepl(".y.y.y.y.y", col_name) 
       
      )){
    
    names(final)[names(final) == col_name] <- paste0("GDP_growth_", substring(col_name, 1, 4))
    
  } 
}

#changing the name of .y in current_account
for(col_name in names(final)){
  if (grepl(".y", col_name)  && 
      (!grepl("Country", col_name) && !grepl(".x.x", col_name) && !grepl(".y.y", col_name)  && !grepl(".y.y.y", col_name)
       && !grepl(".x.x.x.x", col_name)  && !grepl(".x.x.x.x.x", col_name) && !grepl(".y.y.y.y.y", col_name) 
       
      )){
    
    names(final)[names(final) == col_name] <- paste0("Current_account_", substring(col_name, 1, 4))
    
  } 
}

#changing the name of .x.x in GDP_percapita
for(col_name in names(final)){
  if (grepl(".x.x", col_name)  && 
      (!grepl("Country", col_name) && !grepl(".y.y", col_name)  && !grepl(".y.y.y", col_name)
       && !grepl(".x.x.x.x", col_name)  && !grepl(".x.x.x.x.x", col_name) && !grepl(".y.y.y.y.y", col_name) 
       
      )){
    
    names(final)[names(final) == col_name] <- paste0("GDP_percapita_", substring(col_name, 1, 4))
    
  } 
}

#changing the name of .y.y in Prevalence_Undernourishment
for(col_name in names(final)){
  if (grepl(".y.y", col_name)  && 
      (!grepl("Country", col_name) && !grepl(".y.y.y", col_name)
       && !grepl(".x.x.x.x", col_name)  && !grepl(".x.x.x.x.x", col_name) && !grepl(".y.y.y.y.y", col_name) 
       
      )){
    
    names(final)[names(final) == col_name] <- paste0("Prevalence_Undernourishment_",substring(col_name, 1, 4))
    
  } 
}

#changing the name of .y.y.y in Life_Expectancy
for(col_name in names(final)){
  if (grepl(".y.y.y", col_name)  && 
      (!grepl("Country", col_name) && !grepl(".x.x.x.x", col_name)  
       && !grepl(".x.x.x.x.x", col_name) && !grepl(".y.y.y.y.y", col_name) 
       
      )){
    
    names(final)[names(final) == col_name] <- paste0("Life_Expectancy_", substring(col_name, 1, 4))
    
  } 
}

#changing the name of .x.x.x.x in Inflation
for(col_name in names(final)){
  if (grepl(".x.x.x.x", col_name)  && 
      (!grepl("Country", col_name) && !grepl(".x.x.x.x.x", col_name) && !grepl(".y.y.y.y.y", col_name) 
       
      )){
    
    names(final)[names(final) == col_name] <- paste0("Inflation_", substring(col_name, 1, 4))
    
  } 
}

#changing the name of .x.x.x.x.x in _mortality_rate
for(col_name in names(final)){
  if (grepl(".x.x.x.x.x", col_name)  && 
      (!grepl("Country", col_name) && !grepl(".y.y.y.y.y", col_name) 
       
      )){
    
    names(final)[names(final) == col_name] <- paste0("Mortality_rate_", substring(col_name, 1, 4))
    
  } 
}

#changing the name of .y.y.y.y.y in _unemployment
for(col_name in names(final)){
  if (grepl(".y.y.y.y.y", col_name)  && 
      (!grepl("Country", col_name))){
    
    names(final)[names(final) == col_name] <- paste0("Unemployment_", substring(col_name, 1, 4))
    
  } 
}
#SCATTER PLOTS

for (year in 2016:2021) {
  selected_columns <- grep(as.character(year), names(final), value = TRUE)
  year_data <- final[, selected_columns, drop = FALSE]
  year_covariates <- year_data[, 1:8]
  
  num_columns <- ncol(year_covariates)
  
  par(mfrow = c(2, 4))
  
  for (i in 1:(num_columns)) {
    plot(year_covariates[, i], final[[paste0("Happiness_", year)]], 
         xlab = colnames(year_covariates)[i], ylab = paste0("Happiness_", year))
  }
  
}


#RUNNING THE REGRESSION

regression_results <- list()

# Loop through each year
for(year in 2016:2021) {
  
  economic_indicator_pattern <- paste0(".*_", year)
  
  independent_vars <- names(final)[grepl(economic_indicator_pattern, names(final)) & !grepl("Happiness", names(final))]

  dependent_var <- paste0("Happiness_", year)

  independent_vars_quoted <- sapply(independent_vars, function(x) paste0("`", x, "`"))
  formula_str <- paste("`", dependent_var, "` ~ ", paste(independent_vars_quoted, collapse = " + "), sep = "")
  
  formula <- as.formula(formula_str)
  
  # Run the regression
  regression_results[[as.character(year)]] <- lm(formula, data = final)
}

# Now, 'regression_results' contains the regression models for each year

# Loop through each model and print the summary
for(year in names(regression_results)) {
  cat("\nSummary for year:", year, "\n")
  print(summary(regression_results[[year]]))
}
#computing the average of the adjusted R-square values
adjusted_r_squared_values <- numeric()
for (year in 2016:2021) {
  
  model <- regression_results[[as.character(year)]]
  adjusted_r_squared_values <- c(adjusted_r_squared_values, summary(model)$adj.r.squared)
}

mean_adjusted_r_squared <- mean(adjusted_r_squared_values)
print(paste("Mean Adjusted R-squared:", mean_adjusted_r_squared))


#ANALIZING THE RESULTS

#Normality

# Loop through each regression model and plot a histogram of its residuals

par(mfrow = c(2, 3)) 

for(year in names(regression_results)) {
  model <- regression_results[[year]]
  residuals <- residuals(model)

  hist(residuals, breaks = 30, main = paste("Histogram of Residuals for Year", year), xlab = "Residuals")

  abline(v = mean(residuals), col = "red")
}


# Plotting residuals vs. real values

library(ggplot2)

for(year in names(regression_results)) {
  model <- regression_results[[year]]
  
  plot_data <- data.frame(Fitted = fitted(model), Residuals = residuals(model))
  
  p <- ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Residuals vs. Fitted Values for Year", year), 
         x = "Fitted Values", y = "Residuals")
  
  print(p)
}

#Mean of the residuals
mean_residuals <- numeric(length(regression_results))

for(i in seq_along(regression_results)) {
  model <- regression_results[[i]]
  mean_residuals[i] <- mean(residuals(model))
}

names(mean_residuals) <- names(regression_results)

# Print the mean residuals
print(mean_residuals)


# Shapiro-Wilk test
shapiro_wilk_test_results <- list()

for (year in names(regression_results)) {
  model <- regression_results[[year]]
  residuals <- residuals(model)
  
  shapiro_wilk_test_results[[year]] <- shapiro.test(residuals)
}

# Print Shapiro-Wilk test results
print("Shapiro-Wilk Test Results:")
print(shapiro_wilk_test_results)


#QQ-plots to verify conclusions form Shapiro_Wilk

par(mfrow = c(2, 3))  

for (year in names(regression_results)) {
  model <- regression_results[[year]]
  residuals <- residuals(model)
  
  qqnorm(residuals, main = paste("Residuals for Year", year), xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  qqline(residuals, col = 2)  # Add a reference line
  
  title(main = paste("Residuals for Year", year))
  title(xlab = "Theoretical Quantiles")
  title(ylab = "Sample Quantiles")
}

# MODEL SELECTION: step-up, step-down, and step-both

model_selection_summaries <- list()

for (year in 2016:2021) {
  
  model <- regression_results[[as.character(year)]]
  
  # Step-up method
  step_up_model <- step(model)
  model_selection_summaries[[paste("Step Up - Year", year)]] <- summary(step_up_model)
  
  # Step-down method
  step_down_model <- step(model, direction = "backward")
  model_selection_summaries[[paste("Step Down - Year", year)]] <- summary(step_down_model)
  
  # Step-both method
  step_both_model <- step(model, direction = "both")
  model_selection_summaries[[paste("Step Both - Year", year)]] <- summary(step_both_model)
}

# Print the summaries
for (summary_label in names(model_selection_summaries)) {
  cat("\nSummary for", summary_label, "\n")
  print(model_selection_summaries[[summary_label]])
}

#For every year, all three methods yield the same result. For a more compact and readable summary we 
#consider only the results of the step-up method, keeping track of how many times each covariate is added 

step_up_summaries <- list()
covariate_names <- unique(sub("_[0-9]+", "", names(final)[-1]))
covariate_count <- rep(0, length(covariate_names))



for (year in 2016:2021) {
  model <- regression_results[[as.character(year)]]
  
  step_up_model <- step(model)
  
  step_up_summaries[[paste("Step Up - Year", year)]] <- summary(step_up_model)
 
  terms_considered <- names(coef(step_up_model))

  for (i in seq_along(covariate_names)) {
    if (any(grepl(covariate_names[i], terms_considered))) {
      covariate_count[i] <- covariate_count[i] + 1
    }
  }
}

# Printing the summaries
for (summary_label in names(step_up_summaries)) {
  cat("\nSummary for", summary_label, "\n")
  print(step_up_summaries[[summary_label]])
}

# Printing the count for each covariate
for (i in seq_along(covariate_names)) {
  cat(covariate_names[i], "appeared in the step-up model", covariate_count[i], "times\n")
}

#Running the regression on the new model

exclude_strings <- c("Current_account", "Inflation")
final_filtered <- final[, !grepl(paste(exclude_strings, collapse = "|"), names(final))]

regression_results_filtered <- list()

for(year in 2016:2021) {
  
  economic_indicator_pattern <- paste0(".*_", year)
  independent_vars <- names(final_filtered)[grepl(economic_indicator_pattern, names(final_filtered)) & !grepl("Happiness", names(final_filtered))]
  dependent_var <- paste0("Happiness_", year)
  
  independent_vars_quoted <- sapply(independent_vars, function(x) paste0("`", x, "`"))
  formula_str <- paste("`", dependent_var, "` ~ ", paste(independent_vars_quoted, collapse = " + "), sep = "")
  
  formula <- as.formula(formula_str)
  
  #
  regression_results_filtered[[as.character(year)]] <- lm(formula, data = final_filtered)
}

# Now, 'regression_results_filtered' contains the regression models for each year with the modified dataset

# Loop through each model and print the summary
for(year in names(regression_results_filtered)) {
  cat("\nSummary for year:", year, "\n")
  print(summary(regression_results_filtered[[year]]))
}

#Computing the mean of the adjusted R-squared values for the new model
adjusted_r_squared_values_filtered <- numeric()

for (year in 2016:2021) {
  model_filtered <- regression_results_filtered[[as.character(year)]]
  
  adjusted_r_squared_values_filtered <- c(adjusted_r_squared_values_filtered, summary(model_filtered)$adj.r.squared)
}


mean_adjusted_r_squared_filtered <- mean(adjusted_r_squared_values_filtered)
print(paste("Mean Adjusted R-squared (Filtered Model):", mean_adjusted_r_squared_filtered))

#print the previous value as well for easy compariason
print(paste("Mean Adjusted R-squared:", mean_adjusted_r_squared))







#TESTING if non economic variables are more relevant than economic variables

# creating a vector with the percentage of the variance explained by economic covariates
# (we have a value for each covariate for each year, so that we find a result that is general and not
# related to a specific year)

economic_data <- final %>%
  select(contains("GDP_growth") | contains("Current_account") | contains("GDP_percapita") | contains("Inflation") | contains("Unemployement"))

num_columns <- ncol(economic_data)
exp_var_econ <- c()

for (j in 1:num_columns) {
  col_name <- colnames(economic_data)[j]
  
  for (i in 2016:2021) {
    if (grepl(as.character(i), col_name)) {
      model <- lm(economic_data[, j] ~ final[[paste0("Happiness_", as.character(i))]])
      exp_var_econ <- c(exp_var_econ, summary(model)$r.squared)
    }
  }
}

#creating the same vector for non-economic covariates
noneconomic_data <- final %>%
  select(contains("Prevalence_Undernourishment") | contains("Life_Expectancy") | contains("Mortality_Rate"))

num_columns <- ncol(noneconomic_data)
exp_var_nonecon <- c()

for (j in 1:num_columns) {
  col_name <- colnames(noneconomic_data)[j]
  
  for (i in 2016:2021) {
    if (grepl(as.character(i), col_name)) {
      model <- lm(noneconomic_data[, j] ~ final[[paste0("Happiness_", as.character(i))]])
      exp_var_nonecon <- c(exp_var_econ, summary(model)$r.squared)
    }
  }
}

# Now we will perform a t-test where our alternative Hypothesis is that the mean of the 
# percentages of explained variance of the non economic covariates is larger than the 
# one of economic variables

result <- t.test(exp_var_nonecon, exp_var_econ, alternative = "greater")
print(result)



#Performing t-test considering excluding prevalence undernourishment form non-economic variables
noneconomic_data_reduced <- final %>% select(contains("Life_Expectancy") | contains("Mortality_Rate"))
num_columns <- ncol(noneconomic_data_reduced)
exp_var_nonecon_reduced <- c()

for (j in 1:num_columns) {
  col_name <- colnames(noneconomic_data_reduced)[j]
  
  for (i in 2016:2021) {
    if (grepl(as.character(i), col_name)) {
      model <- lm(noneconomic_data_reduced[, j] ~ final[[paste0("Happiness_", as.character(i))]])
      exp_var_nonecon_reduced <- c(exp_var_nonecon_reduced, summary(model)$r.squared)
    }
  }
}
result <- t.test(exp_var_nonecon_reduced, exp_var_econ, alternative = "greater")
print(result)














