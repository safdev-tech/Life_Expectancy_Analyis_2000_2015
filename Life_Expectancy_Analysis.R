# Hypothesis: Health and socio-economic factors influence life expectancy

# Project goal: Investigate life expectancy patterns across countries, trends over time and dispartiies across developed and developing countries

# Installing packages 
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("kableExtra")

# Loading the packages
library(tidyverse)
library(ggplot2)
library(kableExtra)

# Reading and previewing the data
life_exp <- read.csv("life_expectancy.csv")

## Showing first 5 rows
head(life_exp, 5)

# Data cleaning, exploration and distribution statistics

## Renaming table columns
names(life_exp) <- c("Country", "Year", "Status", "Population", "Hepatitis_B", "Measles", 
                     "Polio", "Diphtheria", "HIV_AIDS", "Infant_Deaths", "Under_Five_Deaths",
                     "Total_Expenditure", "GDP", "BMI", "Thinness_1_19_Years", "Alcohol",
                     "Schooling", "Life_Expectancy")

## Show data summary statistics - (optional)
summary(life_exp)

## Changing data types for analysis - Note: check whether to convert year as well
life_exp <- life_exp %>%
  mutate(
    Population = as.numeric(Population),
    Status = as.factor(Status),
    Hepatitis_B = as.numeric(Hepatitis_B),
    Measles = as.numeric(Measles),
    Polio = as.numeric(Polio),
    Diphtheria = as.numeric(Diphtheria),
    HIV_AIDS = as.numeric(HIV_AIDS),
    Infant_Deaths = as.numeric(Infant_Deaths),
    Under_Five_Deaths = as.numeric(Under_Five_Deaths),
    Total_Expenditure = as.numeric(Total_Expenditure),
    BMI = as.numeric(BMI),
    Thinness_1_19_Years = as.numeric(Thinness_1_19_Years),
    Alcohol = as.numeric(Alcohol)
  )

## Creating a grouped Health Indicator column - Note: Should review the dataset to agree on what grouping to do for health indicators or whether to subset it
life_exp_new <- life_exp %>%
  mutate(Health_Indicators = rowMeans(cbind(Hepatitis_B, Measles, Polio, Diphtheria,
    HIV_AIDS,  # Negative because higher values are worse
    -Infant_Deaths, -Under_Five_Deaths, BMI, -Thinness_1_19_Years), na.rm = TRUE)
  ) %>%
  # Removing individual columns that were grouped
  select(-c(Hepatitis_B, Measles, Polio, Diphtheria, HIV_AIDS, Infant_Deaths, Under_Five_Deaths, BMI, Thinness_1_19_Years))

# Display the first few rows of the new dataframe
print(head(life_exp_new))

# Checking the cleaned data - (optional)
summary(life_exp)


## Average life expectancy over years
life_exp %>%
  group_by(Year) %>%
  summarise(avg_life_expectancy = mean(Life_Expectancy, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = avg_life_expectancy)) +
  geom_line() +
  geom_point() +
  labs(title = "Global Average Life Expectancy Over Time",
       x = "Year",
       y = "Average Life Expectancy (years)")

## Plotting count of countries per life expectancy based on status
ggplot(life_exp, aes(x = Life_Expectancy, fill = Status)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Life Expectancy Histogram by Country Status",
       x = "Life Expectancy (years)")

## Comparing life expectancy between developed and developing countries
ggplot(life_exp, aes(x = Status, y = Life_Expectancy, fill = Status)) +
  geom_boxplot() +
  labs(title = "Life Expectancy Distribution by Country Status",
       y = "Life Expectancy (years)",
       x = "Country Status") +
  theme(legend.position = "none")

## Countries with the highest life expectancy per year

highest_life_expectancy_per_year <- life_exp %>% 
  group_by(Year) %>% 
  filter(Life_Expectancy == max(Life_Expectancy, na.rm = TRUE)) %>% 
  select(Country, Life_Expectancy, Year, Status)
print(highest_life_expectancy_per_year)

ggplot(highest_life_expectancy_per_year, aes(x = Year, y = Life_Expectancy, fill = Country)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Highest Life Expectancy Per Year by Country",
       x = "Year",
       y = "Life Expectancy (years)",
       fill = "Country")

## Countries with the lowest life expectancy per year
lowest_life_expectancy_per_year <- life_exp %>% 
  group_by(Year) %>% 
  filter(Life_Expectancy == max(Life_Expectancy, na.rm = TRUE)) %>% 
  select(Country, Life_Expectancy, Year, Status)
print(lowest_life_expectancy_per_year)

ggplot(lowest_life_expectancy_per_year, aes(x = Year, y = Life_Expectancy, fill = Country)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Lowest Life Expectancy Per Year by Country",
       x = "Year",
       y = "Life Expectancy (years)",
       fill = "Country")

## Average life expectancy by country and region

## Life expectancy over time in France

## Comparing life expectancy in France with that of highest and lowest life expectancy

## World map of life expectancy - (which year? > 2023?)


# Data analysis

## Create a summary statistics table for variables of interest

summary_stats <- data.frame(
  Variable = c("Life Expectancy", "Schooling", "GDP", "Total Expenditure", "Population", "Health Indicators"),
  Mean = c(
    mean(life_exp$Life_Expectancy, na.rm = TRUE),
    mean(life_exp$Health_Indicators, na.rm = TRUE),
    mean(life_exp$Schooling, na.rm = TRUE),
    mean(life_exp$GDP, na.rm = TRUE),
    mean(life_exp$Total_Expenditure, na.rm = TRUE),
    mean(life_exp$Population, na.rm = TRUE)
  ),
  Median = c(
    median(life_exp$Life_Expectancy, na.rm = TRUE),
    median(life_exp$Health_Indicators, na.rm = TRUE),
    median(life_exp$Schooling, na.rm = TRUE),
    median(life_exp$GDP, na.rm = TRUE),
    median(life_exp$Total_Expenditure, na.rm = TRUE),
    median(life_exp$Population, na.rm = TRUE)
  ),
  StD = c(
    sd(life_exp$Life_Expectancy, na.rm = TRUE),
    sd(life_exp$Health_Indicators, na.rm = TRUE),
    sd(life_exp$Schooling, na.rm = TRUE),
    sd(life_exp$GDP, na.rm = TRUE),
    sd(life_exp$Total_Expenditure, na.rm = TRUE),
    sd(life_exp$Population, na.rm = TRUE)
  )
)

## Formatting the table with kableExtra
summary_stats %>%
  kbl(caption = "Summary Statistics of Key Variables",
      digits = 2,
      format.args = list(big.mark = ",")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, bold = TRUE)

## Life expectancy vs Health Indicators
ggplot(life_exp, aes(x = Health_Indicators, y = Life_Expectancy)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = paste("Correlation between Health Indicators and Life Expectancy\
       x = "Health Indicators",
       y = "Life Expectancy (years)")

## Life expectancy vs population size
ggplot(life_exp, aes(x = Population, y = Life_Expectancy)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  scale_x_log10(labels = scales::comma) + 
  labs(title = paste("Correlation between Population and Life Expectancy\
       x = "Population (log scale)",
       y = "Life Expectancy (years)")

## Life Expectancy vs GDP
ggplot(life_exp, aes(x = log(GDP), y = Life_Expectancy)) +
  geom_point(alpha = 0.3, aes(color = Status)) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Life Expectancy vs GDP (log scale)",
       x = "Log(GDP)",
       y = "Life Expectancy (years)")

## Life Expectancy vs Schooling
ggplot(life_exp, aes(x = Schooling, y = Life_Expectancy)) +
  geom_point(alpha = 0.3, aes(color = Status)) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Life Expectancy vs GDP (log scale)",
       x = "Schooling",
       y = "Life Expectancy (years)")

## Creating scatter plot of life expectancy vs schooling styled by Status
ggplot(df_clean, aes(x = Schooling, y = Life_Expectancy, color = Status)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = Status), alpha = 0.2) +
  scale_color_manual(values = c("Developed" = "#2E86C1", "Developing" = "#E74C3C")) +
  scale_fill_manual(values = c("Developed" = "#2E86C1", "Developing" = "#E74C3C")) +
  labs(title = "Relationship between Schooling and Life Expectancy by Country Status",
       subtitle = "With linear regression lines showing trends",
       x = "Schooling (years)",
       y = "Life Expectancy (years)",
       color = "Country Status",
       fill = "Country Status") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )


# Conclusion
# On average, developed countries have a higher life expectancy than developing countries
# Socio-economic factors such as education and GPD have a positive relationship life expectancy, while population may not have a strong relationship
