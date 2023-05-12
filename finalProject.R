library(moderndive)
library(infer)
library(tidyverse)

population = read.csv("Population2020.csv")
videogames = read.csv("VideoGames_Sales.csv")

# Remove outliers based on the previous boxplot
videogames <- videogames %>% 
  filter(Critic_Score > 3)
# Segment games into 3 critic score categories
videogames <- videogames %>% 
  mutate(score_cat = ifelse(Critic_Score < 6.5, "Low", ifelse(Critic_Score < 8,"Medium","High")))

########################################################

# NOT ENOUGH DATA TO FILTER ALL RPG, RATING = M, MEDIUM CRIT SCORE, P-VALUE IS TOO HIGH

# filtering for RPG games
videogames_rpg = videogames %>% 
  filter(Genre == "Role-Playing")

# Linear regression for sales in Japan 
lm_sales_JP = lm(JP_Sales~Year, videogames_rpg)
get_regression_table(lm_sales_JP)

videogames_rpg %>% 
  ggplot(aes(x = Year, y = JP_Sales)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## 15 - .008 * Year
## to predict 2022: 17.1 - .008(2022) = .924

# Linear regression for sales in Europe 
lm_sales_EU = lm(EU_Sales~Year, videogames_rpg)
get_regression_table(lm_sales_EU)

videogames_rpg %>% 
  ggplot(aes(x = Year, y = EU_Sales)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## -25.8 + .013 * Year
## to predict 2022: -25.8 + .013(2022) = .486


########################################################


# filtering for M games
videogames_m = videogames %>% 
  filter(Rating == "M")

# Linear regression for sales in Japan 
lm_sales_JP_2 = lm(JP_Sales~Year, videogames_m)
get_regression_table(lm_sales_JP_2)

videogames_m %>% 
  ggplot(aes(x = Year, y = JP_Sales)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## 6.43 - .003 * Year
## to predict 2022: 6.43 - .003(2022) = .364

# Linear regression for sales in Europe 
lm_sales_EU_2 = lm(EU_Sales~Year, videogames_m)
get_regression_table(lm_sales_EU_2)

videogames_m %>% 
  ggplot(aes(x = Year, y = EU_Sales)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## -75.2 + .038 * Year
## to predict 2022: -75.2 + .038(2022) = 1.636

## *Important to note that in both cases, intercept does not make sense 
## because it is impossible for year to be 0


########################################################

## Everything below this did not fit in the presentation

## H0: pc games have lower NA Sales than xbox1/ps4 games
## HA: pc games have higher NA Sales than xbox1/ps4 games

## I filtered data for only PC's, xbox one's and PS4's because these platforms are
## primarily what game companies release their games on nowadays
videogames_adj = videogames %>% 
  filter(Platform == "PC" | Platform == "XOne" | Platform == "PS4")

videogames_adj = videogames_adj %>% 
  mutate(console = ifelse(Platform == "PC", "PC", "Console"))

obs_sales = videogames_adj %>% 
  specify(NA_Sales~console) %>% 
  calculate(stat = "diff in means", order = c("PC", "Console"))

null_sales = videogames_adj %>% 
  specify(NA_Sales~console) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat = "diff in means", order = c("PC", "Console"))

get_p_value(null_sales, obs_sales, direction = "right")

visualize(null_sales) + 
  shade_p_value(obs_stat = obs_sales, direction = "right")

sales_dist = videogames_adj %>% 
  specify(NA_Sales~console) %>% 
  generate(reps = 1000) %>% 
  calculate(stat = "diff in means", order = c("PC", "Console"))
sales_ci <- sales_dist %>% 
  get_confidence_interval(type="percentile")
sales_ci


## p-value is 0 so we should reject H0 meaning that pc games have higher NA_sales
## than xbox
## confidence interval also is negative indicating that pc games have higher NA_sales 
## than xbox so the business recommendation for this is to release games on PC


