library(tidyverse)
library(cowplot)

## Code carried over from previous challenge -----

BOM_data <- read_csv("data/BOM_data.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")

# Tidy the BOM_data file and convert measurements into numeric values
BOM_with_temps <- BOM_data %>% 
  separate(Temp_min_max, into = c("t_min", "t_max"), sep = "/") %>% 
  mutate(
    t_min          = as.numeric(t_min),
    t_max          = as.numeric(t_max),
    Rainfall       = as.numeric(Rainfall),
    Solar_exposure = as.numeric(Solar_exposure)
  )

# Tidy the BOM_stations metadata by reshaping the dataframe 
stations_tidy <- BOM_stations %>% 
  gather(key = "Station_number", value = "values", -info) %>% 
  spread(key = info, value = values)

# Plotting challenge code 
#
# For this challenge I will be using the default scales and themes to make the plot construction
# code clearer.
#
# I encourage you to modify your own code in order to make your plots more presentable.

## Question 1: -----
# For the Perth station (ID 9225), produce three scatter plots showing the relationship between the 
# maximum temperature and each other measurement recorded (minimum temperature, rainfall and 
# solar exposure). 

perth_data <- BOM_with_temps %>% 
  filter(Station_number == 9225)

q1_a <- perth_data %>% 
  ggplot(mapping = aes(x = t_max, y = t_min)) +
  geom_point(alpha = 0.2)

q1_b <- perth_data %>% 
  ggplot(mapping = aes(x = t_max, y = Rainfall)) +
  geom_point(alpha = 0.2)

q1_c <- perth_data %>% 
  ggplot(mapping = aes(x = t_max, y = Solar_exposure)) +
  geom_point(alpha = 0.2)

ggsave("figures/q1_tmax_v_tmin.png", q1_a, width = 5, height = 5)
ggsave("figures/q1_tmax_v_rainfall.png", q1_b, width = 5, height = 5)
ggsave("figures/q1_tmax_v_solar.png", q1_c, width = 5, height = 5)

## Question 2: -----
# Display these four measurements for the Perth station in a single scatter plot by using 
# additional aesthetic mappings.

q2 <- perth_data %>% 
  ggplot(mapping = aes(x = t_max, y = t_min, size = Rainfall, colour = Solar_exposure)) +
  geom_point(alpha = 0.2)

ggsave("figures/q2_all_measurements.png", q2, width = 5, height = 5)
