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
