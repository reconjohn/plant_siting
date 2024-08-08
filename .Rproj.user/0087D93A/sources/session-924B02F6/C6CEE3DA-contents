# libraries
library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(stringdist)
library(tidyverse)
library(openxlsx)
library(readr)
library(scales)
library(sf)
library(stargazer)
library(Hmisc)
library(ggpubr)
library(mapview)
library(leafsync)

sf::sf_use_s2(FALSE)
load("C:/Users/yohan/Big_data/US Tract/census.Rdata") #tr.sf, st

DAC <- read_csv("https://github.com/reconjohn/disadvantaged_communities/raw/main/results/DAC_s.csv")

dis <- tr.sf %>% 
  rename(GEOID = FIPS) %>% 
  dplyr::select(GEOID) %>% 
  left_join(DAC %>% 
              dplyr::select(GEOID, disadvantaged), by = "GEOID") 


relevant_variables_demographics = c("POPDEN",
                                    "OVER64", 
                                    "UNDER5", # relevant demographics variables
                                    "LINGISO",
                                    "PEOPCOLOR",
                                    "UNEMPLOYED", 
                                    "LOWINCOME",
                                    "LESSHS")


# join demo data
demo <- read_csv("./data/demographic_df_feats_tract.csv") %>%
  mutate(GEOID = ifelse(GEOID < 10001040100, paste0("0",GEOID), GEOID) %>%
           as.character()) %>% 
  dplyr::rename(POPDEN =populationdensity) %>% 
  dplyr::select(GEOID, population, POPDEN)


dem <- DAC %>% 

  mutate(PEOPCOLOR = PEOPCOLORPCT,
         LOWINCOME = LOWINCPCT,
         UNEMPLOYED = UNEMPPCT,
         LINGISO = LINGISOPCT,
         LESSHS = LESSHSPCT,
         UNDER5 = UNDER5PCT,
         OVER64 = OVER64PCT) %>% 
  
  dplyr::select(GEOID, PEOPCOLOR,LOWINCOME,UNEMPLOYED, LINGISO,LESSHS,UNDER5,OVER64)

demo <- demo %>%
  left_join(dem, by = "GEOID")



demo_c <- read_csv("./data/demographic_df_feats_county.csv") %>% 
  mutate(GEOID = ifelse(GEOID < 10001, paste0("0",GEOID), GEOID) %>%
           as.character()) %>% 
  dplyr::rename(POPDEN =populationdensity) %>% 
  dplyr::select(GEOID, population, POPDEN) %>% 
  
  left_join(DAC %>% 
              mutate(GEOID = substr(GEOID, start = 1, stop = 5)) %>% 
              dplyr::select(GEOID, PEOPCOLORPCT,LOWINCPCT,UNEMPPCT,LINGISOPCT,LESSHSPCT,UNDER5PCT,OVER64PCT),
            by = "GEOID") %>% 

  group_by(GEOID) %>% 

  dplyr::summarise(PEOPCOLOR = weighted.mean(PEOPCOLORPCT, population, rm.na = T),
            LOWINCOME = weighted.mean(LOWINCPCT, population, rm.na = T),
            UNEMPLOYED = weighted.mean(UNEMPPCT, population, rm.na = T),
            LINGISO = weighted.mean(LINGISOPCT, population, rm.na = T),
            LESSHS = weighted.mean(LESSHSPCT, population, rm.na = T),
            UNDER5 = weighted.mean(UNDER5PCT, population, rm.na = T),
            OVER64 = weighted.mean(OVER64PCT, population, rm.na = T),
            population = mean(population),
            POPDEN = mean(POPDEN)) %>%
  
  dplyr::select(GEOID,population, POPDEN, PEOPCOLOR,LOWINCOME,UNEMPLOYED, LINGISO,LESSHS,UNDER5,OVER64)


# for mapping
dem_m <- tr.sf %>% 
  dplyr::rename(GEOID = FIPS) %>% 
  dplyr::select(GEOID) %>% 
  left_join(demo, by = "GEOID") %>% 
  dplyr::select(GEOID,population, relevant_variables_demographics)

