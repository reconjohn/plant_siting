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
library(purrr)
library(tidycensus)
library(caret)

sf::sf_use_s2(FALSE)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)



state_lookup <- data.frame(
  state_name = state.name,
  STATE_ABBR = state.abb,
  stringsAsFactors = FALSE
)

all_states <- unique(fips_codes$state)[1:51]  # Excludes territories

# Set parameters
year <- 2021
geometry <- TRUE

# Pull ACS data for all states and bind into one sf object
tr.sf <- map_df(all_states, function(state_code) {
  get_acs(
    geography = "tract",
    variables = "B01003_001",  # Total population
    state = state_code,
    year = year,
    geometry = geometry
  )
}) %>% 
  mutate(state = word(NAME, -1)) %>% 
  left_join(state_lookup, by = c("state" = "state_name"))


st <- tr.sf %>% 
  group_by(state) %>% 
  summarise(geometry = st_union(geometry))


### DAC
DAC <- read_csv("https://github.com/reconjohn/disadvantaged_communities/raw/main/results/DAC_s.csv")

# DAC spatial
dis <- tr.sf %>% 
  filter(!STATE_ABBR %in% c("AK","HI")) %>% 
  # rename(GEOID = FIPS) %>% 
  dplyr::select(GEOID) %>% 
  left_join(DAC %>% 
              dplyr::select(GEOID, disadvantaged), by = "GEOID") 

dis1 <- tr.sf %>% 
  filter(!STATE_ABBR %in% c("AK","HI")) %>% 
  # rename(GEOID = FIPS) %>% 
  dplyr::select(GEOID, estimate) %>% 
  left_join(DAC %>% 
              dplyr::select(GEOID, disadvantaged), by = "GEOID") 

relevant_variables_demographics = c("LOWINCOME",
                                    "LESSHS",
                                    "PEOPCOLOR",
                                    "LINGISO",
                                    "UNEMPLOYED",
                                    "UNDER5", # relevant demographics variables
                                    "OVER64", 
                                    "POPDEN")
vec <- c("Low\nincome (%)","Less than\nhigh school\neducation (%)","People of\ncolor (%)","Population\ndensity\n(100 ppsm)", 
         "Over 64\nyears old (%)","Under 5\nyears old (%)","Unemployed (%)","Difficulty with\nEnglish (%)")


### join demo data
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
  # dplyr::rename(GEOID = FIPS) %>% 
  dplyr::select(GEOID) %>% 
  left_join(demo, by = "GEOID") %>% 
  dplyr::select(GEOID,population, relevant_variables_demographics)

group <- c("Coal","Natural Gas","Oil",
           "Biomass","Hydroelectric","Nuclear",
           "Solar","Wind","Batteries","Storage",
           "Others",
           "Substations", "Transmission")
group_g <- c("Coal","Natural Gas","Oil",
             "Biomass","Hydroelectric","Nuclear",
             "Solar","Wind","Batteries","Storage",
             "Others")
group_s <- c("Not Available","Under 100 kV","100-161 kV","220-287 kV",
             "Above 345 kV")
group_t <- c("Not Available","Under 100 kV","100-161 kV","220-287 kV",
             "Above 345 kV & DC")

# source: ACS 2021; EPA EJ screening Tool; EIA 2021
# pw: donut shape weighted demo features by buffer
# ah: host identification for plant by buffer size with DAC joined
# pm: plant mapping
load("./data/data.RData")
pw <- pw %>% 
  mutate(Group = ifelse(substr(Group, 1, 2) == "NG", "Natural Gas", Group)) %>% 
  mutate(Group = ifelse(Group == "Bio", "Biomass", 
                        ifelse(Group == "Hydro", "Hydroelectric", 
                               ifelse(Group == "Petroleum", "Oil", Group)))) 
ah <- ah %>% 
  mutate(Group = ifelse(substr(Group, 1, 2) == "NG", "Natural Gas", Group)) %>% 
  mutate(Group = ifelse(Group == "Bio", "Biomass", 
                        ifelse(Group == "Hydro", "Hydroelectric", 
                               ifelse(Group == "Petroleum", "Oil", Group)))) 
pm <- pm %>% 
  mutate(Group = ifelse(substr(Group, 1, 2) == "NG", "Natural Gas", Group)) %>% 
  mutate(Group = ifelse(Group == "Bio", "Biomass", 
                        ifelse(Group == "Hydro", "Hydroelectric", 
                               ifelse(Group == "Petroleum", "Oil", Group))))

# pw_s: donut shape weighted demo features by buffer
# ah_s: host identification for plant by buffer size with dis joined
# pm_s: plant mapping

load("./data/data_s.RData")
load("./data/TL.RData") # tl_d, ah_t
tl_d <- tl_d %>% 
  dplyr::select(GEOID:buff,Group) %>% 
  mutate(Group = ifelse(Group == "Under 100kV", "Under 100 kV",
                        ifelse(Group == "100-161kV", "100-161 kV",
                               ifelse(Group == "220-287kV", "220-287 kV",
                                      ifelse(Group == "Above 345kV & DC", "Above 345 kV & DC", "Not Available")))),
         Group = factor(Group, levels = group_t)) %>% 
  mutate(buff = ifelse(buff > 8000, 5,
                       ifelse(buff > 4000, 3,
                              ifelse(buff > 3000, 2,
                                     ifelse(buff > 1000, 1,
                                            ifelse(buff > 800, 0.5, 0.25)))))) 

ah_t <- ah_t %>% 
  mutate(Group = ifelse(Group == "Under 100kV", "Under 100 kV",
                        ifelse(Group == "100-161kV", "100-161 kV",
                               ifelse(Group == "220-287kV", "220-287 kV",
                                      ifelse(Group == "Above 345kV & DC", "Above 345 kV & DC", "Not Available")))),
         Group = factor(Group, levels = group_t)) 



# transmission lines
tl <- st_read("./data/TL.shp")%>% 
  rename(Group = VOLT_CLASS) %>%
  mutate(Group = ifelse(Group == "NOT AVAILABLE", "Not Available",
                        ifelse(Group == "UNDER 100", "Under 100 kV", 
                               ifelse(Group == "100-161", "100-161 kV", 
                                      ifelse(Group == "220-287", "220-287 kV", "Above 345 kV & DC")))),
         Group = factor(Group, levels = group_t)) %>% 
  st_transform(4269) %>% 
  dplyr::select(Group)

tl_dis <- tl %>% 
  st_intersection(dis %>% 
                    filter(disadvantaged == "TRUE"))%>% 
  dplyr::select(-GEOID, -disadvantaged)

tl_nondis <- tl %>% 
  st_intersection(dis %>% 
                    filter(disadvantaged == "FALSE"))%>% 
  dplyr::select(-GEOID, -disadvantaged)

tl_tot <- tl_nondis %>% 
  mutate(dis = FALSE) %>% 
  rbind(tl_dis %>% 
          mutate(dis = TRUE)) %>% 
  mutate(disadvantaged = ifelse(dis == T, "Disadvantaged community", 
                                "Not a disadvantaged community")) %>% 
  dplyr::select(-dis) 


### CV regression
regr <- function(data, group, area, buffer){
  
  dat <- data %>% 
    filter(population > 0) %>% 
    filter(Area == area) %>%
    filter(Buff == buffer) %>%
    filter(Group == group) %>%
    mutate(POPDEN = log(POPDEN)) %>% 
    dplyr::select(relevant_variables_demographics, Host) %>%
    mutate(across(where(is.numeric) & !c("Host"), ~ scale(.))) %>% 
    mutate(Host = factor(Host, levels = c("N", "Y")))
  
  # for individual CV
  cfm <- function(data, lev = NULL, model = NULL) {
    cm <- confusionMatrix(table(data$pred, data$obs))
    cm$byClass
  }
  ctrlspecs <- trainControl(method = "cv", number = 10,
                            savePredictions = "all",
                            returnResamp = 'all',
                            classProbs = TRUE,
                            summaryFunction = cfm) #twoClassSummary
  
  fit <- train(Host ~ ., data=dat, 
               method="glm", 
               family=binomial, 
               trControl=ctrlspecs)
  
  summary(fit)$coefficients %>% 
    as.data.frame() %>% 
    mutate(Sig = ifelse(`Pr(>|z|)` < 0.05, "Y", "N")) %>%
    dplyr::select(-"z value",-"Pr(>|z|)") %>% 
    tibble::rownames_to_column("var") %>% 
    filter(!var == "(Intercept)") %>% 
    rename(se = "Std. Error",
           pe = Estimate) %>% 
    mutate(Group = group,
           Buffer = buffer,
           Area = area,
           var = factor(var, levels = c(relevant_variables_demographics))) 
}


### for capacity
regr1 <- function(data, group){
  
  dat <- data %>% 
    filter(POPDEN > 0) %>% 
    filter(BUFF_DIST == 0.25) %>%
    filter(Group == group) %>%
    left_join(dis, by = c("FIPS" = "GEOID")) %>% 
    mutate(POPDEN = log(POPDEN)) %>% 
    dplyr::select(relevant_variables_demographics, Total_Nameplate_Capacity_MW,disadvantaged,Year) %>%
    dplyr::rename(DAC = disadvantaged,
                  cap = Total_Nameplate_Capacity_MW) %>% 
    mutate(across(where(is.numeric) & !c("cap","DAC"), ~ scale(.))) %>% 
    mutate(DAC = ifelse(DAC == T, 1, 0)) 
  
  fit <- lm(cap ~ ., data = dat)
  
  summary(fit)$coefficients %>% 
    as.data.frame() %>% 
    mutate(Sig = ifelse(`Pr(>|t|)` < 0.05, "Y", "N")) %>%
    dplyr::select(-"t value",-"Pr(>|t|)") %>% 
    tibble::rownames_to_column("var") %>% 
    filter(!var == "(Intercept)") %>% 
    rename(se = "Std. Error",
           pe = Estimate) %>% 
    mutate(Group = group,
           var = factor(var, levels = c(relevant_variables_demographics, "Year","DAC"))) 
}
