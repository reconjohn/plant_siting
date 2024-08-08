source("./syntax/Data.R")

# for donut
buffers = rev(c("0_25", "0_5", "1", "2", "3", "5"))
files <- paste0("./data/substation/substation_tract_",buffers,".csv")

# for host
buffers = rev(c("0", "1", "3"))
fi <- paste0("./data/substation/substation_tract_",buffers,".csv")
fi_c <- paste0("./data/substation/substation_county_",buffers,".csv")
# plant intersected with tracts including areas by buffer size

group = c("Not Available", "Under100", "100-161", "220-287", "Above345")


# substations intersected with tracts by buffer with area
# find area difference between consecutive buffers
# find weighted variables for each substation by buffer
### donut area calculation
data <- data.frame()
for(i in 1:(length(files)-1)){
  
  dataa <- read_csv(files[i])[,-3] %>% 
    filter(STATUS == "IN SERVICE")
  
  datab <- read_csv(files[i+1])[,-3] %>% 
    filter(STATUS == "IN SERVICE") %>% 
    dplyr::select(ID, FIPS, Area_Sq_Miles)
  
  dat <- dataa %>% 
    left_join(datab, by = c("ID", "FIPS")) %>% 
    mutate(Area_Sq_Miles.y = ifelse(is.na(Area_Sq_Miles.y), 0, Area_Sq_Miles.y),
           Area_Sq_Miles = Area_Sq_Miles.x - Area_Sq_Miles.y) %>% # area difference
    dplyr::select(-Area_Sq_Miles.x, -Area_Sq_Miles.y)
  
  data <- rbind(dat, data)
  
  if(i == 5){
    data <- rbind(read_csv(files[6])[,-3], data)
  }
}


# weighted average
pw_s <- data %>% 
  mutate(Group = ifelse(MAX_VOLT < 0, "Not Available", # voltage by N, 99, 161, 287, and 765
                       ifelse(MAX_VOLT < 100, "Under100",
                              ifelse(MAX_VOLT < 162, "100-161", 
                                     ifelse(MAX_VOLT < 288, "220-287", "Above345")))),
         Group = factor(Group, levels = group),
         
         BUFF_DIST = ifelse(BUFF_DIST > 8000, 5,
                            ifelse(BUFF_DIST > 4000, 3,
                                   ifelse(BUFF_DIST > 3000, 2,
                                          ifelse(BUFF_DIST > 1000, 1,
                                                 ifelse(BUFF_DIST > 800, 0.5, 0.25)))))) %>% 
  dplyr::select(ID, BUFF_DIST, FIPS, Area_Sq_Miles, Group) %>% 
  left_join(demo, by =c("FIPS" = "GEOID")) %>% 
  group_by(ID, Group, BUFF_DIST) %>%
  mutate(Total_Area = sum(Area_Sq_Miles, na.rm = TRUE),
         across(all_of(relevant_variables_demographics),
                list(w = ~weighted.mean(., Area_Sq_Miles, na.rm = TRUE)),
                .names = "{col}")) %>%
  
  ungroup() %>%
  dplyr::distinct(ID, Group, BUFF_DIST, .keep_all = TRUE) 


# demo data by tract joined by substation 
# find host vs. non-host
### Host vs. non-host
ah_s <- data.frame()
for(k in 1:2){
  for(i in 1:(length(fi))){
    for(j in 1:length(group)){
      if(k == 1){
        files <- fi
        area <- "Tract"
        dmo <- demo
      }else{
        files <- fi_c
        area <- "County"
        dmo <- demo_c
      }
      
      if(buffers[i] == "0"){
        data <- read_csv(files[i])[,-3] %>% 
          mutate(Group = ifelse(MAX_VOLT < 0, "Not Available", # voltage by N, 99, 161, 287, and 765
                               ifelse(MAX_VOLT < 100, "Under100",
                                      ifelse(MAX_VOLT < 162, "100-161", 
                                             ifelse(MAX_VOLT < 288, "220-287", "Above345")))),
                 Group = factor(Group, levels = group)) %>% 
                 
          
          dplyr::select(ID, Group, FIPS) %>% 
          filter(Group == group[j]) 
        
        # join all the tracts with buffer intersected tracts, which are hosts
        dat <- dmo %>% 
          left_join(data, by = c("GEOID" = "FIPS")) %>% 
          mutate(Host = ifelse(is.na(ID), "N", "Y"),
                 Buff = buffers[i],
                 Group = group[j]) %>% 
          dplyr::select(-ID) %>% 
          mutate(Area = area) %>% 
          dplyr::distinct(GEOID, .keep_all = TRUE) 
        
      }else{
        data <- read_csv(files[i])[,-3] %>% 
          mutate(Group = ifelse(MAX_VOLT < 0, "Not Available", # voltage by N, 99, 161, 287, and 765
                                ifelse(MAX_VOLT < 100, "Under100",
                                       ifelse(MAX_VOLT < 162, "100-161", 
                                              ifelse(MAX_VOLT < 288, "220-287", "Above345")))),
                 Group = factor(Group, levels = group)) %>% 
          
          dplyr::select(ID, Group, FIPS, Area_Sq_Miles) %>% 
          filter(Group == group[j]) 
        
        # join all the tracts with buffer intersected tracts, which are hosts
        dat <- dmo %>% 
          left_join(data, by = c("GEOID" = "FIPS")) %>% 
          mutate(Host = ifelse(is.na(ID), "N", "Y"),
                 Buff = buffers[i],
                 Group = group[j]) %>% 
          dplyr::select(-ID, -Area_Sq_Miles) %>% 
          mutate(Area = area) %>% 
          dplyr::distinct(GEOID, .keep_all = TRUE) 
        
      }
      
      
      ah_s <- rbind(dat, ah_s)
    }
  }
}

# substation joined by demo data
### plant mapping
pm_s <- st_as_sf(read_csv(file = "./data/substation/substation_tract_0.csv"), 
               coords = c("LONGITUDE", "LATITUDE"), crs = 4269,
               agr = "constant",
               stringsAsFactors = FALSE,
               remove = TRUE) %>% 
  filter(STATUS == "IN SERVICE") %>% 
  

  mutate(Group = ifelse(MAX_VOLT < 0, "Not Available", # voltage by N, 99, 161, 287, and 765
                        ifelse(MAX_VOLT < 100, "Under100",
                               ifelse(MAX_VOLT < 162, "100-161", 
                                      ifelse(MAX_VOLT < 288, "220-287", "Above345")))),
         Group = factor(Group, levels = group)) %>% 
  
  st_join(dem_m)


save(pw_s, ah_s, pm_s, file = "./data/derived/data_s.RData") 
