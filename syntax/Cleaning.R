source("./syntax/Data.R")

### assign group
# Load grouping data
groups <- read.xlsx("./data/powerplant_grouping.xlsx", fillMergedCells = TRUE) 
group <- unique(groups$Group)

# for donut
buffers = rev(c("0_25", "0_5", "1", "2", "3", "5"))
files <- paste0("./data/pplants/pplants_tract_",buffers,".csv")


# for host
buffers = rev(c("0", "1", "3"))
fi <- paste0("./data/pplants/pplants_tract_",buffers,".csv")
fi_c <- paste0("./data/pplants/pplants_county_",buffers,".csv")
# plant intersected with tracts including areas by buffer size


### donut area calculation
data <- data.frame()
for(i in 1:(length(files)-1)){
  
  dataa <- read_csv(files[i])[,-3]
  
  datab <- read_csv(files[i+1])[,-3] %>% 
    dplyr::select(Plant_Code,Prime_Mover,Fuel_Code,Status, FIPS, Area_Sq_Miles)
  
  dat <- dataa %>% 
    left_join(datab, by = c("Plant_Code","Prime_Mover","Fuel_Code","Status", "FIPS")) %>% 
    mutate(Area_Sq_Miles.y = ifelse(is.na(Area_Sq_Miles.y), 0, Area_Sq_Miles.y),
           Area_Sq_Miles = Area_Sq_Miles.x - Area_Sq_Miles.y) %>% # area difference
    dplyr::select(-Area_Sq_Miles.x, -Area_Sq_Miles.y)
  
  data <- rbind(dat, data)
  
  if(i == 5){
    data <- rbind(read_csv(files[6])[,-3], data)
  }
}



# weighted average
pw <- data %>% 
  dplyr::select(-ORIG_FID:-TRACT_FIPS,-POPULATION:-Shape_Area) %>% 
  mutate(Energy_Source =  paste(Prime_Mover, Fuel_Code, sep = "_")) %>% 
  left_join(groups %>% 
              dplyr::select(-Explanation), by = c("Energy_Source" = "Code")) %>% 
  left_join(groups %>% 
              dplyr::select(-Explanation), by = c("Fuel_Code" = "Code")) %>% 
  mutate(Group = coalesce(Group.x, Group.y)) %>% 
  dplyr::select(-Group.x, -Group.y) %>% 
  left_join(demo, by =c("FIPS" = "GEOID")) %>% 
  filter(Status == "OP") %>% 
  group_by(Plant_Code, Group, BUFF_DIST) %>%
  
  mutate(Total_Area = sum(Area_Sq_Miles, na.rm = TRUE),
         across(all_of(relevant_variables_demographics),
                list(w = ~weighted.mean(., Area_Sq_Miles, na.rm = TRUE)),
                .names = "{col}")) %>%
  
  ungroup() %>%
  dplyr::select(Plant_Code, Group, Total_Nameplate_Capacity_MW:FIPS,POPDEN:Total_Area) %>%
  dplyr::distinct(Plant_Code, Group, BUFF_DIST, .keep_all = TRUE)




### Host vs. non-host
ah <- data.frame()
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
          dplyr::select(Plant_Code,Prime_Mover,Fuel_Code,Status, FIPS) %>% 
          mutate(Energy_Source =  paste(Prime_Mover, Fuel_Code, sep = "_")) %>% 
          left_join(groups %>% 
                      dplyr::select(-Explanation), by = c("Energy_Source" = "Code")) %>% 
          left_join(groups %>% 
                      dplyr::select(-Explanation), by = c("Fuel_Code" = "Code")) %>% 
          mutate(Group = coalesce(Group.x, Group.y)) %>% 
          dplyr::select(-Group.x, -Group.y) %>% 
          filter(Group == group[j]) %>%  # regrouping plants
          filter(Status == "OP") # choose Status
        
        # join all the tracts with buffer intersected tracts, which are hosts
        dat <- dmo %>% 
          left_join(data, by = c("GEOID" = "FIPS")) %>% 
          mutate(Host = ifelse(is.na(Group), "N", "Y"),
                 Buff = buffers[i],
                 Group = group[j]) %>% 
          dplyr::select(-Plant_Code:-Fuel_Code,-Energy_Source, -Status) %>% 
          mutate(Area = area) %>% 
          dplyr::distinct(GEOID, .keep_all = TRUE) 
        
      }else{
        data <- read_csv(files[i])[,-3] %>% 
          dplyr::select(Plant_Code,Prime_Mover,Fuel_Code,Status, FIPS, Area_Sq_Miles) %>% 
          mutate(Energy_Source =  paste(Prime_Mover, Fuel_Code, sep = "_")) %>% 
          left_join(groups %>% 
                      dplyr::select(-Explanation), by = c("Energy_Source" = "Code")) %>% 
          left_join(groups %>% 
                      dplyr::select(-Explanation), by = c("Fuel_Code" = "Code")) %>% 
          mutate(Group = coalesce(Group.x, Group.y)) %>% 
          dplyr::select(-Group.x, -Group.y) %>% 
          filter(Group == group[j]) %>%  # regrouping plants
          filter(Status == "OP") # choose Status
        
        # join all the tracts with buffer intersected tracts, which are hosts
        dat <- dmo %>% 
          left_join(data, by = c("GEOID" = "FIPS")) %>% 
          mutate(Host = ifelse(is.na(Group), "N", "Y"),
                 Buff = buffers[i],
                 Group = group[j]) %>% 
          dplyr::select(-Plant_Code:-Fuel_Code,-Area_Sq_Miles:-Energy_Source, -Status) %>% 
          mutate(Area = area) %>% 
          dplyr::distinct(GEOID, .keep_all = TRUE) 
        
      }
      
      
      ah <- rbind(dat, ah)
    }
  }
  
}


### plant mapping
pm <- st_as_sf(read_csv(file = "./data/data.generators.csv"), coords = c("Longitude", "Latitude"), crs = 4269,
               agr = "constant",
               stringsAsFactors = FALSE,
               remove = TRUE) %>%
  filter(!`Plant Code` %in% c(50329, 58035, 59693, 63089, 65263)) %>% # remove off geometry plants  
  mutate(Energy_Source =  paste(`Prime Mover`, `Fuel Code`, sep = "_")) %>% 
  left_join(groups %>% 
              dplyr::select(-Explanation), by = c("Energy_Source" = "Code")) %>% 
  left_join(groups %>% 
              dplyr::select(-Explanation), by = c("Fuel Code" = "Code")) %>% 
  mutate(Group = coalesce(Group.x, Group.y)) %>% 
  dplyr::select(-Group.x, -Group.y) %>% 
  filter(Status == "OP") %>% 
  st_join(dem_m)

save(ah, pw, pm, file = "./data/derived/data.RData") 
