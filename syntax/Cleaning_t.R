# Transmission line

group = c("Not Available", "Under100", "100-161", "220-287", "Above345 & DC")
buffers = c(0, 1, 3)

ah_t <- data.frame()
for(k in 1:2){
  for(j in 1:length(group)){
    for(i in 1:length(buffers)){
    if(k == 1){
      area <- "Tract"
      data <- tl_d 
      dmo <- demo
    }else{
      area <- "County"
      dmo <- demo_c
      data <- tl_d %>% 
        mutate(GEOID = substr(GEOID, start = 1, stop = 5)) 
    }
    
    data <- data %>% 
      filter(Group == group[j]) %>% 
      mutate(buff = ifelse(buff > 8000, 5,
                           ifelse(buff > 4000, 3,
                                  ifelse(buff > 3000, 2,
                                         ifelse(buff > 1000, 1,
                                                ifelse(buff > 800, 0.5, 
                                                       ifelse(buff > 0, 0.25, buff))))))) %>% 
      filter(buff == buffers[i]) %>% 
      group_by(GEOID) %>% 
      summarise(Area = sum(area))
    
    # join all the tracts with buffer intersected tracts, which are hosts
    dat <- dmo %>% 
      left_join(data, by = "GEOID") %>% 
      mutate(Host = ifelse(is.na(Area), "N", "Y"),
             Group = group[j],
             Buff = buffers[i],
             Area = area) %>% 
      mutate(Area = area)
    
    ah_t <- rbind(dat, ah_t)
  }
}
}

# save(ah_t, file = "./data/derived/TL.RData") 
