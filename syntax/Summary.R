source("./syntax/Data.R")

# source: ACS 2021; EPA EJ screening Tool; EIA 2021
# pw: donut shape weighted demo features by buffer
# ah: host identification for plant by buffer size with dis joined
# pm: plant mapping
load("./data/derived/data.RData")


# pw_s: donut shape weighted demo features by buffer
# ah_s: host identification for plant by buffer size with dis joined
# pm_s: plant mapping
load("./data/derived/data_s.RData")

# tl_d, ah_t
load("./data/derived/TL.RData") 


map <- pm %>% 
  mutate(Type = ifelse(Group %in% c("Coal","Petroleum","NGCC","NGCT","NGIC","NGST"), "Fossil Fuel",
                        ifelse(Group %in% c("Solar","Wind","Bio","Hydro"), "Renewable", "Others")),
         Type = factor(Type, levels = c("Fossil Fuel", "Renewable", "Others"))) %>% 
  
  left_join(dis %>% 
              st_drop_geometry(), by = "GEOID") %>% 
  filter(!is.na(disadvantaged)) %>% 
  
  mutate(disadvantaged = ifelse(disadvantaged == "TRUE", "Disadvantaged community", 
                                "Not disadvantaged community")) %>% 
  
  ggplot() +
  geom_sf(data = st, fill = "gray95", color = "gray60", size = 0.1) +
  geom_sf(shape = 21, alpha = 0.4, aes(fill = disadvantaged)) +
  scale_fill_viridis_d(direction = -1) +
  
  labs(fill = "",
       title = "a") +
  theme_minimal() +
  
  facet_wrap(~Type) +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2700000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "none",
        legend.box = "vertical") +
  guides(fill = guide_legend(order = 1),
         color = guide_legend(order = 2), 
         size = guide_legend(order = 3))


# substation 
map_s <- pm_s %>% 
  
  left_join(dis %>% 
              st_drop_geometry(), by = "GEOID") %>% 
  
  filter(!is.na(disadvantaged)) %>% 
  
  mutate(disadvantaged = ifelse(disadvantaged == "TRUE", "Disadvantaged community", 
                                "Not disadvantaged community")) %>% 
  
  ggplot() +
  geom_sf(data = st, fill = "gray95", color = "gray60", size = 0.1) +
  geom_sf(shape = 21, alpha = 0.4, aes(fill = disadvantaged)) +
  
  scale_fill_viridis_d(direction = -1) +
  
  labs(fill = "",
       title = "b") +
  theme_minimal() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2700000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "none",
        legend.box = "vertical") +
  guides(fill = guide_legend(order = 1),
         color = guide_legend(order = 2))


si_s <- pm_s %>% 
  
  left_join(dis %>% 
              st_drop_geometry(), by = "GEOID") %>% 
  
  filter(!is.na(disadvantaged)) %>% 
  
  mutate(disadvantaged = ifelse(disadvantaged == "TRUE", "Disadvantaged community", 
                                "Not disadvantaged community")) %>% 

  ggplot() +
  geom_sf(data = st, fill = "gray95", color = "gray60", size = 0.1) +
  geom_sf(shape = 21, alpha = 0.8, aes(fill = Group), color = "NA") +
  
  scale_fill_viridis_d(direction = -1) +
  
  labs(fill = "",
       title = "Substations") +
  theme_minimal() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2700000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "bottom",
        legend.box = "vertical") +
  guides(fill = guide_legend(order = 1),
         color = guide_legend(order = 2))

ggsave("./fig/summary/si_s.png", si_s, width = 10, height = 6, dpi = 300)


# transmission lines
tl <- st_read("./data/TL/TL.shp")%>% 
  rename(Group = VOLT_CLASS) %>%
  

  mutate(Group = ifelse(Group == "NOT AVAILABLE", "Not Available",
                     ifelse(Group == "UNDER 100", "Under100", 
                            ifelse(Group == "100-161", "100-161", 
                                   ifelse(Group == "220-287", "220-287", "Above345 & DC")))),
         Group = factor(Group, levels = c("Not Available","Under100","100-161","220-287","Above345 & DC"))) %>% 
  st_transform(4269)


dis <- tr.sf %>% 
  rename(GEOID = FIPS) %>% 
  dplyr::select(GEOID) %>% 
  left_join(DAC %>% 
              dplyr::select(GEOID, disadvantaged), by = "GEOID") 

cropped <- tl %>% 
  st_intersection(dis %>% 
                    filter(disadvantaged == "TRUE"))%>% 
  dplyr::select(-GEOID, -disadvantaged)


map_t <- tl %>% 
  mutate(dis = FALSE) %>% 
  rbind(cropped %>% 
          mutate(dis = TRUE)) %>% 
  mutate(dis = ifelse(dis == T, "Disadvantaged community", 
                                "Not disadvantaged community")) %>% 
  
  ggplot() +
  geom_sf(data = st, fill = "white", color = "gray0") + # US border
  geom_sf(aes(color = dis), size = 0.05) +
  
  scale_color_viridis_d(direction = -1) +
  
  theme_minimal() +
  labs(title = "c", color = "") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "none",
        legend.key.size = unit(0.4, 'cm'),
        legend.text=element_text(size=9),
        plot.margin = unit(c(0,1,0,0), "cm"))


si_t <- tl %>% 
  mutate(dis = FALSE) %>% 
  rbind(cropped %>% 
          mutate(dis = TRUE)) %>% 
  
  
  ggplot() +
  geom_sf(data = st, fill = "white", color = "gray0") + # US border
  geom_sf(aes(fill = Group, color = Group), size = 0.05, ) +
  
  scale_fill_viridis_d(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  
  theme_minimal() +
  labs(title = "Transmission lines", fill = "", color = "") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.4, 'cm'),
        legend.text=element_text(size=9),
        plot.margin = unit(c(0,1,0,0), "cm"))

ggsave("./fig/summary/si_t.png", si_t, width = 10, height = 6, dpi = 300)


f <- ggarrange(map, ggarrange(map_s, map_t, nrow = 1),
               labels = NULL,
               ncol = 1, nrow = 2,
               heights = c(1,0.7),
               common.legend = TRUE, legend = "bottom",
               align = "hv")

ggsave("./fig/summary/plant.png", f, width = 10, height = 6, dpi = 300)


### CEJST
# clipping TL with DAC
pm_t <- tl %>% 
  st_intersection(dis) 

pm_t$length <- as.numeric(st_length(pm_t)) # length of TL


var <- DAC %>% 
  dplyr::select(GEOID, disadvantaged:`poverty level`)

nm <- names(var)[-1]

group <- c("Coal","Petroleum",
           "NG",
           "Others","Nuclear","Bio","Hydro",
           "Storage","Batteries",
           "Solar","Wind",
           "Substations", "Tr. lines")

data <- data.frame()
for(i in 1:length(group)){
  
  if(i %in% 1:11){
    
    dat <- var %>%
      
      left_join(pm %>%
                  
                  mutate(Group = ifelse(Group %in% c("NGCC","NGCT","NGIC","NGST"), 
                                        "NG", Group)) %>% 
                  
                  st_drop_geometry() %>%
                  filter(Group == group[i]) %>%
                  group_by(GEOID) %>%
                  summarise(N = sum(`Total Nameplate Capacity MW`)), by = "GEOID") %>%
      mutate_at(c("N"), ~replace_na(.,0))  # replace NA with 0
  }else if(i == 12){
    
    dat <- var %>%
      left_join(pm_s %>%
                  st_drop_geometry() %>%
                  group_by(GEOID) %>%
                  summarise(N = n()), by = "GEOID") %>%
      mutate_at(c("N"), ~replace_na(.,0))  # replace NA with 0
    
  }else{
    
    dat <- var %>% 
      left_join(pm_t %>%
                  st_drop_geometry() %>%
                  group_by(GEOID) %>%
                  summarise(N = sum(length)), by = "GEOID") %>%
      mutate_at(c("N"), ~replace_na(.,0))  # replace NA with 0
    
  }
  
  
  
  da <- data.frame()
  for(j in 2:(length(colnames(dat))-1)){
    d <- dat %>%
      group_by(get(colnames(dat)[j])) %>%
      summarise(value = sum(N)) %>%
      mutate(sum = sum(value),
             perc = value/sum*100)
    
    d<- d[2,]
    
    da <- rbind(d,da)
    
  }
  
  da <- da %>%
    cbind(data.frame(var = rev(colnames(dat))[c(-1,-32)])) %>%
    mutate(type = group[i])
  
  data <- rbind(da, data)
  
}


lab <- c("Workforce","Health", "Waster\nwater", "Pollution",
         "Housing", "Transpor\ntation", "Energy", "Climate")


jst <- data %>% 
  mutate(var = factor(var, levels = nm),
         
         class = ifelse(var == nm[1], "total",
                        ifelse(var %in% nm[27:30], "education", "income")),
         cat = ifelse(var == nm[1], "Community",
                      ifelse(var %in% nm[2:6], lab[8], 
                             ifelse(var %in% nm[7:8], lab[7],
                                    ifelse(var %in% nm[9:11], lab[6],
                                           ifelse(var %in% nm[12:15], lab[5],
                                                  ifelse(var %in% nm[16:20], lab[4],
                                                         ifelse(var %in% nm[21:22], lab[3],
                                                                ifelse(var %in% nm[23:26], lab[2], lab[1])))))))),
         color = cut(perc, breaks = c(0,10,20,27), include.lowest = T),
         tot = ifelse(class == "total", 56, 27)) 


f1 <- jst %>% 
  mutate(type = factor(type, levels = c("Coal","Petroleum",
                                        "NG",
                                        "Others","Nuclear","Bio","Hydro",
                                        "Storage","Batteries",
                                        "Solar","Wind",
                                        "Substations","Tr. lines"))) %>% 
  
  filter(class == "total") %>% 
  mutate(var = "     disadvantaged",
         cat = "Combined\ncommunity") %>% 
  ggplot() +
  geom_col(aes(x = tot, y = var), fill = "gray", position = "dodge", width = 0.5) +
  geom_col(aes(x = perc, y = var),  fill = "gray20", position = "dodge", width = 0.5) +
  
  labs(x = "", y = "\n\n\n\n\n\n\n", fill = "") + 
  facet_grid(cat+class ~type, scales = "free", space = "free", switch = "y") +
  geom_text(aes(x = perc-10, y = 1, label = paste0(round(perc, 0),"%")), 
            size = 2, color = "white") +
  
  theme_classic() +
  theme(legend.position = "none",
        strip.text.y = element_text(size = 6))

# cols <- hue_pal()(3)
f2 <- jst %>% 
  mutate(type = factor(type, levels = c("Coal","Petroleum",
                                        "NG",
                                        "Others","Nuclear","Bio","Hydro",
                                        "Storage","Batteries",
                                        "Solar","Wind",
                                        "Substations","Tr. lines"))) %>% 
  filter(class != "total") %>% 
  ggplot() +
  geom_col(aes(x = tot, y = var), fill = "gray", position = "dodge", width = 0.9) +
  geom_col(aes(x = perc, y = var, fill = color), position = "dodge", width = 0.9) +
  
  # geom_text(data = tp, aes(x = 0, y = n + 0.5, label = cat), hjust = 1) +
  
  labs(x = "Percentage of electric power capacity, substations, or transmission line length (%)", y = "", fill = "") + 
  scale_fill_viridis_d(begin = 0.8, end = 1) +
  facet_grid(cat+class ~type, scales = "free", space = "free", switch = "y") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.text.y = element_text(size = 6))

library(ggpubr)
f12 <- ggarrange(f1,f2, nrow = 2, heights = c(0.2,1))

ggsave("./fig/summary/jst_com.png", f12, width = 13, height = 8, dpi=300)

### distance analysis

var_mean <- demo %>% 
  gather(key, value, POPDEN:OVER64) %>% 
  group_by(key) %>% 
  summarise(value = weighted.mean(value, population, rm.na = T))

avg <- rbind(data.frame("Group" = rep("Average", 6*8),
                 "BUFF_DIST" = rep(c("0.25","0.5","1","2","3","5"), 8),
                 "variable" = rep(names(pw)[12:19], each = 6),
                 "value" = rep(var_mean$value[c(6,5,3,8,2,1,7,4)], each = 6),
                 "sd" = rep(NA, 6*8))) %>% 
  
  mutate(value = ifelse(variable == "POPDEN", value/ 100,
                        value*100)) %>% 
  
  mutate(variable = ifelse(variable == "POPDEN", "POPDEN\n(10^2 in SQML)",
                           paste0(variable, " (%)")))
  

# combine NG generators
plot1 <- pw %>% 
  mutate(BUFF_DIST = as.character(BUFF_DIST)) %>% 
  mutate(Group = ifelse(Group %in% c("NGCC","NGCT","NGIC","NGST"), "NG", Group)) %>% 
  
  gather(key = "variable", value = "value", POPDEN:OVER64) %>% 
  group_by(Group,BUFF_DIST, variable) %>% 
  summarise(value = mean(value),
            sd = sd(value)) %>% 
  
  mutate(Group = factor(Group, levels = c("Coal","Petroleum",
                                          "NG",
                                          "Others","Nuclear","Bio","Hydro",
                                          "Storage","Batteries",
                                          "Solar","Wind"))) %>% 
  
  mutate(value = ifelse(variable == "POPDEN", value/ 100,
                        value*100)) %>% 
  
  mutate(variable = ifelse(variable == "POPDEN", "POPDEN\n(10^2 in SQML)",
                           paste0(variable, " (%)"))) %>% 
  
  ggplot(aes(x = BUFF_DIST, y = value, group = Group, color = Group)) +
  geom_line() +
  geom_line(data = avg, aes(x = BUFF_DIST, y = value), color = "red", linetype = "dotted") +
  
  scale_color_viridis_d() +
  facet_wrap(~variable, ncol = 4, scales = "free") +
  labs(x = "Distance (miles)", y = "", color = "", title = "Intersection area weighted (average by count)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") # Center the title

ggsave("./fig/summary/donut_SI.png", plot1, width = 9, height = 7, dpi = 300)


# combine NG
plot_g <- pw %>% 
  
  mutate(BUFF_DIST = as.character(BUFF_DIST)) %>% 
  mutate(Group = ifelse(Group %in% c("NGCC","NGCT","NGIC","NGST"), "NG", Group)) %>% 
  
  gather(key = "variable", value = "value", POPDEN:OVER64) %>% 
  group_by(Group,BUFF_DIST, variable) %>% 
  summarise(value = weighted.mean(value, Total_Nameplate_Capacity_MW, rm.na = T),
            sd = sd(value)) %>% 
  mutate(Group = factor(Group, levels = c("Coal","Petroleum",
                                          "NG",
                                          "Others","Nuclear","Bio","Hydro",
                                          "Storage","Batteries",
                                          "Solar","Wind"))) %>% 
  
  mutate(value = ifelse(variable == "POPDEN", value/ 100,
                        value*100)) %>% 
  
  mutate(variable = ifelse(variable == "POPDEN", "POPDEN\n(10^2 in SQML)",
                           paste0(variable, " (%)"))) %>% 
  
  ggplot(aes(x = BUFF_DIST, y = value, group = Group, color = Group)) +
  geom_line() +
  geom_line(data = avg, aes(x = BUFF_DIST, y = value), color = "red", linetype = "dotted") +
  
  scale_color_viridis_d() +
  facet_wrap(~variable, ncol = 4, scales = "free") +
  labs(x = "Distance (miles)", y = "", color = "", title = "a") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0),
        legend.position = "bottom",
        legend.text =element_text(size=11)) + # Center the title
  guides(color=guide_legend(nrow=2,byrow=TRUE))

# substation 
plot_s <- pw_s %>% 
  mutate(BUFF_DIST = as.character(BUFF_DIST)) %>% 
  gather(key = "variable", value = "value", POPDEN:OVER64) %>% 
  group_by(Type,BUFF_DIST, variable) %>% 
  summarise(value = mean(value),
            sd = sd(value)) %>% 
  ungroup() %>% 
  
  mutate(value = ifelse(variable == "POPDEN", value/ 100,
                        value*100)) %>% 
  
  mutate(variable = ifelse(variable == "POPDEN", "POPDEN\n(10^2 in SQML)",
                           paste0(variable, " (%)"))) %>% 
  
  
  ggplot(aes(x = BUFF_DIST, y = value, group = Group, color = Group)) +
  geom_line() +
  geom_line(data = avg, aes(x = BUFF_DIST, y = value), color = "red", linetype = "dotted") +
  
  scale_color_viridis_d(direction = -1) +
  facet_wrap(~variable, ncol = 4, scales = "free") +
  labs(x = "Distance (miles)", y = "", color = "kV", 
       title = "b") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0),
        legend.position = "bottom",
        legend.text =element_text(size=12)) + # Center the title
  guides(color=guide_legend(nrow=2,byrow=TRUE))

# transmission lines
group = c("Not Available", "Under100", "100-161", "220-287", "Above345 & DC")
plot_tl <- tl_d %>% 
  mutate(buff = ifelse(buff > 8000, 5,
                       ifelse(buff > 4000, 3,
                              ifelse(buff > 3000, 2,
                                     ifelse(buff > 1000, 1,
                                            ifelse(buff > 800, 0.5, 0.25)))))) %>% 
  group_by(Group, buff) %>% 
  summarise(Area = sum(area, na.rm = TRUE),
            across(all_of(relevant_variables_demographics),
                   list(w = ~weighted.mean(., area, na.rm = TRUE)),
                   .names = "{col}")) %>% 
  
  ungroup() %>%  
  mutate(buff = as.character(buff)) %>% 
  gather(key = "variable", value = "value", POPDEN:LESSHS) %>% 
  
  mutate(value = ifelse(variable == "POPDEN", value/ 100,
                        value*100)) %>% 
  
  mutate(variable = ifelse(variable == "POPDEN", "POPDEN\n(10^2 in SQML)",
                           paste0(variable, " (%)"))) %>% 
  
  ggplot(aes(x = buff, y = value, group = Group, color = Group)) +
  geom_line() +
  geom_line(data = avg %>% 
              rename(buff = BUFF_DIST), aes(x = buff, y = value), color = "red", linetype = "dotted") +
  
  scale_color_viridis_d(direction = -1) +
  facet_wrap(~variable, ncol = 4, scales = "free") +
  labs(x = "Distance (miles)", y = "", color = "kV", 
       title = "c") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0),
        legend.position = "bottom",
        legend.text =element_text(size=12)) + # Center the title
  guides(color=guide_legend(nrow=2,byrow=TRUE))

plot <- ggarrange(plot_g, plot_s, plot_tl, nrow = 1)

ggsave("./fig/summary/donut.png", plot, width = 17, height = 5, dpi = 300)


# combine NG
# combine voltate
plot <- ah %>% 
  filter(population > 0) %>% 
  mutate(Group = ifelse(Group %in% c("NGCC","NGCT","NGIC","NGST"), "NG", Group)) %>% 
  # drop_na() %>% 
  # mutate(POPDEN = rescale(POPDEN)) %>%
  dplyr::select(Area, Group, Host, Buff, relevant_variables_demographics) %>% 
  mutate(Buff = as.character(Buff)) %>% 
  gather(key = "variable", value = "value", POPDEN:LESSHS) %>% 
  group_by(Area, Group,Host,Buff, variable) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  
  rbind(ah_s %>% 
          filter(population > 0) %>% 
          
          # drop_na() %>% 
          # mutate(POPDEN = rescale(POPDEN)) %>%
          dplyr::select(Area, Host, Buff, relevant_variables_demographics) %>% 
          mutate(Buff = as.character(Buff)) %>% 
          gather(key = "variable", value = "value", POPDEN:LESSHS) %>% 
          group_by(Area,Host,Buff, variable) %>% 
          summarise(mean = mean(value),
                    sd = sd(value)) %>% 
          mutate(Group = "Substations") %>% 
          dplyr::select(Area,Group,Host,Buff,variable,mean,sd)) %>% 
  
  rbind(ah_t %>% 
          filter(population > 0) %>% 
          dplyr::select(Area, Host, Buff, relevant_variables_demographics) %>% 
          mutate(Buff = as.character(Buff)) %>% 
          gather(key = "variable", value = "value", POPDEN:LESSHS) %>% 
          group_by(Area,Host,Buff, variable) %>% 
          summarise(mean = mean(value),
                    sd = sd(value)) %>% 
          mutate(Group = "Tr. lines")) %>% 
  

  mutate(Group = factor(Group, levels = c("Coal","Petroleum",
                                          "NG",
                                          "Others","Nuclear","Bio","Hydro",
                                          "Storage","Batteries",
                                          "Solar","Wind",
                                          "Substations", "Tr. lines"))) %>% 
  
  mutate(mean = ifelse(variable == "POPDEN", mean/ 100,
                       mean*100)) %>% 
  
  mutate(variable = ifelse(variable == "POPDEN", "POPDEN\n(10^2 in SQML)",
                           paste0(variable, " (%)"))) %>% 
  mutate(Host = ifelse(Host == "Y", "Host", "Non-host")) %>% 
  
  
  ggplot(aes(x = Buff, y = mean, color = Host, group = interaction(Host, Area))) +
  geom_line(aes(linetype = Area)) +
  geom_point(aes(color = Host)) +
  
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_manual(values = c("green1", "grey70")) +
  facet_grid(variable ~ Group, scales = "free") +
  labs(x = "Distance from infrastructure (miles)", y = "", 
       title = "",
       color = "", linetype = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.text =element_text(size=12)) # Center the title

ggsave("./fig/summary/host_com.png", plot, width = 11, height = 11)




### regression
### confusion matrix
library(caret)

# generators 
d <- data.frame()
area <- c("Tract","County")
buffers = rev(c("0", "1", "3"))

group <- c("Coal","Petroleum",
           "NG",
           "Others","Nuclear","Bio","Hydro",
           "Storage","Batteries",
           "Solar","Wind",
           "Substations", "Tr. lines")

data <- ah %>% 
  mutate(Group = ifelse(Group %in% c("NGCC","NGCT","NGIC","NGST"),
                        "NG", Group)) %>%
  rbind(ah_s %>% 
          mutate(Group = "Substations") %>% 
          dplyr::select(names(ah))) %>% 
  
  rbind(ah_t %>% 
          mutate(Group = "Tr. lines") %>% 
          dplyr::select(names(ah))) %>% 
  mutate(POPDEN = POPDEN/ 10000)

for(i in 1:length(group)){
  
  for(k in 1:2){
    for(j in 1:length(buffers)){
      
      dat <- data %>% 
        filter(population > 0) %>% 
        filter(Area == area[k]) %>%
        filter(Buff == buffers[j]) %>%
        filter(Group == group[i]) %>%
        dplyr::select(relevant_variables_demographics, Host) %>%
        mutate(
          # Host = ifelse(Host == "N", 0, 1),
          Host = factor(Host, levels = c("Y", "N")),
          POPDEN = log(POPDEN))
      
      
      #split dataset into training and testing set
      set.seed(1)
      
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
      
      
      # Set random seed for subsequent random selection and assignment operations
      set.seed(1981)
      
      # Specify logistic regression model to be estimated using training data
      # and k-fold cross-validation process
      cf <- train(Host ~ ., data=dat, 
                  method="glm", 
                  family=binomial, 
                  trControl=ctrlspecs)
      
      
      dat <- cbind(cf$resample['Balanced Accuracy'] %>% 
                     as.data.frame(),
                   cf$resample["Sensitivity"] %>% 
                     as.data.frame(),
                   cf$resample["Specificity"] %>% 
                     as.data.frame()) %>% 
        
        mutate(Group = group[i],
               Buffer = buffers[j],
               Area = area[k]) %>% 
        rename(Accuracy = 'Balanced Accuracy')
      
      d <- rbind(dat, d)
      

    }
  }
}


plt <- d %>% 
  mutate_if(is.numeric, ~ . * 100) %>% 
  mutate(Group = factor(Group, levels = group),
         Area = factor(Area, levels = c("Tract","County"))) %>% 
  gather(key, value, Accuracy:Specificity) %>% 
  group_by(Group, Buffer, Area, key) %>% 
  summarise(mean = mean(value),
            lower = quantile(value, prob = 0.025),
            upper = quantile(value, prob = 0.975)) %>% 
  mutate(Total = 1) %>% 
  
  ggplot(aes(x = mean, y = key, xmin=lower, xmax=upper)) +
  geom_col(aes(x = Total, y = key), fill = "gray", position = "dodge", width = 0.9) +
  geom_col(fill = "darkgreen", position = "dodge", width = 0.9) +
  geom_errorbar(width=0.4, colour="orange", alpha=0.9, size=0.7, position=position_dodge(.9)) +

  
  labs(x = "Performance (%)", y = "", fill = "", title = "Cross validation perfermance") + 

  scale_fill_viridis_d(begin = 0.2, end = 0.95) +
  facet_grid(Area+Buffer~Group, scales = "free", space = "free", switch = "y") +

  scale_x_continuous(breaks = c(0, 30, 60, 90)) +
  theme_classic() +

  theme(legend.position = "bottom",
        strip.text.y = element_text(size = 6))

ggsave("./fig/regression/accuracy.png", plt, width = 15, height = 5)


### regression 
regr <- function(data, group, area, buffer){
  
  dat <- data %>% 
    filter(population > 0) %>% 
    filter(Area == area) %>%
    filter(Buff == buffer) %>%
    filter(Group == group) %>%
    dplyr::select(relevant_variables_demographics, Host) %>%
    mutate(Host = ifelse(Host == "N", 0, 1),
           POPDEN = log(POPDEN))
  
  fit <- glm(Host ~ ., family = binomial(link="logit"), data = dat)
  
  summary(fit)$coefficients %>% 
    as.data.frame() %>% 
    mutate(color = ifelse(`Pr(>|z|)` < 0.05, "Y", "N")) %>%
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


# combine NG
area <- c("Tract","County")
buffers = c("0", "1", "3")
area <- c("Tract", "County")
TP <- data.frame()

group <- c("Coal","Petroleum",
           "NG",
           "Others","Nuclear","Bio","Hydro",
           "Storage","Batteries",
           "Solar","Wind",
           "Substations", "Tr. lines")

for(i in 1:length(group)){
  for(j in 1:length(buffers)){
    for(k in 1:2){
      
      tp <- regr(ah %>% 
                   mutate(Group = ifelse(Group %in% c("NGCC","NGCT","NGIC","NGST"), 
                                         "NG", Group)) %>% 
                   rbind(ah_s %>% 
                           mutate(Group = "Substations") %>% 
                           dplyr::select(names(ah))) %>% 
                   
                   rbind(ah_t %>% 
                           mutate(Group = "Tr. lines") %>% 
                           dplyr::select(names(ah))), group[i], area[k], buffers[j])
      
      TP <- rbind(tp, TP)
      
    }
  }
}


rg_t <- TP %>% 
  mutate(Group = factor(Group, levels = group),
         Area = factor(Area, levels = area),
         color = factor(color, levels = c("Y","N"))) %>% 
  filter(Area == "Tract") %>% 
  
  
  ggplot(aes(y = exp(pe), x = reorder(var, exp(pe)))) +
  geom_hline(yintercept = 1,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_errorbar(aes(ymin=exp(pe-1.96*se), ymax=exp(pe+1.96*se)),color="gray50") +
  geom_point(aes(fill = color),size = 2, color = "black",pch=21) +
  
  # geom_text(data = tp, aes(x = 0, y = n + 0.5, label = cat), hjust = 1) +
  coord_flip() +
  theme_bw() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "", y ="Odds ratio", 
       title = "",) +

  facet_grid(Area+Buffer ~Group, scales = "free") +
  scale_fill_manual(values=c("orange", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "none",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 

ggsave("./fig/regression/reg_t.png", rg_t, width = 14, height = 4)


rg_c <- TP %>% 
  mutate(Group = factor(Group, levels = group),
         Area = factor(Area, levels = area),
         color = factor(color, levels = c("Y","N"))) %>% 
  filter(Area == "County") %>% 
  
  ggplot(aes(y = exp(pe), x = reorder(var, exp(pe)))) +
  geom_hline(yintercept = 1,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_errorbar(aes(ymin=exp(pe-1.96*se), ymax=exp(pe+1.96*se)),color="gray50") +
  geom_point(aes(fill = color),size = 2, color = "black",pch=21) +
  
  # geom_text(data = tp, aes(x = 0, y = n + 0.5, label = cat), hjust = 1) +
  coord_flip() +
  theme_bw() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "", y ="Odds ratio", 
       title = "",) +
  
  facet_grid(Area+Buffer ~Group, scales = "free") +
  scale_fill_manual(values=c("orange", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "none",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 


ggsave("./fig/regression/reg_c.png", rg_c, width = 14, height = 4)
