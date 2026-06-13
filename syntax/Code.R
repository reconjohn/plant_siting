source("./syntax/Data.R")
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

### f1
dac_portion <- dis1 %>% 
  st_drop_geometry() %>%
  group_by(disadvantaged) %>%
  summarise(count = sum(estimate)) %>%
  mutate(sum = sum(count)) %>%
  mutate(rate = count/sum*100) %>% 
  pull(rate)

gen_dac <- pm %>% 
  dplyr::select(GEOID,Group) %>% 
  # mutate(Group = ifelse(Group == "Others", "Other plants", Group)) %>% 
  left_join(dis %>% 
              st_drop_geometry(), by = "GEOID") %>% 
  filter(!is.na(disadvantaged)) %>% 
  mutate(disadvantaged = ifelse(disadvantaged == "TRUE", "Disadvantaged community", 
                                "Not a disadvantaged community")) %>% 
  dplyr::select(-GEOID) %>% 
  st_buffer(0.1) %>% 
  
  mutate(Group = factor(Group, levels = group_g)) 

gen_dac1 <- pm %>% 
  st_drop_geometry() %>% 
  dplyr::rename(cap = "Total Nameplate Capacity MW") %>% 
  dplyr::select(GEOID,Group, cap) %>% 
  # mutate(Group = ifelse(Group == "Others", "Other plants", Group)) %>% 
  left_join(dis %>% 
              st_drop_geometry(), by = "GEOID") %>% 
  filter(!is.na(disadvantaged)) %>% 
  mutate(disadvantaged = ifelse(disadvantaged == "TRUE", "Disadvantaged community", 
                                "Not a disadvantaged community")) %>% 
  dplyr::select(-GEOID) %>% 
  
  mutate(Group = factor(Group, levels = group_g)) 

# substation
sub_dac <- pm_s %>%
  
  left_join(dis %>%
              st_drop_geometry(), by = "GEOID") %>%
  
  filter(!is.na(disadvantaged)) %>%
  mutate(disadvantaged = ifelse(disadvantaged == "TRUE", "Disadvantaged community", 
                                "Not a disadvantaged community")) %>% 
  dplyr::select(Group,disadvantaged)


map <- gen_dac %>% 
  mutate(Type = ifelse(Group %in% c("Coal","Oil","Natural Gas"), "Fossil fuel generators",
                       "Zero carbon generators and storage"),
         Type = factor(Type, levels = c("Fossil fuel generators", "Zero carbon generators and storage"))) %>%
  rbind(
    sub_dac %>%
      mutate(Type = "Substations") %>% 
      st_buffer(0.05),
    tl_tot %>% 
      mutate(Type = "Transmission lines")
  ) %>% 
  
  ggplot() +
  geom_sf(data = st, fill = "white", color = "gray0") + # US border
  geom_sf(aes(color = disadvantaged, fill = disadvantaged), size = 0.007, linewidth = 0.1) +
  
  scale_fill_manual(breaks=c("Disadvantaged community","Not a disadvantaged community"),
                    values=c("orange","cornflowerblue")) +
  scale_color_manual(breaks=c("Disadvantaged community","Not a disadvantaged community"),
                     values=c("orange","cornflowerblue")) +
  
  facet_wrap(~Type, nrow = 2) +
  theme_minimal() +
  labs(title = "", color = "", fill = "") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "none",
        legend.key.size = unit(0.4, 'cm'),
        legend.text=element_text(size=9),
        plot.margin = unit(c(0,1,0,0), "cm"))



d_count_da <- gen_dac %>%
  st_drop_geometry() %>%
  group_by(Group, disadvantaged) %>%
  summarise(sum = n()) %>%
  group_by(Group) %>%
  mutate(total = sum(sum)) %>%
  mutate(rate = sum/total*100) %>% 
  mutate(class = "Count") %>% 
  rbind(gen_dac1 %>%
          group_by(Group, disadvantaged) %>%
          summarise(sum = sum(cap)) %>%
          group_by(Group) %>%
          mutate(total = sum(sum)) %>%
          mutate(rate = sum/total*100) %>% 
          mutate(class = "Capacity")) %>% 
  mutate(disadvantaged = factor(disadvantaged, levels = c("Not a disadvantaged community","Disadvantaged community"))) 

gen_order <- d_count_da %>% 
  filter(class == "Capacity" & disadvantaged == "Disadvantaged community") %>% 
  arrange(rate) %>% 
  pull(Group)

d_count <- d_count_da %>% 
  mutate(Group = factor(Group, levels = gen_order)) %>% 
  filter(Group != "Others") %>% 
  
  ggplot() +
  geom_col(aes(y = Group, x = rate, fill = disadvantaged), alpha = 0.9) +
  geom_vline(xintercept = dac_portion[2], linetype = "dashed", color = "gray22")+
  labs(x = "DAC percentage (%)", y = "", fill = "", title = "b") +
  facet_wrap( ~ class) +
  # scale_fill_manual(values = c("#CC9900","#CCCC00")) +
  scale_fill_manual(breaks=c("Disadvantaged community","Not a disadvantaged community"),
                    values=c("orange","cornflowerblue")) +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="white",color="white"),
        strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=8),
        legend.position = "none",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=7),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.5)) 

t_count <- rbind(
  sub_dac %>%
    st_drop_geometry() %>%
    mutate(Group = as.character(Group),
           Group = ifelse(Group == "Above 345 kV", " Above 345 kV", Group)) %>% 
    group_by(Group, disadvantaged) %>%
    summarise(sum = n()) %>%
    group_by(Group) %>%
    mutate(total = sum(sum)) %>%
    mutate(rate = sum/total*100) %>% 
    mutate(class = "Substations"),
  
  tl_tot %>% 
    mutate(length = as.numeric(st_length(.))) %>% 
    st_drop_geometry() %>%
    group_by(Group, disadvantaged) %>%
    summarise(sum = sum(length)) %>%
    group_by(Group) %>%
    mutate(total = sum(sum)) %>%
    mutate(rate = sum/total*100) %>% 
    mutate(class = "Transmission lines") %>% 
    mutate(Group = as.character(Group),
           Group = ifelse(Group == "Above 345 kV & DC", " Above 345 kV", Group))
) %>% 
  filter(Group != "Not Available") %>% 
  mutate(disadvantaged = factor(disadvantaged, levels = c("Not a disadvantaged community","Disadvantaged community"))) %>% 
  mutate(Group = factor(Group, levels = rev(c("Not Available","Under 100 kV","100-161 kV","220-287 kV"," Above 345 kV")))) %>% 
  ggplot() +
  geom_col(aes(y = Group, x = rate, fill = disadvantaged), alpha = 0.9) +
  geom_vline(xintercept = dac_portion[2], linetype = "dashed", color = "gray22")+
  labs(x = "DAC percentage (%)", y = "", fill = "") +
  facet_wrap( ~ class) +
  # scale_fill_manual(values = c("#CC9900","#CCCC00")) +
  scale_fill_manual(breaks=c("Disadvantaged community","Not a disadvantaged community"),
                    values=c("orange","cornflowerblue")) +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="white",color="white"),
        strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=8),
        legend.position = "none",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=7),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 


TP_g <- data.frame()
for(i in 1:length(group_g)){
  
  tp <- regr1(pw, group_g[i])
  TP_g <- rbind(tp, TP_g)
  
}


dac <- TP_g %>% 
  mutate(Group = factor(Group, levels = group),
         Sig = ifelse(Sig == "Y","Significant","Not significant"),
         Sig = factor(Sig, levels = c("Significant","Not significant")),
         var = factor(var, levels = c(rev(relevant_variables_demographics), "Year", "DAC"))) %>% 
  
  filter(var %in% c("POPDEN","Year", "DAC")) %>% 
  filter(Group != "Others") %>% 
  
  ggplot(aes(x = pe, y = var, fill = Sig)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_errorbar(aes(xmin=pe-1.96*se, xmax=pe+1.96*se), width = 0.3, position = position_dodge(width = 0.9),
                show.legend = FALSE) +
  geom_point(size = 2,pch=21, position = position_dodge(width = 0.9)) +
  theme_minimal() +

  labs(x = "Capacity (MW)", y ="", 
       title = "",fill = "", color = "") +

  facet_grid( ~Group, scales = "free") +
  scale_fill_manual(values=c("gold2", "gray80")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        # strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=8,face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8,angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=11),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))

ggsave("./fig/plant.png", 
       ggarrange(
         ggarrange(map, 
                   ggarrange(d_count, t_count, nrow = 2, heights = c(1,0.5)), nrow = 1,widths = c(1,0.4),
                   common.legend = T, legend = "bottom"),
         dac,
         nrow = 2,
         heights = c(1,0.3), 
         labels = c("a", "c"),  # Adds labels to plots
         
         label.x = 0,        # Adjust horizontal position of labels
         label.y = 1,        # Adjust vertical position of labels
         font.label = list(size = 16, face = "bold"),
         common.legend = TRUE, legend = "bottom"), 
       width = 12, height = 9, dpi = 300)


### f2 CEJST
# clipping TL with DAC
pm_t <- tl %>% 
  st_intersection(dis) 

pm_t$length <- as.numeric(st_length(pm_t)) # length of TL

var <- DAC %>% 
  dplyr::select(GEOID, disadvantaged:`poverty level`)

nm <- names(var)[-1]

data <- data.frame()
for(i in 1:length(group)){
  
  if(i %in% 1:11){
    
    dat <- var %>%
      
      left_join(pm %>%
                  
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
         color = ifelse(color == "[0,10]", "<10%",
                        ifelse(color == "(10,20]", "≥10% to <20%", "≥20%")),
         color = factor(color, levels = c("<10%","≥10% to <20%", "≥20%")),
         tot = ifelse(class == "total", 56, 27)) 

f2 <- jst %>% 
  mutate(type = factor(type, levels = group)) %>% 
  
  filter(class != "total") %>% 
  filter(type != "Others") %>% 
  
  ggplot() +
  geom_col(aes(x = tot, y = var), fill = "gray80", position = "dodge", width = 0.9) +
  geom_col(aes(x = perc, y = var, fill = color), position = "dodge", width = 0.9) +
  labs(x = "Percentage of electric power capacity, substations, or transmission line length (%)", y = "", fill = "") + 
  scale_fill_manual(values = c("yellow","orange","red")) +
  facet_grid(cat ~type, scales = "free", space = "free", switch = "y") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major.x = element_blank(),
        # strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=8,face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=11),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 

ggsave("./fig/jst_com.png", f2, width = 13, height = 7, dpi=300)

### f3 distance analysis
var_mean <- demo %>% 
  gather(key, value, POPDEN:OVER64) %>% 
  group_by(key) %>% 
  summarise(value = weighted.mean(value, estimate, rm.na = T))

avg <- rbind(data.frame("Group" = rep("Average", 6*8),
                        "BUFF_DIST" = rep(c("0.25","0.5","1","2","3","5"), 8),
                        "var" = rep(names(pw)[12:19], each = 6),
                        "value" = rep(var_mean$value[c(6,5,3,8,2,1,7,4)], each = 6),
                        "sd" = rep(NA, 6*8))) %>% 
  
  mutate(value = ifelse(var == "POPDEN", value/ 100,
                        value*100)) %>% 
  
  mutate(var = ifelse(var == "LOWINCOME", "Low\nincome (%)",
                      ifelse(var == "LESSHS", "Less than\nhigh school\neducation (%)",
                             ifelse(var == "PEOPCOLOR", "People of\ncolor (%)",
                                    ifelse(var == "LINGISO", "Difficulty with\nEnglish (%)",
                                           ifelse(var == "UNEMPLOYED", "Unemployed (%)",
                                                  ifelse(var == "UNDER5", "Under 5\nyears old (%)",
                                                         ifelse(var == "OVER64", "Over 64\nyears old (%)",
                                                                "estimate\ndensity\n(100 ppsm)"))))))),
         var = factor(var, levels = vec)) %>% 
  filter(BUFF_DIST != "0.25")


# combine NG generators
# Define a custom labeling function
custom_formatter <- function(x) {
  sapply(x, function(val) {
    # First, check if the value is NA
    if (is.na(val)) {
      return(NA_character_) # Return a missing character value
    } else if (val == round(val)) {
      return(sprintf("%.0f", val))
    } else {
      return(sprintf("%.1f", val))
    }
  })
}

plot1 <- pw %>% 
  mutate(BUFF_DIST = as.character(BUFF_DIST)) %>% 
  mutate(Type = ifelse(Group %in% c("Coal","Natural Gas","Oil"), "Fossil fuel generators",
                       "Zero carbon generators/storages")) %>%
  
  gather(key = "var", value = "value", POPDEN:OVER64) %>% 
  group_by(Group,BUFF_DIST, var,Type) %>% 
  summarise(value = mean(value),
            sd = sd(value)) %>% 
  
  mutate(Group = factor(Group, levels = group_g)) %>% 
  
  mutate(value = ifelse(var == "POPDEN", value/ 100,
                        value*100)) %>%
  
  mutate(var = ifelse(var == "LOWINCOME", "Low\nincome (%)",
                      ifelse(var == "LESSHS", "Less than\nhigh school\neducation (%)",
                             ifelse(var == "PEOPCOLOR", "People of\ncolor (%)",
                                    ifelse(var == "LINGISO", "Difficulty with\nEnglish (%)",
                                           ifelse(var == "UNEMPLOYED", "Unemployed (%)",
                                                  ifelse(var == "UNDER5", "Under 5\nyears old (%)",
                                                         ifelse(var == "OVER64", "Over 64\nyears old (%)",
                                                                "estimate\ndensity\n(100 ppsm)"))))))),
         var = factor(var, levels = vec)) %>%
  
  filter(Group != "Others") %>% 
  filter(Type == "Fossil fuel generators") %>% 
  filter(BUFF_DIST != "0.25") %>% 
  
  ggplot(aes(x = BUFF_DIST, y = value, group = Group, color = Group)) +
  geom_line(linewidth = 0.8) +
  geom_line(data = avg, aes(x = BUFF_DIST, y = value), color = "black", linetype = "dotted") +
  
  scale_color_manual(values = c("gray50","red","black")) +
  scale_y_continuous(labels = custom_formatter) +
  
  facet_wrap(~var, scales = "free", nrow = 1) +
  labs(x = "Distance (miles)", y = "", color = "", title = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        # strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=11,face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=11),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))


plot2 <- pw %>% 
  mutate(BUFF_DIST = as.character(BUFF_DIST)) %>% 
  mutate(Type = ifelse(Group %in% c("Coal","Oil","Natural Gas"), "Fossil fuel generators",
                       "Zero carbon generators/storages")) %>%
  
  gather(key = "var", value = "value", POPDEN:OVER64) %>% 
  group_by(Group,BUFF_DIST, var,Type) %>% 
  summarise(value = mean(value),
            sd = sd(value)) %>% 
  
  mutate(Group = factor(Group, levels = group_g)) %>% 
  
  mutate(value = ifelse(var == "POPDEN", value/ 100,
                        value*100)) %>%
  
  mutate(var = ifelse(var == "LOWINCOME", "Low\nincome (%)",
                      ifelse(var == "LESSHS", "Less than\nhigh school\neducation (%)",
                             ifelse(var == "PEOPCOLOR", "People of\ncolor (%)",
                                    ifelse(var == "LINGISO", "Difficulty with\nEnglish (%)",
                                           ifelse(var == "UNEMPLOYED", "Unemployed (%)",
                                                  ifelse(var == "UNDER5", "Under 5\nyears old (%)",
                                                         ifelse(var == "OVER64", "Over 64\nyears old (%)",
                                                                "estimate\ndensity\n(100 ppsm)"))))))),
         var = factor(var, levels = vec)) %>%
  
  filter(Group != "Others") %>% 
  filter(Type == "Zero carbon generators/storages") %>% 
  filter(BUFF_DIST != "0.25") %>% 
  
  ggplot(aes(x = BUFF_DIST, y = value, group = Group, color = Group)) +
  geom_line(linewidth = 0.8) +
  geom_line(data = avg, aes(x = BUFF_DIST, y = value), color = "black", linetype = "dotted") +
  
  scale_color_manual(values = c("seagreen","blue4","purple4","gold1","lightblue2",
                                "orange2","darkorange1")) +
  scale_y_continuous(labels = custom_formatter) +
  
  facet_wrap(~var, scales = "free", nrow = 1) +
  labs(x = "Distance (miles)", y = "", color = "", title = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        # strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=11,face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=11),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))


# substation 
plot_s <- pw_s %>% 
  mutate(BUFF_DIST = as.character(BUFF_DIST)) %>% 
  gather(key = "var", value = "value", POPDEN:OVER64) %>% 
  group_by(Group,BUFF_DIST, var) %>% 
  summarise(value = mean(value),
            sd = sd(value)) %>% 
  ungroup() %>% 
  
  mutate(value = ifelse(var == "POPDEN", value/ 100,
                        value*100)) %>% 
  
  mutate(var = ifelse(var == "LOWINCOME", "Low\nincome (%)",
                      ifelse(var == "LESSHS", "Less than\nhigh school\neducation (%)",
                             ifelse(var == "PEOPCOLOR", "People of\ncolor (%)",
                                    ifelse(var == "LINGISO", "Difficulty with\nEnglish (%)",
                                           ifelse(var == "UNEMPLOYED", "Unemployed (%)",
                                                  ifelse(var == "UNDER5", "Under 5\nyears old (%)",
                                                         ifelse(var == "OVER64", "Over 64\nyears old (%)",
                                                                "estimate\ndensity\n(100 ppsm)"))))))),
         var = factor(var, levels = vec)) %>%
  filter(Group != "Not Available") %>% 
  filter(BUFF_DIST != "0.25") %>% 
  
  ggplot(aes(x = BUFF_DIST, y = value, group = Group, color = Group)) +
  geom_line(linewidth = 0.8) +
  geom_line(data = avg, aes(x = BUFF_DIST, y = value), color = "black", linetype = "dotted") +
  
  scale_color_viridis_d(option = "A", end = 0.8) +
  scale_y_continuous(labels = custom_formatter) +
  facet_wrap(~var, nrow = 1, scales = "free") +
  labs(x = "Distance (miles)", y = "", color = "", 
       title = "") +
  theme_bw() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        # strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=10,face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=11),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))

# transmission lines
plot_tl <- tl_d %>% 
  group_by(Group, buff) %>% 
  summarise(Area = sum(area, na.rm = TRUE),
            across(all_of(relevant_variables_demographics),
                   list(w = ~weighted.mean(., area, na.rm = TRUE)),
                   .names = "{col}")) %>% 
  
  ungroup() %>%  
  mutate(buff = as.character(buff)) %>% 
  gather(key = "var", value = "value", LOWINCOME:POPDEN) %>% 
  
  mutate(value = ifelse(var == "POPDEN", value/ 100,
                        value*100)) %>% 
  
  mutate(var = ifelse(var == "LOWINCOME", "Low\nincome (%)",
                      ifelse(var == "LESSHS", "Less than\nhigh school\neducation (%)",
                             ifelse(var == "PEOPCOLOR", "People of\ncolor (%)",
                                    ifelse(var == "LINGISO", "Difficulty with\nEnglish (%)",
                                           ifelse(var == "UNEMPLOYED", "Unemployed (%)",
                                                  ifelse(var == "UNDER5", "Under 5\nyears old (%)",
                                                         ifelse(var == "OVER64", "Over 64\nyears old (%)",
                                                                "estimate\ndensity\n(100 ppsm)"))))))),
         var = factor(var, levels = vec)) %>%
  filter(Group != "Not Available") %>% 
  filter(buff != "0.25") %>% 
  
  ggplot(aes(x = buff, y = value, group = Group, color = Group)) +
  geom_line(linewidth = 0.8) +
  geom_line(data = avg %>% 
              rename(buff = BUFF_DIST), aes(x = buff, y = value), color = "black", linetype = "dotted") +
  
  facet_wrap(~ var, nrow = 1, scales = "free") +
  labs(x = "Distance (miles)", y = "", color = "", 
       title = "") +
  scale_color_viridis_d(option = "A", end = 0.8) +
  scale_y_continuous(labels = custom_formatter) +
  theme_bw() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        # strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=10,face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=11),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))

plot <- ggarrange(plot1, plot2,
                  plot_s, plot_tl, nrow = 4,
                  labels = c("a", "b", "c", "d"),  # Adds labels to plots
                  label.x = 0,        # Adjust horizontal position of labels
                  label.y = 1,        # Adjust vertical position of labels
                  font.label = list(size = 16, face = "bold"))

ggsave("./fig/donut.png", plot, width = 12, height = 12, dpi = 300)

### f4
# combine NG
# combine voltate
plot <- ah %>% # estimate weighted mean
  filter(estimate > 0) %>% 
  # drop_na() %>% 
  # mutate(POPDEN = rescale(POPDEN)) %>%
  dplyr::select(estimate, Area, Group, Host, Buff, relevant_variables_demographics) %>% 
  mutate(Buff = as.character(Buff)) %>% 
  gather(key = "var", value = "value", LOWINCOME:POPDEN) %>% 
  group_by(Area, Group,Host,Buff, var) %>% 
  dplyr::summarise(mean = weighted.mean(value, estimate, rm.na = T)) %>% 
  
  rbind(ah_s %>% 
          filter(estimate > 0) %>% 
          
          # drop_na() %>% 
          # mutate(POPDEN = rescale(POPDEN)) %>%
          dplyr::select(estimate, Area, Host, Buff, relevant_variables_demographics) %>% 
          mutate(Buff = as.character(Buff)) %>% 
          gather(key = "var", value = "value", LOWINCOME:POPDEN) %>% 
          group_by(Area,Host,Buff, var) %>% 
          summarise(mean = weighted.mean(value, estimate, rm.na = T)) %>% 
          mutate(Group = "Substations") %>% 
          dplyr::select(Area,Group,Host,Buff,var,mean)) %>% 
  
  rbind(ah_t %>% 
          filter(estimate > 0) %>% 
          dplyr::select(estimate, Area, Host, Buff, relevant_variables_demographics) %>% 
          mutate(Buff = as.character(Buff)) %>% 
          gather(key = "var", value = "value", LOWINCOME:POPDEN) %>% 
          group_by(Area,Host,Buff, var) %>% 
          summarise(mean = weighted.mean(value, estimate, rm.na = T)) %>% 
          mutate(Group = "Transmission")) %>% 
  
  
  mutate(Group = factor(Group, levels = group)) %>% 
  
  mutate(mean = ifelse(var == "POPDEN", mean/ 100,
                       mean*100)) %>% 
  
  mutate(var = ifelse(var == "LOWINCOME", "Low\nincome (%)",
                      ifelse(var == "LESSHS", "Less than\nhigh school\neducation (%)",
                             ifelse(var == "PEOPCOLOR", "People of\ncolor (%)",
                                    ifelse(var == "LINGISO", "Difficulty with\nEnglish (%)",
                                           ifelse(var == "UNEMPLOYED", "Unemployed (%)",
                                                  ifelse(var == "UNDER5", "Under 5\nyears old (%)",
                                                         ifelse(var == "OVER64", "Over 64\nyears old (%)",
                                                                "estimate\ndensity\n(100 ppsm)"))))))),
         var = factor(var, levels = vec)) %>%
  mutate(Host = ifelse(Host == "Y", "Host community", "Non-host community"),
         Area = ifelse(Area == "Tract", "Census tract", Area),
         Area = factor(Area, levels = c("County","Census tract"))) %>% 
  filter(Group != "Others") %>% 
  
  ggplot(aes(x = Buff, y = mean, color = Host, group = interaction(Host, Area))) +
  geom_line(aes(linetype = Area), linewidth = 0.9) +
  geom_point(aes(color = Host)) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  scale_color_manual(values = c("gold2", "purple4")) +
  facet_grid(var ~ Group, scales = "free") +
  labs(x = "Distance from infrastructure (miles)", y = "", 
       title = "",
       color = "", linetype = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=9,face = "bold"),
        legend.position = "bottom",
        legend.text =element_text(size=12)) # Center the title

ggsave("./fig/host_com.png", plot, width = 12, height = 12)

### f5 regression 
# combine NG
area <- c("Tract","County")
buffers = c("0", "1", "3")
TP <- data.frame()

# data for regression input
df <- ah %>% 
  rbind(ah_s %>% 
          mutate(Group = "Substations") %>% 
          dplyr::select(names(ah))) %>% 
  
  rbind(ah_t %>% 
          mutate(Group = "Transmission") %>% 
          dplyr::select(names(ah)))

for(i in 1:length(group)){
  for(j in 1:length(buffers)){
    for(k in 1:2){
      
      tp <- regr(df, group[i], area[k], buffers[j])
      
      TP <- rbind(tp, TP)
      
    }
  }
}


rg_t <- TP %>%
  filter(Buffer != "1") %>% 
  filter(Group != "Others") %>% 
  mutate(inter = interaction(Area, Buffer),
         inter = factor(inter, levels = rev(c("Tract.0","Tract.3","County.0","County.3")))) %>% 
  mutate(Group = factor(Group, levels = group),
         Sig = ifelse(Sig == "Y", "Significant", "Not significant"),
         Sig = factor(Sig, levels = c("Significant","Not significant")),
         var = factor(var, levels = c(relevant_variables_demographics))) %>%
  mutate(var = ifelse(var == "LOWINCOME", "Low\nincome (%)",
                      ifelse(var == "LESSHS", "Less than\nhigh school\neducation (%)",
                             ifelse(var == "PEOPCOLOR", "People of\ncolor (%)",
                                    ifelse(var == "LINGISO", "Difficulty with\nEnglish (%)",
                                           ifelse(var == "UNEMPLOYED", "Unemployed (%)",
                                                  ifelse(var == "UNDER5", "Under 5\nyears old (%)",
                                                         ifelse(var == "OVER64", "Over 64\nyears old (%)",
                                                                "Population\ndensity\n(100 ppsm)"))))))),
         var = factor(var, levels = c("Population\ndensity\n(100 ppsm)", setdiff(vec, "Population\ndensity\n(100 ppsm)")))) %>%
  
  ggplot(aes(x = pe, y = inter, color = inter, fill = Sig)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_errorbar(aes(xmin=pe-1.96*se, xmax=pe+1.96*se), width = 0.3, position = position_dodge(width = 0.9),
                show.legend = FALSE) +
  geom_point(size = 2,pch=21, position = position_dodge(width = 0.9)) +
  theme_minimal() +
  
  facet_grid(var ~Group, scales = "free", switch = "y") +
  
  # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", function(x) sprintf("%.1f", x))) +
  labs(x = "Odds ratio (log scale)", y ="", 
       title = "",fill = "", color = "") +
  
  scale_color_manual(values = c("Tract.0" = "black", "Tract.3" = "gray20",
                                "County.0" = "gray40", "County.3" = "gray60"),
                     labels = rev(c("Census tract, 0-mile buffer","Census tract, 3-mile buffer",
                                    "County tract, 0-mile buffer","County tract, 3-mile buffer")),
                     name = "") +
  scale_fill_manual(values=c("gold2", "white")) +
  
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=9,face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8, angle = 40, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=16)) +
  guides(color = guide_legend(reverse = T))

ggsave("./fig/reg.png", 
       rg_t,
       width = 12, height = 10, dpi = 300)


### f0 method map
p_point <- pw %>%
  filter(Group == "Oil" & BUFF_DIST == 1) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4269,
           agr = "constant",
           stringsAsFactors = FALSE,
           remove = TRUE) %>% 
  st_buffer(dist = 0.05)


b1 <- tr.sf %>% 
  left_join(ah %>% 
              filter(Area == "Tract" & Group == "Oil"),
            by = c("FIPS" = "GEOID")) %>% 
  filter(!is.na(Area) & Host == "Y" & Buff == 0) %>% 
  
  ggplot() +
  geom_sf(data = st, fill = "gray35", color = "gray60", size = 0.1) +
  geom_sf(aes(fill = Host), color = NA) +
  geom_sf(data = p_point, aes(fill = "Facility"), color = NA) +  # Add legend for red point
  scale_fill_manual(values = c("red","yellow"), labels = c("Electricity infrastructure","Host community")) + # Yellow polygon
  labs(title = "\n\nHost communities (0-mile buffer)", x = "", y = "", fill = "", color = "") +
  
  theme_minimal() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2700000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "bottom",
        legend.text =element_text(size=10),
        plot.title = element_text(hjust = 0.5)) 


b2 <- tr.sf %>% 
  left_join(ah %>% 
              filter(Area == "Tract" & Group == "Oil"),
            by = c("FIPS" = "GEOID")) %>% 
  filter(!is.na(Area) & Host == "Y" & Buff == 1) %>% 
  
  ggplot() +
  geom_sf(data = st, fill = "gray35", color = "gray60", size = 0.1) +
  geom_sf(aes(fill = Host), color = NA) +
  geom_sf(data = p_point, aes(fill = "Energy Facility"), color = NA) +  # Add legend for red point
  scale_fill_manual(values = c("red","yellow"), labels = c("Electricity infrastructure","Host community")) + # Yellow polygon
  labs(title = "\n\nHost communities (1-mile buffer)", x = "", y = "", fill = "", color = "") +
  
  theme_minimal() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2700000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "bottom",
        legend.text =element_text(size=10),
        plot.title = element_text(hjust = 0.5)) 


b3 <- tr.sf %>% 
  left_join(ah %>% 
              filter(Area == "Tract" & Group == "Oil"),
            by = c("FIPS" = "GEOID")) %>% 
  filter(!is.na(Area) & Host == "Y" & Buff == 3) %>% 
  
  ggplot() +
  geom_sf(data = st, fill = "gray35", color = "gray60", size = 0.1) +
  geom_sf(aes(fill = Host), color = NA) +
  geom_sf(data = p_point, aes(fill = "Energy Facility"), color = NA) +  # Add legend for red point
  scale_fill_manual(values = c("red","yellow"), labels = c("Electricity infrastructure","Host community")) + # Yellow polygon
  labs(title = "\n\nHost communities (3-mile buffer)", x = "", y = "", fill = "", color = "") +
  
  theme_minimal() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2700000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "bottom",
        legend.text =element_text(size=10),
        plot.title = element_text(hjust = 0.5)) 



# Seattle Petroleum generator
seattle_bbox <- st_as_sfc(st_bbox(c(xmin=-122.435, xmax=-122.135, 
                                    ymin=47.511, ymax=47.735), 
                                  crs = st_crs(4326)))

# Get transformed coordinate boundaries
s_box <- st_bbox(seattle_bbox)

p_point <- pw %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4269,
           agr = "constant",
           stringsAsFactors = FALSE,
           remove = TRUE) %>%
  filter(Plant_Code == 54809)


# Transform to a projected CRS for distance calculations (EPSG:3857)
point_projected <- st_transform(p_point, crs = 3857)
buffer_1mile <- st_buffer(point_projected, dist = 2450)
buffer_3mile <- st_buffer(point_projected, dist = 7350)

# Transform back to WGS 84 (EPSG:4326)
buffer_1mile_wgs84 <- st_transform(buffer_1mile, crs = 4326)
buffer_3mile_wgs84 <- st_transform(buffer_3mile, crs = 4326)


h1 <- tr.sf %>% 
  filter(STATE_ABBR == "WA") %>% 
  left_join(ah %>% 
              filter(Area == "Tract" & Group == "Oil"),
            by = c("FIPS" = "GEOID")) %>% 
  filter(!is.na(Area)) %>% 
  mutate(interact = interaction(Host, Buff, sep="")) %>% 
  
  group_by(FIPS) %>% 
  mutate(comm = case_when(
    any(interact == "Y0") ~ "Host community (0-mile buffer)",
    any(interact == "Y1") ~ "Host community (1-mile buffer)",
    any(interact == "Y3") ~ "Host community (3-mile buffer)",
    TRUE ~ "Non-host community")) %>% 
  ggplot() +
  geom_sf(aes(fill = comm), color = "white") +
  geom_sf(data = buffer_3mile_wgs84, color = "black", aes(linetype = "3 mile"), fill = NA, linewidth = 0.5) +
  geom_sf(data = buffer_1mile_wgs84, color = "black", aes(linetype = "1 mile"), fill = NA, linewidth = 0.5) +
  geom_sf(data = p_point, aes(color = "Electricity infrastructure")) +
  annotate(geom="text", 
           x = -122.262,
           y = 47.647,
           label = "1 mile", 
           color = "black",
           size = 2) +
  annotate(geom="text", 
           x = -122.259,
           y = 47.611,
           label = "3 mile", 
           color = "black",
           size = 2) +
  
  labs(title = "\nElectricity generator in urban area", x = "", y = "", color = "", fill = "") +
  scale_color_manual(values = c("red")) +
  scale_fill_viridis_d(option = "D", direction =-1) +
  scale_linetype_manual(values = c("solid", "dotted"), guide = F) +
  # scale_color_manual(values = c("red", "blue"), labels = c("1 mile","3 mile")) +
  theme_minimal() +
  coord_sf(xlim = c(s_box[1], s_box[3]), 
           ylim = c(s_box[2],s_box[4]), expand = FALSE, datum = NA) +
  theme(legend.position = "bottom",
        legend.text =element_text(size=10)) +
  guides(fill = guide_legend(order = 2),
         color = guide_legend(order = 1))



# rural Petroleum generator
rural_bbox <- st_as_sfc(st_bbox(c(xmin=-114.4, xmax=-113.38, 
                                  ymin=44.78, ymax=45.58), 
                                crs = st_crs(4326)))

# Get transformed coordinate boundaries
s_box <- st_bbox(rural_bbox)

p_point <- pw %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4269,
           agr = "constant",
           stringsAsFactors = FALSE,
           remove = TRUE) %>%
  filter(Plant_Code == 817)


# Transform to a projected CRS for distance calculations (EPSG:3857)
point_projected <- st_transform(p_point, crs = 3857)
buffer_1mile <- st_buffer(point_projected, dist = 2450)
buffer_3mile <- st_buffer(point_projected, dist = 7350)

# Transform back to WGS 84 (EPSG:4326)
buffer_1mile_wgs84 <- st_transform(buffer_1mile, crs = 4326)
buffer_3mile_wgs84 <- st_transform(buffer_3mile, crs = 4326)


h2 <- tr.sf %>% 
  filter(STATE_ABBR == "ID") %>% 
  left_join(ah %>% 
              filter(Area == "Tract" & Group == "Oil"),
            by = c("FIPS" = "GEOID")) %>% 
  filter(!is.na(Area)) %>%
  mutate(interact = interaction(Host, Buff, sep="")) %>% 
  group_by(FIPS) %>% 
  mutate(comm = case_when(
    any(interact == "Y0") ~ "Host community (0-mile buffer)",
    any(interact == "Y1") ~ "Host community (1-mile buffer)",
    any(interact == "Y3") ~ "Host community (3-mile buffer)",
    TRUE ~ "Non-host community")) %>% 
  
  ggplot() +
  geom_sf(aes(fill = comm), color = "white") +
  geom_sf(data = buffer_3mile_wgs84, color = "black", aes(linetype = "3 mile"), fill = NA, linewidth = 0.5) +
  geom_sf(data = buffer_1mile_wgs84, color = "black", aes(linetype = "1 mile"), fill = NA, linewidth = 0.5) +
  geom_sf(data = p_point, aes(color = "Electricity infrastructure"), size = 0.01) +
  annotate(geom="text", 
           x = -113.88,
           y = 45.21,
           label = "1 mile", 
           color = "black",
           size = 2) +
  annotate(geom="text", 
           x = -113.717,
           y = 45.18,
           label = "3 mile", 
           color = "black",
           size = 2) +
  
  labs(title = "\nElectricity generator in rural area", x = "", y = "", color = "", fill = "") +
  scale_color_manual(values = c("red")) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  scale_linetype_manual(values = c("solid", "dotted"), guide = F) +
  theme_minimal() +
  coord_sf(xlim = c(s_box[1], s_box[3]), 
           ylim = c(s_box[2],s_box[4]), expand = FALSE, datum = NA) +
  theme(legend.position = "bottom",
        legend.text =element_text(size=10))


# CA tranmission lines
tr_bbox <- st_as_sfc(st_bbox(c(xmin=-121.035, xmax=-120.435, 
                               ymin=34.911, ymax=35.435), 
                             crs = st_crs(4326)))

# Get transformed coordinate boundaries
t_box <- st_bbox(tr_bbox)

p_line <- tl %>%
  filter(Group == "220-287 kV") %>% 
  st_transform(st_crs(t_box)) %>% 
  st_crop(tr_bbox)


# Transform to a projected CRS for distance calculations (EPSG:3857)
point_projected <- st_transform(p_line, crs = 3857)
buffer_1mile <- st_buffer(point_projected, dist = 1500) %>% 
  st_union(by_feature = FALSE) 
buffer_3mile <- st_buffer(point_projected, dist = 4900) %>% 
  st_union(by_feature = FALSE) 

# Transform back to WGS 84 (EPSG:4326)
buffer_1mile_wgs84 <- st_transform(buffer_1mile, crs = 4326)
buffer_3mile_wgs84 <- st_transform(buffer_3mile, crs = 4326)

h3 <- tr.sf %>% 
  filter(STATE_ABBR == "CA") %>% 
  left_join(ah_t %>% 
              filter(Area == "Tract" & Group == "220-287kV"),
            by = c("FIPS" = "GEOID")) %>% 
  filter(!is.na(Area)) %>% 
  mutate(interact = interaction(Host, Buff, sep="")) %>% 
  
  group_by(FIPS) %>% 
  mutate(comm = case_when(
    any(interact == "Y0") ~ "Host community (0-mile buffer)",
    any(interact == "Y1") ~ "Host community (1-mile buffer)",
    any(interact == "Y3") ~ "Host community (3-mile buffer)",
    TRUE ~ "Non-host community")) %>% 
  ggplot() +
  geom_sf(aes(fill = comm), color = "white") +
  geom_sf(data = buffer_3mile_wgs84, aes(linetype = "3 mile"), color = "black", fill = NA, linewidth = 0.5) +
  geom_sf(data = buffer_1mile_wgs84, aes(linetype = "1 mile"), color = "black", fill = NA, linewidth = 0.5) +
  geom_sf(data = p_line, aes(color = "Electricity infrastructure")) +
  annotate(geom="text",
           x = -120.862,
           y = 35.19,
           label = "1 mile",
           color = "black",
           size = 2) +
  annotate(geom="text",
           x = -120.689,
           y = 35.14,
           label = "3 mile",
           color = "black",
           size = 2) +
  
  labs(title = "\nElectricity transmission", x = "", y = "", color = "", fill = "") +
  scale_color_manual(values = c("red")) +
  scale_fill_viridis_d(option = "D", direction =-1) +
  scale_linetype_manual(values = c("solid", "dotted"), guide = F) +
  # scale_color_manual(values = c("red", "blue"), labels = c("1 mile","3 mile")) +
  theme_minimal() +
  coord_sf(xlim = c(t_box[1]+0.1, t_box[3]+0.1), 
           ylim = c(t_box[2],t_box[4]+0.03), expand = FALSE, datum = NA) +
  theme(legend.position = "bottom",
        legend.text =element_text(size=10))

ggsave("./fig/map.png", 
       ggarrange(
         ggarrange(h1, h2, h3, nrow = 1,
                   common.legend = T, legend = "bottom"),
         ggarrange(b1,b3, nrow = 1, 
                   common.legend = T, legend = "bottom"),
         nrow = 2,
         labels = c("a", "b"),  # Adds labels to plots
         heights = c(1,1),
         # vjust = 1,
         label.x = 0,        # Adjust horizontal position of labels
         label.y = 1,        # Adjust vertical position of labels
         font.label = list(size = 16, face = "bold")),
       width = 12, height = 10, dpi = 300)
