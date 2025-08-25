### SI
source("./syntax/Data.R")

plot <- list()
for(i in 1:3){
  plot[[i]] <- pm %>% 
    filter(Group == group_g[i]) %>% 
    left_join(dis %>% 
                st_drop_geometry(), by = "GEOID") %>% 
    filter(!is.na(disadvantaged)) %>% 
    
    mutate(disadvantaged = ifelse(disadvantaged == "TRUE", "Disadvantaged community", 
                                  "Not disadvantaged community")) %>% 
    
    ggplot() +
    geom_sf(data = st, fill = "gray95", color = "gray60", size = 0.1) +
    geom_sf(shape = 21, alpha = 0.7, aes(fill = disadvantaged, 
                                         size = `Total Nameplate Capacity MW`/1000)) +
    # scale_fill_manual(values = c(HH = "red",HL = "green",LH = "blue",LL = "white")) +
    scale_fill_viridis_d(direction = -1) +
    
    scale_color_manual(values = c("black","white")) + 
    scale_shape_manual(values = c(21,22)) +
    labs(fill = "", 
         color = "",
         size = "Capacity (GW)",
         title = group_g[i]) +
    theme_minimal() +
    coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2700000), 
             ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
    theme(legend.position = "none",
          legend.text = element_text(color = "black",family="Franklin Gothic Book",size=15),
          plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
    guides(fill = guide_legend(order = 1),
           color = guide_legend(order = 2), 
           size = guide_legend(order = 3))
  
}

s01 <- ggarrange(plot[[1]],plot[[2]],plot[[3]], nrow = 2, ncol = 2,
                 common.legend = T, legend = "bottom")
ggsave(paste0("./fig/", "s01", ".png"), s01, width = 12, height = 8)



plot <- list()
for(i in 4:10){
  plot[[i-3]] <- pm %>% 
    filter(Group == group_g[i]) %>% 
    left_join(dis %>% 
                st_drop_geometry(), by = "GEOID") %>% 
    filter(!is.na(disadvantaged)) %>% 
    
    mutate(disadvantaged = ifelse(disadvantaged == "TRUE", "Disadvantaged community", 
                                  "Not disadvantaged community")) %>% 
    
    ggplot() +
    geom_sf(data = st, fill = "gray95", color = "gray60", size = 0.1) +
    geom_sf(shape = 21, alpha = 0.7, aes(fill = disadvantaged, 
                                         size = `Total Nameplate Capacity MW`/1000)) +
    # scale_fill_manual(values = c(HH = "red",HL = "green",LH = "blue",LL = "white")) +
    scale_fill_viridis_d(direction = -1) +
    
    scale_color_manual(values = c("black","white")) + 
    scale_shape_manual(values = c(21,22)) +
    labs(fill = "", 
         color = "",
         size = "Capacity (GW)",
         title = group_g[i]) +
    theme_minimal() +
    coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2700000), 
             ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
    theme(legend.position = "none",
          legend.text = element_text(color = "black",family="Franklin Gothic Book",size=15),
          plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
    guides(fill = guide_legend(order = 1),
           color = guide_legend(order = 2), 
           size = guide_legend(order = 3))
  
}

s02 <- ggarrange(plot[[1]],plot[[2]],plot[[3]], 
                 plot[[4]],plot[[5]],plot[[6]],plot[[7]], nrow = 4, ncol = 2,
                 common.legend = T, legend = "bottom")
ggsave(paste0("./fig/", "s02", ".png"), s02, width = 12, height = 12)



si_s <- pm_s %>% 
  
  left_join(dis %>% 
              st_drop_geometry(), by = "GEOID") %>% 
  
  filter(!is.na(disadvantaged)) %>% 
  
  mutate(disadvantaged = ifelse(disadvantaged == "TRUE", "Disadvantaged community (DAC)", 
                                "Non DAC")) %>% 
  
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
        legend.box = "vertical",
        legend.text = element_text(color = "black",family="Franklin Gothic Book",size=15),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
  guides(fill = guide_legend(order = 1),
         color = guide_legend(order = 2))

ggsave("./fig/si_s.png", si_s, width = 12, height = 8, dpi = 300)



si_t <- tl_nondis %>% 
  mutate(dis = FALSE) %>% 
  rbind(tl_dis %>% 
          mutate(dis = TRUE)) %>% 
  
  ggplot() +
  geom_sf(data = st, fill = "white", color = "gray0") + # US border
  geom_sf(aes(fill = Group, color = Group), size = 0.05, ) +
  
  # geom_sf(aes(color = "FALSE"), size = 0.05) +
  # geom_sf(data = cropped, aes(color = "TRUE"), size = 0.05) +
  # scale_color_manual(values = c("TRUE" = "yellow", "FALSE" = "darkmagenta"),
  #                    breaks = c("TRUE", "FALSE")) +
  
  scale_fill_viridis_d(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  
  theme_minimal() +
  labs(title = "Transmission lines", fill = "", color = "") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(color = "black",family="Franklin Gothic Book",size=15),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 

ggsave("./fig/si_t.png", si_t, width = 12, height = 8, dpi = 300)


plot <- list()
for(i in 1:3){
  plot[[i]] <- pw %>% 
    mutate(BUFF_DIST = as.character(BUFF_DIST)) %>% 
    gather(key = "variable", value = "value", POPDEN:OVER64) %>% 
    filter(Group == group_g[i]) %>% 
    ggplot(aes(x = BUFF_DIST, y = value)) +
    geom_boxplot() +
    facet_wrap(~variable, nrow = 1, scales = "free") +
    labs(x = "Distance (miles)", y = "Value", title = group_g[i]) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0),
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8)) # Center the title
}

s1 <- ggarrange(plot[[1]],plot[[2]],plot[[3]], nrow = 3)
ggsave(paste0("./fig/", "s1", ".png"), s1, width = 12, height = 8)


plot <- list()
for(i in 4:10){
  plot[[i-3]] <- pw %>% 
    mutate(BUFF_DIST = as.character(BUFF_DIST)) %>% 
    gather(key = "variable", value = "value", POPDEN:OVER64) %>% 
    filter(Group == group_g[i]) %>% 
    ggplot(aes(x = BUFF_DIST, y = value)) +
    geom_boxplot() +
    facet_wrap(~variable, nrow = 1, scales = "free") +
    labs(x = "Distance (miles)", y = "Value", title = group_g[i]) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0),
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8)) # Center the title
}

s2 <- ggarrange(plot[[1]],plot[[2]],plot[[3]], 
                plot[[4]],plot[[5]],plot[[6]],plot[[7]],nrow = 7)
ggsave(paste0("./fig/", "s2", ".png"), s2, width = 12, height = 12)


plot <- list()
for(i in 1:5){
  plot[[i]] <- pw_s %>% 
    mutate(BUFF_DIST = as.character(BUFF_DIST)) %>% 
    gather(key = "variable", value = "value", POPDEN:OVER64) %>% 
    filter(Group == group_s[i]) %>% 
    ggplot(aes(x = BUFF_DIST, y = value)) +
    geom_boxplot() +
    facet_wrap(~variable, nrow = 1, scales = "free") +
    labs(x = "Distance (miles)", y = "Value", title = group_s[i]) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0),
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8)) # Center the title
}

s3 <- ggarrange(plot[[1]],plot[[2]],plot[[3]], plot[[4]],plot[[5]], nrow = 5)
ggsave(paste0("./fig/", "s3", ".png"), s3, width = 12, height = 10)

plot <- list()
for(i in 1:5){
  
  plot[[i]] <- tl_d %>% 
    mutate(buff = as.character(buff)) %>% 
    filter(Group == group_t[i]) %>% 
    
    gather(key = "variable", value = "value", POPDEN:OVER64) %>% 
    
    ggplot(aes(x = buff, y = value)) +
    geom_boxplot() +
    facet_wrap(~variable, nrow = 1, scales = "free") +
    labs(x = "Distance (miles)", y = "Value", title = group_t[i]) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0), # Center the title
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8)) # Center the title
}

s4 <- ggarrange(plot[[1]],plot[[2]],plot[[3]], plot[[4]],plot[[5]], nrow = 5)
ggsave(paste0("./fig/", "s4", ".png"), s4, width = 12, height = 10)


plot <- list()
for(i in 1:3){
  plot[[i]] <- ah %>% 
    filter(Area == "Tract") %>% 
    dplyr::select(- Area) %>% 
    drop_na() %>% 
    mutate(POPDEN = log(POPDEN)) %>% 
    
    filter(Group == group_g[i]) %>% 
    dplyr::select(Group, Host, Buff, relevant_variables_demographics) %>% 
    mutate(Buff = as.character(Buff)) %>% 
    gather(key = "variable", value = "value", POPDEN:LESSHS) %>% 
    
    ggplot(aes(x = Buff, y = value, color = Host)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~variable, nrow = 1, scales = "free") +
    labs(x = "Distance (miles)", y = "Value", title = group_g[i]) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0),
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8), # Center the title
          legend.position = "none") # Center the title
  
}

s5 <- ggarrange(plot[[1]],plot[[2]],plot[[3]], nrow = 3,
                common.legend = T, legend = "bottom")
ggsave(paste0("./fig/", "s5", ".png"), s5, width = 12, height = 8)



plot <- list()
for(i in 4:10){
  plot[[i-3]] <- ah %>% 
    filter(Area == "Tract") %>% 
    dplyr::select(- Area) %>% 
    drop_na() %>% 
    mutate(POPDEN = log(POPDEN)) %>% 
    
    filter(Group == group_g[i]) %>% 
    dplyr::select(Group, Host, Buff, relevant_variables_demographics) %>% 
    mutate(Buff = as.character(Buff)) %>% 
    gather(key = "variable", value = "value", POPDEN:LESSHS) %>% 
    
    ggplot(aes(x = Buff, y = value, color = Host)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~variable, nrow = 1, scales = "free") +
    labs(x = "Distance (miles)", y = "Value", title = group_g[i]) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0),
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8), # Center the title
          legend.position = "none") # Center the title
  
}

s6 <- ggarrange(plot[[1]],plot[[2]],plot[[3]], 
                plot[[4]],plot[[5]],plot[[6]],plot[[7]],nrow = 7,
                common.legend = T, legend = "bottom")
ggsave(paste0("./fig/", "s6", ".png"), s6, width = 12, height = 12)


plot <- list()
for(i in 1:5){
  plot[[i]] <- ah_s %>% 
    filter(Area == "Tract") %>% 
    dplyr::select(- Area) %>% 
    mutate(POPDEN = log(POPDEN)) %>% 
    
    filter(Group == group_s[i]) %>% 
    dplyr::select(Group, Host, Buff, relevant_variables_demographics) %>% 
    mutate(Buff = as.character(Buff)) %>% 
    gather(key = "variable", value = "value", LOWINCOME:POPDEN) %>% 
    
    ggplot(aes(x = Buff, y = value, color = Host)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~variable, nrow = 1, scales = "free") +
    labs(x = "Distance (miles)", y = "Value", title = group_s[i]) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0),
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8), # Center the title
          legend.position = "none") # Center the title
}

s7 <- ggarrange(plot[[1]],plot[[2]],plot[[3]], 
                plot[[4]],plot[[5]],nrow = 5,
                common.legend = T, legend = "bottom")
ggsave(paste0("./fig/", "s7", ".png"), s7, width = 12, height = 10)


plot <- list()
for(i in 1:5){
  plot[[i]] <- ah_t %>% 
    filter(Area == "Tract") %>% 
    dplyr::select(- Area) %>% 
    mutate(POPDEN = log(POPDEN)) %>% 
    
    filter(Group == group_t[i]) %>% 
    dplyr::select(Group, Host, Buff, relevant_variables_demographics) %>% 
    mutate(Buff = as.character(Buff)) %>% 
    gather(key = "variable", value = "value", LOWINCOME:POPDEN) %>% 
    
    ggplot(aes(x = Buff, y = value, color = Host)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~variable, nrow = 1, scales = "free") +
    labs(x = "Distance (miles)", y = "Value", title = group_t[i]) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0),
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8), # Center the title
          legend.position = "none") # Center the title
  
}

s8 <- ggarrange(plot[[1]],plot[[2]],plot[[3]], 
                plot[[4]],plot[[5]],nrow = 5,
                common.legend = T, legend = "bottom")
ggsave(paste0("./fig/", "s8", ".png"), s8, width = 12, height = 10)



plot <- list()
for(i in 1:3){
  plot[[i]] <- ah %>% 
    filter(Area == "County") %>% 
    dplyr::select(- Area) %>% 
    drop_na() %>% 
    mutate(POPDEN = log(POPDEN)) %>% 
    
    filter(Group == group_g[i]) %>% 
    dplyr::select(Group, Host, Buff, relevant_variables_demographics) %>% 
    mutate(Buff = as.character(Buff)) %>% 
    gather(key = "variable", value = "value", POPDEN:LESSHS) %>% 
    
    ggplot(aes(x = Buff, y = value, color = Host)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~variable, nrow = 1, scales = "free") +
    labs(x = "Distance (miles)", y = "Value", title = group_g[i]) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0),
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8), # Center the title
          legend.position = "none") # Center the title
  
}

s9 <- ggarrange(plot[[1]],plot[[2]],plot[[3]], nrow = 3,
                common.legend = T, legend = "bottom")
ggsave(paste0("./fig/", "s9", ".png"), s9, width = 12, height = 8)



plot <- list()
for(i in 4:10){
  plot[[i-3]] <- ah %>% 
    filter(Area == "County") %>% 
    dplyr::select(- Area) %>% 
    drop_na() %>% 
    mutate(POPDEN = log(POPDEN)) %>% 
    
    filter(Group == group_g[i]) %>% 
    dplyr::select(Group, Host, Buff, relevant_variables_demographics) %>% 
    mutate(Buff = as.character(Buff)) %>% 
    gather(key = "variable", value = "value", POPDEN:LESSHS) %>% 
    
    ggplot(aes(x = Buff, y = value, color = Host)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~variable, nrow = 1, scales = "free") +
    labs(x = "Distance (miles)", y = "Value", title = group_g[i]) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0),
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8), # Center the title
          legend.position = "none") # Center the title
  
}

s10 <- ggarrange(plot[[1]],plot[[2]],plot[[3]], 
                 plot[[4]],plot[[5]],plot[[6]],plot[[7]],nrow = 7,
                 common.legend = T, legend = "bottom")
ggsave(paste0("./fig/", "s10", ".png"), s10, width = 12, height = 12)


plot <- list()
for(i in 1:5){
  plot[[i]] <- ah_s %>% 
    filter(Area == "County") %>% 
    dplyr::select(- Area) %>% 
    mutate(POPDEN = log(POPDEN)) %>% 
    
    filter(Group == group_s[i]) %>% 
    dplyr::select(Group, Host, Buff, relevant_variables_demographics) %>% 
    mutate(Buff = as.character(Buff)) %>% 
    gather(key = "variable", value = "value", LOWINCOME:POPDEN) %>% 
    
    ggplot(aes(x = Buff, y = value, color = Host)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~variable, nrow = 1, scales = "free") +
    labs(x = "Distance (miles)", y = "Value", title = group_s[i]) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0),
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8), # Center the title
          legend.position = "none") # Center the title
}

s11 <- ggarrange(plot[[1]],plot[[2]],plot[[3]], 
                 plot[[4]],plot[[5]],nrow = 5,
                 common.legend = T, legend = "bottom")
ggsave(paste0("./fig/", "s11", ".png"), s11, width = 12, height = 10)


plot <- list()
for(i in 1:5){
  plot[[i]] <- ah_t %>% 
    filter(Area == "County") %>% 
    dplyr::select(- Area) %>% 
    mutate(POPDEN = log(POPDEN)) %>% 
    
    filter(Group == group_t[i]) %>% 
    dplyr::select(Group, Host, Buff, relevant_variables_demographics) %>% 
    mutate(Buff = as.character(Buff)) %>% 
    gather(key = "variable", value = "value", LOWINCOME:POPDEN) %>% 
    
    ggplot(aes(x = Buff, y = value, color = Host)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~variable, nrow = 1, scales = "free") +
    labs(x = "Distance (miles)", y = "Value", title = group_t[i]) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0),
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8), # Center the title
          legend.position = "none") # Center the title
  
}

s12 <- ggarrange(plot[[1]],plot[[2]],plot[[3]], 
                 plot[[4]],plot[[5]],nrow = 5,
                 common.legend = T, legend = "bottom")
ggsave(paste0("./fig/", "s12", ".png"), s12, width = 12, height = 10)

# regression
area <- c("Tract","County")
buffers = rev(c("0", "1", "3"))
vrn <- c("POPDEN","LOWINCOME","PEOPCOLOR","UNEMPLOYED","LINGISO","LESSHS","UNDER5","OVER64")

d <- data.frame()
for(i in 1:length(group_g)){
  for(k in 1:2){
    for(j in 1:length(buffers)){
      for(m in 1:4){
        num <- c(2,3,6,8)
        
        vr = vrn[1:num[m]]
        
        # Create the formula dynamically
        mod <- as.formula(paste("Host ~", paste(vr, collapse = " + ")))
        
        dat <- ah %>% 
          filter(population > 0) %>% 
          filter(Area == area[k]) %>%
          filter(Buff == buffers[j]) %>%
          filter(Group == group_g[i]) %>%
          mutate(POPDEN = log(POPDEN)) %>% 
          dplyr::select(relevant_variables_demographics, Host) %>%
          mutate(across(where(is.numeric) & !c("Host"), ~ scale(.))) %>% 
          mutate(Host = ifelse(Host == "N", 0, 1))
        
        
        fit <- glm(mod, family = binomial(link="logit"), data = dat)
        
        dat <- summary(fit)$coefficients %>% 
          as.data.frame() %>% 
          mutate(color = ifelse(`Pr(>|z|)` < 0.05, "Y", "N")) %>%
          dplyr::select(-"z value",-"Pr(>|z|)") %>% 
          tibble::rownames_to_column("var") %>% 
          filter(!var == "(Intercept)") %>% 
          rename(se = "Std. Error",
                 pe = Estimate) %>% 
          mutate(Group = group_g[i],
                 Buffer = paste0(buffers[j], " mile"),
                 Area = area[k],
                 var = factor(var, levels = c(relevant_variables_demographics)),
                 model = paste0("Model ",m))
        
        d <- rbind(dat, d)
        
      }
      
    }
  }
  
  plt <- d %>% 
    mutate(Area = factor(Area, levels = area),
           color = factor(color, levels = c("Y", "N")),
           var = factor(var, levels = vrn)) %>% 
    filter(Group == group_g[i]) %>% 
    
    ggplot(aes(y = exp(pe), x = var)) +
    geom_hline(yintercept = 1,linetype = "dashed", size = 0.5, color = "gray30") +
    geom_errorbar(aes(ymin=exp(pe-1.96*se), ymax=exp(pe+1.96*se)),color="gray50") +
    geom_point(aes(fill = color),size = 2, color = "black",pch=21) +
    
    # geom_text(data = tp, aes(x = 0, y = n + 0.5, label = cat), hjust = 1) +
    coord_flip() +
    theme_bw() +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", function(x) sprintf("%.1f", x))) +
    labs(x = "", y ="Odds ratio (log scale)", 
         title = group_g[i]) +
    
    facet_grid(model~Area+Buffer) +
    scale_fill_manual(values=c("orange", "gray")) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background =element_rect(fill="gray22",color="gray22"),
          strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
          legend.position = "none",
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8),
          axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
          axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
          plot.title=element_text(family="Franklin Gothic Demi", size=20)) 
  
  ggsave(paste0("./fig/regression/", group_g[i], ".png"), 
         plt, width = 10, height = 7)
  
}

### substation
d <- data.frame()
for(i in 1:length(group_s)){
  for(k in 1:2){
    for(j in 1:length(buffers)){
      for(m in 1:4){
        num <- c(2,3,6,8)
        
        vr = vrn[1:num[m]]
        
        # Create the formula dynamically
        mod <- as.formula(paste("Host ~", paste(vr, collapse = " + ")))
        
        dat <- ah_s %>% 
          filter(population > 0) %>% 
          filter(Area == area[k]) %>%
          filter(Buff == buffers[j]) %>%
          filter(Group == group_s[i]) %>%
          mutate(POPDEN = log(POPDEN)) %>% 
          dplyr::select(relevant_variables_demographics, Host) %>%
          mutate(across(where(is.numeric) & !c("Host"), ~ scale(.))) %>% 
          mutate(Host = ifelse(Host == "N", 0, 1))
        
        
        fit <- glm(mod, family = binomial(link="logit"), data = dat)
        
        dat <- summary(fit)$coefficients %>% 
          as.data.frame() %>% 
          mutate(color = ifelse(`Pr(>|z|)` < 0.05, "Y", "N")) %>%
          dplyr::select(-"z value",-"Pr(>|z|)") %>% 
          tibble::rownames_to_column("var") %>% 
          filter(!var == "(Intercept)") %>% 
          rename(se = "Std. Error",
                 pe = Estimate) %>% 
          mutate(Group = group_s[i],
                 Buffer = paste0(buffers[j], " mile"),
                 Area = area[k],
                 var = factor(var, levels = c(relevant_variables_demographics)),
                 model = paste0("Model ",m))
        
        d <- rbind(dat, d)
        
      }
      
    }
  }
  
  plt <- d %>% 
    mutate(Area = factor(Area, levels = area),
           color = factor(color, levels = c("Y", "N")),
           var = factor(var, levels = vrn)) %>% 
    filter(Group == group_s[i]) %>% 
    
    ggplot(aes(y = exp(pe), x = var)) +
    geom_hline(yintercept = 1,linetype = "dashed", size = 0.5, color = "gray30") +
    geom_errorbar(aes(ymin=exp(pe-1.96*se), ymax=exp(pe+1.96*se)),color="gray50") +
    geom_point(aes(fill = color),size = 2, color = "black",pch=21) +
    
    # geom_text(data = tp, aes(x = 0, y = n + 0.5, label = cat), hjust = 1) +
    coord_flip() +
    theme_bw() +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", function(x) sprintf("%.1f", x))) +
    labs(x = "", y ="Odds ratio (log scale)", 
         title = group_s[i]) +
    
    facet_grid(model~Area+Buffer) +
    scale_fill_manual(values=c("orange", "gray")) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background =element_rect(fill="gray22",color="gray22"),
          strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
          legend.position = "none",
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8),
          axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
          axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
          plot.title=element_text(family="Franklin Gothic Demi", size=20)) 
  
  ggsave(paste0("./fig/regression/S", group_s[i], ".png"), 
         plt, width = 10, height = 7)
  
}


### transmission
d <- data.frame()
for(i in 1:length(group_t)){
  for(k in 1:2){
    for(j in 1:length(buffers)){
      for(m in 1:4){
        num <- c(2,3,6,8)
        
        vr = vrn[1:num[m]]
        
        # Create the formula dynamically
        mod <- as.formula(paste("Host ~", paste(vr, collapse = " + ")))
        
        dat <- ah_t %>% 
          filter(population > 0) %>% 
          filter(Area == area[k]) %>%
          filter(Buff == buffers[j]) %>%
          filter(Group == group_t[i]) %>%
          mutate(POPDEN = log(POPDEN)) %>% 
          dplyr::select(relevant_variables_demographics, Host) %>%
          mutate(across(where(is.numeric) & !c("Host"), ~ scale(.))) %>% 
          mutate(Host = ifelse(Host == "N", 0, 1))
        
        
        fit <- glm(mod, family = binomial(link="logit"), data = dat)
        
        dat <- summary(fit)$coefficients %>% 
          as.data.frame() %>% 
          mutate(color = ifelse(`Pr(>|z|)` < 0.05, "Y", "N")) %>%
          dplyr::select(-"z value",-"Pr(>|z|)") %>% 
          tibble::rownames_to_column("var") %>% 
          filter(!var == "(Intercept)") %>% 
          rename(se = "Std. Error",
                 pe = Estimate) %>% 
          mutate(Group = group_t[i],
                 Buffer = paste0(buffers[j], " mile"),
                 Area = area[k],
                 var = factor(var, levels = c(relevant_variables_demographics)),
                 model = paste0("Model ",m))
        
        d <- rbind(dat, d)
        
      }
      
    }
  }
  
  plt <- d %>% 
    mutate(Area = factor(Area, levels = area),
           color = factor(color, levels = c("Y", "N")),
           var = factor(var, levels = vrn)) %>% 
    filter(Group == group_t[i]) %>% 
    
    ggplot(aes(y = exp(pe), x = var)) +
    geom_hline(yintercept = 1,linetype = "dashed", size = 0.5, color = "gray30") +
    geom_errorbar(aes(ymin=exp(pe-1.96*se), ymax=exp(pe+1.96*se)),color="gray50") +
    geom_point(aes(fill = color),size = 2, color = "black",pch=21) +
    
    # geom_text(data = tp, aes(x = 0, y = n + 0.5, label = cat), hjust = 1) +
    coord_flip() +
    theme_bw() +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", function(x) sprintf("%.1f", x))) +
    labs(x = "", y ="Odds ratio (log scale)", 
         title = group_t[i]) +
    
    facet_grid(model~Area+Buffer) +
    scale_fill_manual(values=c("orange", "gray")) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background =element_rect(fill="gray22",color="gray22"),
          strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
          legend.position = "none",
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8),
          axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
          axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
          plot.title=element_text(family="Franklin Gothic Demi", size=20)) 
  
  ggsave(paste0("./fig/regression/T", group_t[i], ".png"), 
         plt, width = 10, height = 7)
  
}


TP_g <- data.frame()
for(i in 1:length(group_g)){
  
  tp <- regr1(pw, group_g[i])
  TP_g <- rbind(tp, TP_g)
  
}


dac <- TP_g %>% 
  mutate(Group = factor(Group, levels = group),
         Sig = ifelse(Sig == "Y","Significant","Not significant"),
         Sig = factor(Sig, levels = c("Significant","Not significant")),
         var = factor(var, levels = c(rev(c("POPDEN",setdiff(relevant_variables_demographics, "POPDEN"))), "Year", "DAC"))) %>% 
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
        strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=10,face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=5),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=11),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))

ggsave("./fig/s21.png", dac, width = 12, height = 6)


### regression
### confusion matrix
library(caret)

# generators 
d <- data.frame()
area <- c("Tract","County")
buffers = rev(c("0", "1", "3"))


df <- ah %>% 
  rbind(ah_s %>% 
          mutate(Group = "Substations") %>% 
          dplyr::select(names(ah))) %>% 
  
  rbind(ah_t %>% 
          mutate(Group = "Transmission") %>% 
          dplyr::select(names(ah)))

for(i in 1:length(group)){
  
  for(k in 1:2){
    for(j in 1:length(buffers)){
      
      dat <- df %>% 
        filter(population > 0) %>% 
        filter(Area == area[k]) %>%
        filter(Buff == buffers[j]) %>%
        filter(Group == group[i]) %>%
        mutate(POPDEN = log(POPDEN)) %>% 
        dplyr::select(relevant_variables_demographics, Host) %>%
        mutate(across(where(is.numeric) & !c("Host"), ~ scale(.))) %>% 
        mutate(
          # Host = ifelse(Host == "N", 0, 1),
          Host = factor(Host, levels = c("N", "Y")))
      
      
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
  mutate(Buffer = paste0(Buffer," mile")) %>% 
  
  ggplot(aes(x = mean, y = key, xmin=lower, xmax=upper)) +
  geom_col(aes(x = Total, y = key), fill = "gray", position = "dodge", width = 0.9) +
  geom_col(fill = "cornflowerblue", position = "dodge", width = 0.9, alpha = 0.5) +
  geom_errorbar(width=0.4, colour="black", alpha=0.9, size=0.7, position=position_dodge(.9)) +
  
  labs(x = "Performance (%)", y = "", fill = "", title = "Cross validation perfermance") + 
  scale_fill_viridis_d(begin = 0.2, end = 0.95) +
  facet_grid(Area+Buffer~Group, scales = "free", space = "free") +
  scale_x_continuous(breaks = c(0, 30, 60, 90)) +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=9),
        legend.position = "none",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=10),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 

ggsave("./fig/accuracy.png", plt, width = 12, height = 6)
