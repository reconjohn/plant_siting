
# donut
donut <- function(data, group){
  
  data %>% 
    mutate(BUFF_DIST = as.character(BUFF_DIST)) %>% 
    gather(key = "variable", value = "value", POPDEN:OVER64) %>% 
    filter(Group == group) %>% 
    ggplot(aes(x = BUFF_DIST, y = value)) +
    geom_boxplot() +
    facet_wrap(~variable, ncol = 4, scales = "free") +
    labs(x = "Buffer", y = "Value", title = group) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) # Center the title

}

# donut(pw, "Wind")

box_pl <- function(data, group, area){
  data %>% 
    filter(Area == area) %>% 
    dplyr::select( - Area) %>% 
    drop_na() %>% 
    mutate(POPDEN = log(POPDEN)) %>% 
    
    filter(Group == group) %>% 
    dplyr::select(Group, Host, Buff, relevant_variables_demographics) %>% 
    mutate(Buff = as.character(Buff)) %>% 
    gather(key = "variable", value = "value"  , POPDEN:LESSHS) %>% 
    
    ggplot(aes(x = Buff, y = value, color = Host)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~variable, ncol = 4, scales = "free") +
    labs(x = "Buffer", y = "Value", title = paste0(group)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom") # Center the title
  
}

# box_pl(ah, "Wind", "Tract")


regr_pl <- function(data, group, area, buffer){
        
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
                 var = factor(var, levels = c(relevant_variables_demographics))) %>% 
          
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
               title = paste0(group)) +
          
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
        
}

  
# regr_pl(ah, "Wind", "Tract", "3")


mpping <- function(data, group){
  data %>% 
    filter(Group == group) %>% 
    
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
         title = group) +
    theme_minimal() +
    coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2700000), 
             ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
    guides(fill = guide_legend(order = 1),
           color = guide_legend(order = 2), 
           size = guide_legend(order = 3))
    
}

# mpping(pm, "Wind")
