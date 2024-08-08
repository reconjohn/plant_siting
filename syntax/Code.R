source("./syntax/Data.R")
source("./syntax/Function.R")

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


# technology boxplot by distance
donut(pw, "Wind")

# technology boxplot by host vs. non-host by distance
box_pl(ah, "Wind", "Tract")

# regression results
regr_pl(ah, "Wind", "Tract", "3")

# generator mapping 
mpping(pm, "Wind")

