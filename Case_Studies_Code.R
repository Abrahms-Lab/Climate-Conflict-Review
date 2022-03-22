#Code to create figures from Abrahms et al. "Climate change as an amplifier of human-wildlife conflict"
#March 2022

#Load packages
library(tidyverse)
library(viridis)

#Load data
cases <- read_csv("Case_Studies.csv", skip=1)

# World map with locations of case studies
world <- map_data("world")
cases %>%
  distinct(Source, .keep_all = T) %>% #remove duplicate source entries
  ggplot() +
  geom_map(data = world, map = world,aes(long, lat, map_id = region),fill = "lightgray")  +
  geom_point(aes(x=Lon, y=Lat, color = `Essential Climate Variable\n(Primary)`, shape=`Type of Climate Change`), size=4, alpha=0.85) +
  theme_void() +
  scale_color_viridis(option="plasma", direction=1, discrete = TRUE, name = "Climate Variable") +
  guides(shape=guide_legend(title="Climate Scale")) +
  theme(legend.text=element_text(size=11), legend.title = element_text(size=12))

# Histogram by year
cases %>%
  ggplot() +
  geom_histogram(aes(x=Year, fill=`Type of Climate Change`), binwidth=1.5, color='black') +
  theme_classic() +
  scale_fill_viridis(option="magma", discrete=T, begin=0.2, end=0.5) +
  ylab("number of studies") + xlab("year") +
  theme(text = element_text(size=12), legend.position = c(0.3, 0.8)) +
  guides(fill=guide_legend(title="climate scale"))

#Histogram of environmental change - acute event, long-term, interannual change 
cases %>%
  gather(key="variable", value="value", -Source) %>%
  filter(variable %in% c("Resource Abundance",
                         "Resource Distribution",            
                         "Resource Phenology",                             
                         "Community Composition/Other"),
         value==TRUE) %>%
  mutate(variable = reorder(as.factor(variable),as.factor(variable),FUN=function(x) -length(x))) %>%
  ggplot(aes(x=variable)) +
  geom_histogram(stat="count", color='black', size=0.5, fill="#1576a6") +
  theme_classic() +
  guides(fill=guide_legend(title="ecological change")) + xlab(NULL) +
  theme(axis.text.x = element_text(angle = 50, hjust=1), text=element_text(size=15)) +
  ylab("Number of studies")

#Histogram of conflict types by family
cases %>%
  gather(key="outcome", value="value", -Source, -`Focal Family`) %>%
  filter(outcome %in% c("Wildlife Injury / Mortality",
                         "Human Injury / Mortality",                             
                         "Property Damage / Nuisance",                     
                         "Revenue Loss",                                       
                          "Loss of Food Production"),
         value=="TRUE") %>%
  mutate(family = reorder(as.factor(`Focal Family`),as.factor(`Focal Family`),FUN=function(x) length(x)),
         outcome = reorder(as.factor(`outcome`),as.factor(`outcome`),FUN=function(x) length(x))) %>% #reorder factors by freq
  ggplot(aes(x=family, fill=outcome)) +
  geom_histogram(stat="count", color='black', size=0.5) +
  theme_classic() +
  scale_fill_viridis(option="rocket", discrete=T, begin=0.4, end=1) +
  guides(fill=guide_legend(title="Conflict outcome")) + xlab(NULL) +
  theme(text=element_text(size=12), legend.position = c(0.7, 0.25)) +
  ylab("Number of studies") +
  coord_flip()
