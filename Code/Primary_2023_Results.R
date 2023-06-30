##This calculates the "Satya" index for each aldermanic district 
#based upon the spring primary.

library(tidyverse)
library(readxl)

Spring_Primary_2023_02_21 <- read_excel("ElectionData/Spring-Primary_2023-02-21.xlsx",    
                                        skip = 3)
Ward_District <- read_excel("ElectionData/Ward-District.xlsx")
Alder_District <- read_excel("ElectionData/Alder-District.xlsx")

results.primary.2023 <- Spring_Primary_2023_02_21 %>%
                      gather(key = "Candidate", value = "Vote",
                             -`Precincts`) %>%
                      mutate(Ward = str_sub(Precincts, -3)) %>%
                      left_join(Ward_District, by = "Ward") %>%
                      inner_join(Alder_District, by = "Aldermanic District")

total.vote.ward <- results.primary.2023 %>%
                group_by(`Aldermanic District`, Ward) %>%
                summarize(Vote = sum(Vote, na.rm= TRUE)) %>%
                group_by(`Aldermanic District`) %>%
                mutate(`Prop of Ald District Vote` = Vote/sum(Vote),
                       Ward = as.numeric(Ward)) %>%
                ungroup()
  
satya.share.alder <- results.primary.2023 %>%
                      group_by(`Aldermanic District`, `Alder Last Name`, Candidate) %>%
                      summarize(Vote = sum(Vote, na.rm= TRUE)) %>%
                      group_by(`Aldermanic District`,`Alder Last Name`) %>%
                      mutate(prop  = Vote / sum(Vote),
                             prc = paste(as.character(round(100* (Vote / sum(Vote)), 2)), "%", sep = ""),
                             gain = Vote - (sum(Vote)-Vote)) %>%
                    ungroup()%>%
                        filter(Candidate == "Satya Rhodes-Conway (Non)")


satya.share.ward <- results.primary.2023 %>%
                group_by(Ward, Candidate) %>%
                summarize(Vote = sum(Vote, na.rm= TRUE)) %>%
                group_by(Ward) %>%
                mutate(prop  = Vote / sum(Vote),
                       prc = paste(as.character(round(100* (Vote / sum(Vote)), 2)), "%", sep = ""),
                       gain = Vote - (sum(Vote)-Vote),
                       Ward = as.numeric(Ward)) %>%
                ungroup()%>%
                filter(Candidate == "Satya Rhodes-Conway (Non)")



##maps

library(rgdal)
library(maptools)
library(broom)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

#Alder Districts
alder_spdf <- readOGR( 
  dsn= paste0(getwd(),"/ElectionData/") , 
  layer="Alder_Districts",
  verbose=FALSE
)
spdf_alder_fortified <- tidy(alder_spdf, region = "ALD_DIST")

spdf_alder_fortified <- spdf_alder_fortified %>%
                  mutate(`Aldermanic District` = as.numeric(id)) %>%
                  inner_join(satya.share.alder[,c("Aldermanic District", "prop", "gain")],
                             by = "Aldermanic District")


ggplot() +
  geom_polygon(data = spdf_alder_fortified, aes( x = long, y = lat, group = group, fill = prop), color="white") +
  theme_void() 

ggplot() +
  geom_polygon(data = spdf_alder_fortified, aes( x = long, y = lat, group = group, fill = gain), color="white") +
  theme_void() 



#Wards
ward_spdf <- readOGR( 
  dsn= paste0(getwd(),"/ElectionData/") , 
  layer="Wards",
  verbose=FALSE
)
spdf_ward_fortified <- tidy(ward_spdf, region = "WARD")

satya_spdf_ward_fortified <- spdf_ward_fortified %>%
  mutate(`Ward` = as.numeric(id)) %>%
  inner_join(satya.share.ward[,c("Ward", "prop", "gain")],
             by = "Ward")

total_vote_ward_fortified <- spdf_ward_fortified %>%
                      mutate(`Ward` = as.numeric(id)) %>%
                  inner_join(total.vote.ward, by = "Ward")

ggplot() +
  geom_polygon(data = spdf_ward_fortified, aes( x = long, y = lat, group = group, fill = prop), color="white") +
  labs(title = "Proportion of Ward Vote for Satya Rhodes-Conway",
       subtitle = "2023 Spring Primary") +
  theme_void() 
  


ggplot() +
  geom_polygon(data = spdf_ward_fortified, aes( x = long, y = lat, group = group, fill = gain), color="white") +
  labs(title = "Satya Rhodes-Conway Vote Gain (loss) by Ward",
       subtitle = "2023 Spring Primary") +
  theme_void() 



#total vote count by facet

total_vote_ward_fortified %>%
  filter(`Aldermanic District` == 11) %>%
ggplot() +
  geom_polygon(aes( x = long, y = lat, 
                    group = group, fill = `Prop of Ald District Vote`), color="white") +
  labs(title = "Proportion of Total Aldermanic District Vote by Ward",
       subtitle = "2023 Spring Primary") +
  theme_void() 



