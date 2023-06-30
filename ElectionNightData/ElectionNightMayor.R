###This processes election night data

library(readxl)
library(tidyverse)

setwd("~/Documents/madison-elections/ElectionNightData")
#download.file(url = "https://elections.countyofdane.com/Precinct-ResultExcel/148/0005", 
#              destfile = "primary.xlsx", mode="wb")



download.file(url = "https://elections.countyofdane.com/Precinct-ResultExcel/153/0015", 
             destfile = "general.xlsx", mode="wb")


primary_results <- read_excel("primary.xlsx", skip = 6)
#general_results <- primary_results
general_results <- read_excel("general.xlsx", skip = 6)
Ward_District <- read_excel("~/Documents/madison-elections/ElectionData/Ward-District.xlsx")
Alder_District <- read_excel("~/Documents/madison-elections/ElectionData/Alder-District.xlsx")

results.primary <- primary_results %>%
  gather(key = "Candidate", value = "Vote",
         -`Precincts`) %>%
  mutate(Ward = str_sub(Precincts, -3)) %>%
  left_join(Ward_District, by = "Ward") %>%
  inner_join(Alder_District, by = "Aldermanic District")

results.district.primary.satya <- results.primary %>%
  group_by(`Aldermanic District`, Candidate) %>%
  summarize(Primary_Vote = sum(Vote)) %>%
  group_by(`Aldermanic District`) %>%
  mutate(Primary_prop = Primary_Vote/sum(Primary_Vote)) %>%
  filter(Candidate == "Satya Rhodes-Conway (Non)") %>%
  select(-Candidate)


ward.district.vote.share <- results.primary %>%
          group_by(`Aldermanic District`, Ward) %>%
        summarize(n = sum(Vote)) %>%
  group_by(`Aldermanic District`) %>%
  mutate(District_Prop = n/sum(n)) %>%
  select(-n)

district.prop.in <-  general_results %>%
     gather(key = "Candidate", value = "Vote",
            -`Precincts`) %>%
     mutate(Ward = str_sub(Precincts, -3)) %>%
     group_by(Ward)  %>%
     summarize(n = sum(Vote)) %>%
     mutate(WardVoteIn = ifelse(n > 0, TRUE, FALSE)) %>%
    inner_join(ward.district.vote.share, by = "Ward") %>%
     mutate(District_Prop = ifelse(WardVoteIn, District_Prop, 0)) %>%
     group_by(`Aldermanic District`) %>%
     summarize(District_Prop = sum(District_Prop))

results.district.general.satya <- general_results %>%
  gather(key = "Candidate", value = "Vote",
         -`Precincts`) %>%
  mutate(Ward = str_sub(Precincts, -3)) %>%
  left_join(Ward_District, by = "Ward") %>%
  group_by(Candidate, `Aldermanic District`) %>%
  summarize(General_Vote = sum(Vote)) %>%
  group_by(`Aldermanic District`) %>%
  mutate(General_Prop = General_Vote/sum(General_Vote)) %>%
  filter(Candidate ==  "Satya Rhodes-Conway (Non)") %>%
  inner_join(results.district.primary.satya, by = "Aldermanic District") %>%
inner_join(district.prop.in, by = "Aldermanic District") %>%
  select(-Candidate) %>%
  mutate(Vote_Prop_Diff = General_Prop-Primary_prop) %>%
  arrange(by = -District_Prop)

print(Sys.time())

results.district.general.satya

