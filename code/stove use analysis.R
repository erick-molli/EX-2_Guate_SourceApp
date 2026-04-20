#Packages to use
library(here)
library(readxl)
library(tidyverse)
library(lubridate)

#Open the HAPIN data set and the list of filters from SA
hapin <- read_csv(here("data", "raw-data", "HAPIN_EX17_Guatemala_20230728.csv"))
xrflist <- read_csv(here("data", "raw-data", "sa_filters_emollinedo.csv"))

#Select the variables of interest from the HAPIN data frame
hapin_temp <- hapin %>%
  select(hhid, h41_date, trt_blinded, timepoint, h41_m_ecm_fid, h41_b_ecm_fid, h41b_filter1, 
         h41b_filter2, h42_cook, h42_stove_num, h42_stove1_hours, h42_stove1_Biomass,
         h42_stove1_LPG, h42_stove2_hour, h42_stove2_Biomass, h42_stove2_LPG, h43_stove1)

#Make sure all filter IDs are upper case, using the `mutate()` function with `toupper()`
hapin_temp <- hapin_temp %>% 
  mutate(h41_m_ecm_fid = toupper(h41_m_ecm_fid),
         h41b_filter1 = toupper(h41b_filter1),
         h41_b_ecm_fid = toupper(h41_b_ecm_fid),
         h41b_filter2 = toupper(h41b_filter2))

#Select all filters that match with the list of filters from XRF, using the `filter()` function.
hapin_temp <- hapin_temp %>%
  filter(h41_m_ecm_fid %in% xrflist$filter_id |
           h41b_filter1 %in% xrflist$filter_id |
           h41_b_ecm_fid %in% xrflist$filter_id |
           h41b_filter2 %in% xrflist$filter_id)

#Concatenate `trt_blinded` and `timepoint` into a new temporary variable
hapin_temp$concatenated <- paste(hapin_temp$trt_blinded, hapin_temp$timepoint) 

#Create a new variable `fueltype` that categorizes all the observations by type of fuel ('R' means the visit belongs to the 'Control' treatment and 'Q' for the 'Intervention' treatment)
hapin_temp <- mutate(hapin_temp, fueltype = case_when(
  endsWith(concatenated, "BL") ~ "Biomass", #BL corresponds to the baseline, where all the participants had a biomass type fuel stove
  endsWith(concatenated, "R P1") ~ "Biomass",
  endsWith(concatenated, "R P2") ~ "Biomass",
  endsWith(concatenated, "Q P1") ~ "LPG",
  endsWith(concatenated, "Q P2") ~ "LPG",
  endsWith(concatenated, "R BLP1") ~ "Biomass",
  endsWith(concatenated, "R P1P2") ~ "Biomass",
  endsWith(concatenated, "R P2B1") ~ "Biomass",
  endsWith(concatenated, "Q BLP1") ~ "LPG",
  endsWith(concatenated, "Q P1P2") ~ "LPG",
  endsWith(concatenated, "Q P2B1") ~ "LPG"))

#Create variable 'group' to categorize samples as Baseline, Control and Intervention
hapin_temp <- mutate(hapin_temp, group = case_when(
  endsWith(concatenated, "BL") ~ "Baseline",
  endsWith(concatenated, "R P1") ~ "Control",
  endsWith(concatenated, "R P2") ~ "Control",
  endsWith(concatenated, "Q P1") ~ "Intervention",
  endsWith(concatenated, "Q P2") ~ "Intervention",
  endsWith(concatenated, "R BLP1") ~ "Control",
  endsWith(concatenated, "R P1P2") ~ "Control",
  endsWith(concatenated, "R P2B1") ~ "Control",
  endsWith(concatenated, "Q BLP1") ~ "Intervention",
  endsWith(concatenated, "Q P1P2") ~ "Intervention",
  endsWith(concatenated, "Q P2B1") ~ "Intervention"))

#Rename some variables and delete unnecesary ones
hapin_temp <- hapin_temp %>% rename(filter_id = "h41_m_ecm_fid", dup1_filterid = "h41b_filter1", dup2_filterid = "h41b_filter2",
                         blank_filterid = "h41_b_ecm_fid", date = "h41_date",  arm = "trt_blinded") %>% 
  select(-concatenated) %>% 
  mutate(arm = case_when(arm == 'R' ~ 'Control', arm == 'Q' ~ 'Intervention', TRUE ~ arm))

#Set date in date format
hapin_temp <- hapin_temp %>% mutate(date = mdy(date))

#Creating the dataframe for personal sample filters, and deleting the other columns with unnecessary filter IDs
hapin_personal <- hapin_temp %>% filter(filter_id %in% xrflist$filter_id) %>% 
  select(-c(blank_filterid, dup1_filterid, dup2_filterid))

#Creating the dataframe for duplicates in column 4, and deleting the other columns with unnecessary filter IDs, and renaming `dup1_filterid` to `filter_id`.
hapin_dup1 <- hapin_temp %>% filter(dup1_filterid %in% xrflist$filter_id) %>% 
  select(-c(blank_filterid, filter_id, dup2_filterid)) %>% 
  rename(filter_id = "dup1_filterid")

#Creating the dataframe for duplicates in column 5, and deleting the other columns with unnecessary filter IDs, and renaming `dup2_filterid` to `filter_id`.
hapin_dup2 <- hapin_temp %>% filter(dup2_filterid %in% xrflist$filter_id) %>% 
  select(-c(blank_filterid, filter_id, dup1_filterid)) %>% 
  rename(filter_id = "dup2_filterid")

#Creating the dataframe for blank filters, and deleting the other columns with unnecessary filter IDs, and renaming `blank_filterid` to `filter_id`.
hapin_blanks <- hapin_temp %>% filter(blank_filterid %in% xrflist$filter_id) %>% 
  select(-c(filter_id, dup1_filterid, dup2_filterid)) %>% 
  rename(filter_id = "blank_filterid")

#Combining the `personal` and `dup` dataframes into a single one, using `bind_rows()`
hapin_stoves <- bind_rows(hapin_personal, hapin_dup1, hapin_dup2)

#Remove the observation with filter ID 3M53864
hapin_stoves <- hapin_stoves %>% filter(filter_id != "3M53864")



#Create df for participants if they cooked
cooked_yes <- hapin_stoves %>% filter(h42_cook == "Yes")
cooked_no <- hapin_stoves %>% filter(h42_cook == "No" | is.na(h42_cook))

#Create dfs according to number of stoves they used
one_stove <- cooked_yes %>% filter(h42_stove_num == 1 |is.na(h42_stove_num)) #1 stove
two_stove <- cooked_yes %>% filter(h42_stove_num == "2 or more") #+2 stoves

#Types of stoves used
one_stove %>% count(h42_stove1_Biomass == "Checked") #Chimney, one stove
two_stove %>% count(h42_stove1_Biomass == "Checked" & h42_stove2_Biomass == "Checked") #Both chimneys, two stoves
two_stove %>% count(h42_stove1_Biomass == "Checked" & h42_stove2_Biomass == "Unchecked") #One chimney, two stoves
two_stove %>% count(h42_stove1_Biomass == "Unchecked" & h42_stove2_Biomass == "Checked") #One chimney, two stoves

#Study arm if cooked
cooked_yes %>% count(group)
