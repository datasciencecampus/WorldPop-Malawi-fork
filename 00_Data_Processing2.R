# Summarizing data at the EA level using EA-CODE and Spatial location of the points

#load packages
library(sf)
library(haven)
library(tidyverse)
#library(foreign)

options(scipen = 999) # turn off scientific notation for all variables

#Specify Drive Path
drive_path <- "//Internal/MALAWI/Ortis/"
input_path <- paste0(drive_path, "Input_Data/Surveys/")
output_path <- paste0(drive_path, "/Output_Data/")
shapefile_path <- paste0(drive_path, "Input_Data/Shapefiles/")

#Load datasets
mphc_2018 <- read_dta(paste0(input_path, "mphc2018Data_AllRegions.dta"))
ICT_data <- read_dta(paste0(input_path, "ICT Listing WorldPop.dta"))
IHS6_data <- read_dta(paste0(input_path, "IHS6 Listing WorldPop.dta"))
Naca_data <- read_dta(paste0(input_path, "Naca Listing WorldPop.dta"))
ea <- st_read(file.path(shapefile_path, "EA_Shapefile.shp"))

#####################################################################################
####################################################################################
######### PROCESS CENSUS DATA ################################################

# Filter records without GPS coordinates
mphc_2018_no_gps <- mphc_2018 %>% 
  filter(is.na(hh_longitude) | is.na(hh_latitude))

#Add additonal digits to EA and TA code

mphc_2018_no_gps <- mphc_2018_no_gps %>%  
  mutate(new_ta = str_pad(ta, width = 2, pad = 0),
         new_ea = str_pad(ea, width = 3, pad = 0))

#check unique values
unique(mphc_2018_no_gps$new_ta)
unique(mphc_2018_no_gps$new_ea)

#Create EA_CODE by concatenating district, new_ta and new_ea code
mphc_2018_no_gps <- mphc_2018_no_gps %>%  
  mutate(EA_CODE = str_c(district, new_ta, new_ea),
         new_ta_ea = str_c(new_ta, new_ea),
         hh_count = 1)  # Individual observation

# Summarise no gps data at EA 

#total population
mphc_pop_no_gps <- mphc_2018_no_gps %>%  
  group_by(EA_CODE) %>%  
  summarise(mphc_total_pop = sum(hh_count, na.rm = T),
            mphc_hh_count = n_distinct(hhnumber),   #Distinct count of household
            male_count   = sum(p03 == 1, na.rm = TRUE),
            female_count = sum(p03 == 2, na.rm = TRUE))


#Create a bin for each age catgeory
age_summary_no_gps <- mphc_2018_no_gps %>%  
  mutate(age_group = case_when(
    p05 < 1  ~ "age_group_01_less",      #less than 1
    p05 >= 1 & p05 <= 4   ~ "age_group_01_04",
    p05 >= 5 & p05 <= 9   ~ "age_group_05_09",
    p05 >= 10 & p05 <= 14 ~ "age_group_10_14",
    p05 >= 15 & p05 <= 19 ~ "age_group_15_19",
    p05 >= 20 & p05 <= 24 ~ "age_group_20_24",
    p05 >= 25 & p05 <= 29 ~ "age_group_25_29",
    p05 >= 30 & p05 <= 34 ~ "age_group_30_34",
    p05 >= 35 & p05 <= 39 ~ "age_group_35_39",
    p05 >= 40 & p05 <= 44 ~ "age_group_40_44",
    p05 >= 45 & p05 <= 49 ~ "age_group_45_49",
    p05 >= 50 & p05 <= 54 ~ "age_group_50_54",
    p05 >= 55 & p05 <= 59 ~ "age_group_55_59",
    p05 >= 60 & p05 <= 64 ~ "age_group_60_64",
    p05 >= 65 & p05 <= 69 ~ "age_group_65_69",
    p05 >= 70 & p05 <= 74 ~ "age_group_70_74",
    p05 >= 75 & p05 <= 79 ~ "age_group_75_79",
    p05 >= 80             ~ "age_group_80plus",
    TRUE ~ NA_character_
  )) %>%  
  
  # summarise counts per EA_CODE × age_group
  group_by(EA_CODE, age_group) %>%  
  summarise(count = n(), .groups = "drop") %>%  
  arrange(EA_CODE, age_group)%>%  
  
  #Pivot to wide columns
  pivot_wider(
    names_from = age_group,
    values_from = count,
    values_fill = 0
  ) %>%  
  arrange(EA_CODE)

#Join it to pop data
mphc_pop_no_gps <- mphc_pop_no_gps %>%  
  left_join(age_summary_no_gps, by = "EA_CODE")

# ============================================================== ----------

# Convert remaining mphc_2018 data to shapefiles

# #Convert to sf object
 mphc_2018_sf <- mphc_2018 %>%  
   drop_na(hh_longitude, hh_latitude) %>%  
   st_as_sf(coords = c("hh_longitude", "hh_latitude"))
 
# #set the spatial reference
 st_crs(mphc_2018_sf) <- 4326

 #Fix corrupt geometries
 st_make_valid(ea)
 
 #Turn off invalid geometries
 sf::sf_use_s2(FALSE)
 
 #transform
 mphc_2018_sf <- st_transform(mphc_2018_sf, crs = st_crs(ea))
 
 # EA Nearest Neighbor Assignment 
 nearest_indices <- st_nearest_feature(mphc_2018_sf, ea)
 
 # Extract the EA_CODE  of the nearest polygons
 nearest_ids <- ea$EA_CODE[nearest_indices]
 
 # Add the EA_CODE to data
 mphc_2018_sf$EA_CODE <- nearest_ids
 
 #Write to file
 #st_write(mphc_2018_sf , 
 # dsn = file.path(output_path, "mphc_2018_sf_ea.gpkg"), 
 # driver = "GPKG", 
 # delete_layer = TRUE)
 
 #load dataset
 #mphc_2018_sf <- st_read(paste0(output_path, "mphc_2018_sf_ea.gpkg"))
 
 #convert to dataframe
 mphc_2018_df <- mphc_2018_sf %>%  
   as_tibble() 
 
 #check the summary of gps accuracy
 summary(mphc_2018_df$hh_gps_accuracy)
 
# Summarize data base on their spatial location
 
mphc_2018_pop_spatial <- mphc_2018_df %>%  
  group_by(EA_CODE) %>%  
  summarise(mphc_total_pop = sum(hh_count, na.rm = T),
            mphc_hh_count = n_distinct(hhnumber),   #Distinct count of household
            male_count   = sum(p03 == 1, na.rm = TRUE),
            female_count = sum(p03 == 2, na.rm = TRUE))


#Create a bin for each age catgeory
age_summary_spatial <- mphc_2018_df %>%  
  mutate(age_group = case_when(
    p05 < 1  ~ "age_group_01_less",      #less than 1
    p05 >= 1 & p05 <= 4   ~ "age_group_01_04",
    p05 >= 5 & p05 <= 9   ~ "age_group_05_09",
    p05 >= 10 & p05 <= 14 ~ "age_group_10_14",
    p05 >= 15 & p05 <= 19 ~ "age_group_15_19",
    p05 >= 20 & p05 <= 24 ~ "age_group_20_24",
    p05 >= 25 & p05 <= 29 ~ "age_group_25_29",
    p05 >= 30 & p05 <= 34 ~ "age_group_30_34",
    p05 >= 35 & p05 <= 39 ~ "age_group_35_39",
    p05 >= 40 & p05 <= 44 ~ "age_group_40_44",
    p05 >= 45 & p05 <= 49 ~ "age_group_45_49",
    p05 >= 50 & p05 <= 54 ~ "age_group_50_54",
    p05 >= 55 & p05 <= 59 ~ "age_group_55_59",
    p05 >= 60 & p05 <= 64 ~ "age_group_60_64",
    p05 >= 65 & p05 <= 69 ~ "age_group_65_69",
    p05 >= 70 & p05 <= 74 ~ "age_group_70_74",
    p05 >= 75 & p05 <= 79 ~ "age_group_75_79",
    p05 >= 80             ~ "age_group_80plus",
    TRUE ~ NA_character_
  )) %>%  
  
  # summarise counts per EA_CODE × age_group
  group_by(EA_CODE, age_group) %>%  
  summarise(count = n(), .groups = "drop") %>%  
  arrange(EA_CODE, age_group)%>%  
  
  #Pivot to wide columns
  pivot_wider(
    names_from = age_group,
    values_from = count,
    values_fill = 0
  ) %>%  
  arrange(EA_CODE)

#Join it to pop data
mphc_2018_pop_spatial <- mphc_2018_pop_spatial %>%  
  left_join(age_summary_spatial, by = "EA_CODE") 
  

# ================================ ----------------------------------------

#Rbind mphc_pop_no_gps and mphc_pop_spatial

#check the variable names
names(mphc_pop_no_gps)
names(mphc_2018_pop_spatial)

#rbind both dataset
mphc_rbind <- rbind(mphc_2018_pop_spatial, mphc_pop_no_gps)
  
#Summarize overall data
mphc_rbind <- mphc_rbind %>%  
  group_by(EA_CODE) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
  
  
#Check if age columns sum to total_pop
mphc_rbind <- mphc_rbind %>%
  mutate(
    tally_age = ifelse(
      rowSums(select(., starts_with("age_group_")), na.rm = TRUE) == mphc_total_pop,
      "true", "false"
    )
  )

 sum(mphc_rbind$mphc_total_pop) 
 nrow(mphc_2018) 
 
 #Calculate hh_size
 mphc_rbind <- mphc_rbind %>%  
   mutate(hh_size = mphc_total_pop/mphc_hh_count)
 
 #####################################################################################
 ####################################################################################
 ######### PROCESS ICT DATA ################################################ 
 
 #Add a new column to data called hh_count
 ICT_data <- ICT_data %>%  
   mutate(hh_count = 1)
 
 # Filter records without GPS coordinates
 ict_no_gps <- ICT_data %>% 
   filter(is.na(GPS__Longitude) | is.na(GPS__Latitude))
 
 #summarize hh count
 ict_no_gps <- ict_no_gps %>%  
   group_by(EA_Number) %>%  
   summarise(ict_hh_count = sum(hh_count, na.rm = T)) %>%  
   ungroup()
 
 
 #ICT
  ICT_sf <- ICT_data %>%  
    drop_na(GPS__Longitude, GPS__Latitude) %>%  
   st_as_sf(coords = c("GPS__Longitude", "GPS__Latitude"))
  
 # #set the spatial reference
  st_crs(ICT_sf) <- 4326
 
  #transform
  ICT_sf <- st_transform(ICT_sf, crs = st_crs(ea))
  
  # EA Nearest Neighbor Assignment 
  nearest_indices <- st_nearest_feature(ICT_sf, ea)
  
  # Extract the EA_CODE  of the nearest polygons
  nearest_ids <- ea$EA_CODE[nearest_indices]
  
  # Add the EA_CODE to data
  ICT_sf$EA_CODE <- nearest_ids
  
#convert data to tibble
  ICT_sf <- ICT_sf %>%  
    as_tibble()
  
#summarize gps accuracy
  summary(ICT_sf$GPS__Accuracy)
  
 # if gps accuracy is greater than 5m summarize data in original EA Code
  # If less than 5m summarize at spatial location
  
#filter out point more than 
  ICT_greater <- ICT_sf %>%  
    filter(GPS__Accuracy > 5)
  
  #summarize hh count
  ICT_greater <- ICT_greater %>%  
    group_by(EA_Number) %>%    #Use orginal EA_Number of data point
    summarise(ict_hh_count = sum(hh_count, na.rm = T)) %>%  
    ungroup()
  
  sum(ICT_greater$ict_hh_count)
  
  #filter out point less than 5
  ICT_less <- ICT_sf %>%  
    filter(GPS__Accuracy < 5)
  
  #summarize hh count
  ICT_less <- ICT_less %>%  
    group_by(EA_CODE) %>%       # use EA_CODE
    summarise(ict_hh_count = sum(hh_count, na.rm = T)) %>%  
    ungroup() %>%  
    rename(EA_Number = EA_CODE)  #Rename EA_CODE to EA_Number
  
  sum(ICT_less$ict_hh_count)
  
  #Rbind the three ICT data partitions
  ICT_rbind <- rbind(ict_no_gps, ICT_greater, ICT_less)
  
  #Summarize overall data
  ICT_rbind <- ICT_rbind %>%  
    group_by(EA_Number) %>%
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
  
# check whether data adds up to original
  sum(ICT_rbind$ict_hh_count)
  sum(ICT_data$hh_count)
  
  
  #####################################################################################
  ####################################################################################
  ######### PROCESS IHS6 DATA ################################################ 
  
  #Add a new column to data called hh_count
  IHS6_data <- IHS6_data %>%  
    mutate(hh_count = 1)
  
  # Filter records without GPS coordinates
  IHS_no_gps <- IHS6_data %>% 
    filter(is.na(GPS__Longitude) | is.na(GPS__Latitude))
  
  #summarize hh count
  IHS_no_gps <- IHS_no_gps %>%  
    group_by(EA_CODE) %>%  
    summarise(ihs_hh_count = sum(hh_count, na.rm = T)) %>%  
    ungroup()
  
  sum(IHS_no_gps$ihs_hh_count)
  
  #Convert to sf object
  IHS_sf <- IHS6_data %>%  
    drop_na(GPS__Longitude, GPS__Latitude) %>%  
    st_as_sf(coords = c("GPS__Longitude", "GPS__Latitude"))
  
  # #set the spatial reference
  st_crs(IHS_sf) <- 4326
  
  #transform
  IHS_sf <- st_transform(IHS_sf, crs = st_crs(ea))
  
  # EA Nearest Neighbor Assignment 
  nearest_indices <- st_nearest_feature(IHS_sf, ea)
  
  # Extract the EA_CODE  of the nearest polygons
  nearest_ids <- ea$EA_CODE[nearest_indices]
  
  # Add the EA_CODE to data
  IHS_sf$EA_CODE2 <- nearest_ids
  
  #Write to file
 # st_write(IHS_sf , 
  # dsn = file.path(output_path, "ihs_sf_ea.gpkg"), 
   #driver = "GPKG", 
   #delete_layer = TRUE)
  
  #convert data to tibble
  IHS_sf <- IHS_sf %>%  
    as_tibble()
  
  #summarize gps accuracy
  summary(IHS_sf$GPS__Accuracy)
  
  # if gps accuracy is greater than 5m summarize data in original EA Code
  # If less than 5m summarize at spatial location
  
  #filter out point more than 
  IHS_greater <- IHS_sf %>%  
    filter(GPS__Accuracy > 5)
  
  #summarize hh count
  IHS_greater <- IHS_greater %>%  
    group_by(EA_CODE) %>%    #Use orginal EA_Number of data point
    summarise(ihs_hh_count = sum(hh_count, na.rm = T)) %>%  
    ungroup()
  
  sum(IHS_greater$ihs_hh_count)
  
  #filter out point less than 5
  IHS_less <- IHS_sf %>%  
    filter(GPS__Accuracy < 5)
  
  #summarize hh count
  IHS_less <- IHS_less %>%  
    group_by(EA_CODE2) %>%       # use EA_CODE2
    summarise(ihs_hh_count = sum(hh_count, na.rm = T)) %>%  
    ungroup() %>%  
    rename(EA_CODE = EA_CODE2)  #Rename EA_CODE to EA_Number
  
  sum(IHS_less$ihs_hh_count)
  
  #Rbind the three data partitions
  IHS_rbind <- rbind(IHS_no_gps, IHS_greater, IHS_less)
  
  #Summarize overall data
  IHS_rbind <- IHS_rbind %>%  
    group_by(EA_CODE) %>%
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
  
  # check whether data adds up to original
  sum(IHS_rbind$ihs_hh_count)
  sum(IHS6_data$hh_count)
  
  #####################################################################################
  ####################################################################################
  ######### PROCESS NACA DATA ################################################ 
  
  #Add a new column to data called hh_count
  Naca_data <- Naca_data %>%  
    mutate(hh_count = 1)
  
  # Filter records without GPS coordinates
  Naca_no_gps <- Naca_data %>% 
    filter(is.na(longitude) | is.na(latitude))
  
  
  #Convert to sf object
  Naca_sf <- Naca_data %>%  
    drop_na(longitude, latitude) %>%  
    st_as_sf(coords = c("longitude", "latitude"))
  
  # #set the spatial reference
  st_crs(Naca_sf) <- 4326
  
  #transform
  Naca_sf <- st_transform(Naca_sf, crs = st_crs(ea))
  
  # EA Nearest Neighbor Assignment 
  nearest_indices <- st_nearest_feature(Naca_sf, ea)
  
  # Extract the EA_CODE  of the nearest polygons
  nearest_ids <- ea$EA_CODE[nearest_indices]
  
  # Add the EA_CODE to data
  Naca_sf$EA_CODE2 <- nearest_ids
  
  #convert data to tibble
  Naca_sf <- Naca_sf %>%  
    as_tibble()
  
  #summarize gps accuracy
  summary(Naca_sf$accuracy)
  
  # if gps accuracy is greater than 5m summarize data in original EA Code
  # If less than 5m summarize at spatial location
  
  #filter out point more than 
  Naca_greater <- Naca_sf %>%  
    filter(accuracy > 5)
  
  #summarize hh count
  Naca_greater <- Naca_greater %>%  
    group_by(EA_Number) %>%    #Use orginal EA_Number of data point
    summarise(naca_hh_count = sum(hh_count, na.rm = T)) %>%  
    ungroup()
  
  #filter out point less than 5
  Naca_less <- Naca_sf %>%  
    filter(accuracy < 5)
  
  #summarize hh count
  Naca_less <- Naca_less %>%  
    group_by(EA_CODE2) %>%       # use EA_CODE2
    summarise(naca_hh_count = sum(hh_count, na.rm = T)) %>%  
    ungroup() %>%  
    rename(EA_Number = EA_CODE2)  #Rename EA_CODE to EA_Number
  
  #Rbind the three Naca data partitions
  Naca_rbind <- rbind(Naca_greater, Naca_less)
  
  #Summarize overall data
  Naca_rbind <- Naca_rbind %>%  
    group_by(EA_Number) %>%
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
  
  # check whether data adds up to original
  sum(Naca_rbind$naca_hh_count)
  sum(Naca_data$hh_count) 
  
############################################################################
###########################################################################

# Combine Data ------------------------------------------------------------

combined_data <- mphc_rbind %>%  
  left_join(ICT_rbind, by = c("EA_CODE" ="EA_Number")) %>%  
  left_join(IHS_rbind, by = "EA_CODE") %>%  
  left_join(Naca_rbind, by = c("EA_CODE" ="EA_Number"))

#create observed hh_count based on priority conditions
combined_data <- combined_data %>%  
  mutate(
    observed_hh_count = case_when(
      # if ihs_hh_count is available, use it (highest priority)
      !is.na(ihs_hh_count) ~ ihs_hh_count,
      # else if naca_hh_count is available, use it (second priority)
      !is.na(naca_hh_count) ~ naca_hh_count,
      # else if ict_hh_count is available, use it (last priority)
      !is.na(ict_hh_count) ~ ict_hh_count,
      # else put NA
      TRUE ~ NA_real_
    )
  )

#Arrange data
combined_data <- combined_data %>%  
  select(EA_CODE, mphc_total_pop,hh_size, observed_hh_count,
         mphc_hh_count, ict_hh_count, ihs_hh_count,
         naca_hh_count, female_count, male_count, starts_with("age_"))

#Write to file

write.csv(combined_data, paste0(output_path, "summarized_survey_data.csv"), row.names = F)


#join combined data to EA shapefile and export
#Join to ea data
combined_data <- combined_data %>%  
  mutate(EA_CODE = as.character(EA_CODE))  #convert EA code to interger

#select hh size
hh_size <- combined_data %>%  
  dplyr::select(EA_CODE, mphc_total_pop, hh_size)

hh_ea <-full_join(ea, hh_size, by = "EA_CODE")

#select important variables
hh_ea <- hh_ea %>%  
  select(EA_CODE, mphc_total_pop, hh_size)

#write to file
st_write(hh_ea, paste(output_path, "hh_size_data.shp"), append = T)


#################### END OF SCRIPT #########################################
###########################################################################

