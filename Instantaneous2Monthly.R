# Load required libraries
library(dataRetrieval)
library(dplyr)
library(lubridate)
library(geosphere)  # For calculating distances between sites
library(readxl)  # For reading Excel files

# Load the Excel file with site numbers
sites_df <- read_excel("sitesnwisalltemps.xlsx")

# Ensure site numbers are treated as character to preserve leading zeros and filter for 8-digit site numbers
sites_df <- sites_df %>% 
  mutate(SiteNumber = as.character(SiteNumber)) %>% 
  filter(nchar(SiteNumber) == 8)

# Retrieve site metadata to get coordinates
site_metadata <- whatNWISsites(siteNumber = sites_df$SiteNumber)

# Filter for sites with valid latitude and longitude
site_metadata <- site_metadata %>% 
  filter(!is.na(dec_lat_va) & !is.na(dec_long_va)) %>% 
  select(site_no, dec_lat_va, dec_long_va)

# Function to pair sites based on closest geographic proximity within 50 miles
pair_sites_by_proximity <- function(metadata) {
  paired_sites <- data.frame()
  remaining_sites <- metadata
  max_distance <- 50 * 1609.34  # Convert 50 miles to meters
  
  while (nrow(remaining_sites) > 1) {
    site1 <- remaining_sites[1, ]
    remaining_sites <- remaining_sites[-1, ]
    
    distances <- distHaversine(cbind(site1$dec_long_va, site1$dec_lat_va), 
                               cbind(remaining_sites$dec_long_va, remaining_sites$dec_lat_va))
    
    nearest_idx <- which.min(distances)
    nearest_distance <- distances[nearest_idx]
    
    if (nearest_distance <= max_distance) {
      site2 <- remaining_sites[nearest_idx, ]
      
      paired_sites <- bind_rows(paired_sites, 
                                data.frame(instantaneous_site = site1$site_no, 
                                           continuous_site = site2$site_no))
      
      remaining_sites <- remaining_sites[-nearest_idx, ]
    }
  }
  
  return(paired_sites)
}

# Generate all possible site pairs based on proximity within 50 miles
paired_sites <- pair_sites_by_proximity(site_metadata)

# Define the time range for August
start_date <- '2023-08-01'
end_date <- '2023-08-31'

# Initialize a dataframe to store results
all_results <- data.frame()

# Loop through each paired site
for (i in 1:nrow(paired_sites)) {
  
  site_instantaneous <- paired_sites$instantaneous_site[i]
  site_continuous <- paired_sites$continuous_site[i]
  
  # Retrieve continuous temperature data for both sites
  data_instantaneous <- readNWISuv(siteNumbers = site_instantaneous, parameterCd = '00010', startDate = start_date, endDate = end_date)
  data_continuous <- readNWISuv(siteNumbers = site_continuous, parameterCd = '00010', startDate = start_date, endDate = end_date)
  
  # Identify the correct temperature column name dynamically (exclude quality code columns)
  temp_col_instantaneous <- grep('^X_00010_00000$', names(data_instantaneous), value = TRUE)
  temp_col_continuous <- grep('^X_00010_00000$', names(data_continuous), value = TRUE)
  
  # Skip if temperature columns are not found
  if (length(temp_col_instantaneous) == 0 | length(temp_col_continuous) == 0) {
    next
  }
  
  # Prepare data: select temperature and timestamps
  data_instantaneous <- data_instantaneous %>% 
    select(dateTime, temp_instantaneous = all_of(temp_col_instantaneous)) %>% 
    filter(!is.na(temp_instantaneous))
  
  data_continuous <- data_continuous %>% 
    select(dateTime, temp_continuous = all_of(temp_col_continuous)) %>% 
    filter(!is.na(temp_continuous))
  
  # Proceed only if both datasets have data
  if (nrow(data_instantaneous) > 0 & nrow(data_continuous) > 0) {
    
    # Select a random instantaneous reading
    set.seed(42 + i)  # Change seed for variation across iterations
    instantaneous_sample <- data_instantaneous %>% sample_n(1)
    instantaneous_time <- instantaneous_sample$dateTime
    instantaneous_temp <- instantaneous_sample$temp_instantaneous
    
    # Find the closest continuous reading within 1 hour
    time_window <- hours(1)
    closest_continuous <- data_continuous %>% 
      filter(between(dateTime, instantaneous_time - time_window, instantaneous_time + time_window)) %>% 
      arrange(abs(difftime(dateTime, instantaneous_time, units = "mins"))) %>% 
      slice(1)
    
    # Check if a close reading exists
    if (nrow(closest_continuous) > 0) {
      continuous_temp_at_instant <- closest_continuous$temp_continuous
      
      # Calculate the offset
      temp_offset <- instantaneous_temp - continuous_temp_at_instant
      
      # Estimate mean August temperature at instantaneous site
      mean_august_temp_continuous <- mean(data_continuous$temp_continuous)
      estimated_mean_august_temp <- mean_august_temp_continuous + temp_offset
      
      # Actual mean August temperature at the instantaneous site
      actual_mean_august_temp <- mean(data_instantaneous$temp_instantaneous)
      
      # Store results
      result <- data.frame(
        Instantaneous_Site = site_instantaneous,
        Continuous_Site = site_continuous,
        Instantaneous_Reading_Time = instantaneous_time,
        Instantaneous_Temperature = instantaneous_temp,
        Closest_Continuous_Reading_Time = closest_continuous$dateTime,
        Continuous_Temperature_at_Instant = continuous_temp_at_instant,
        Temperature_Offset = temp_offset,
        Mean_August_Temp_Continuous_Site = mean_august_temp_continuous,
        Estimated_Mean_August_Temp_Instant_Site = estimated_mean_august_temp,
        Actual_Mean_August_Temp_Instant_Site = actual_mean_august_temp,
        Estimation_Error = estimated_mean_august_temp - actual_mean_august_temp
      )
      
      all_results <- bind_rows(all_results, result)
    }
  }
}

# Display all results
print(all_results)

# Summary of estimation errors
summary(all_results$Estimation_Error)
library(tidyverse)

all.lm=lm(all_results$Estimated_Mean_August_Temp_Instant_Site~all_results$Actual_Mean_August_Temp_Instant_Site)
summary(all.lm)

all_results%>%
  ggplot(aes(y=Estimated_Mean_August_Temp_Instant_Site, x=Actual_Mean_August_Temp_Instant_Site))+
  geom_point()+
  geom_smooth()+
  annotate("text",x=25, y=10, label= "r^2 = 0.95, p<0.01", size=6)+
  theme_classic()
  
  
