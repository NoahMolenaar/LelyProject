# Install and load necessary packages
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("readxl")) install.packages("readxl")
if (!require("ggplot2")) install.packages("ggplot2")


library(tidyr)
library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)

# Set the working directory
setwd("/Volumes/LaCie/Econometrie Master/Blok 3/Lely case study/New Data")

# Import 'devices' data
milkings <- read.csv("milkings.csv", header = TRUE)
activity <- read.csv("activity_data.csv", header = TRUE)
additional_milking_data <- read.csv("additional_milking_data.csv", header = TRUE)
daily_data <- read.csv("daily_cow_data.csv", header = TRUE)
devices_location_bridge <- read.csv("devices_location_bridge.csv", header = TRUE)
devices <- read.csv("devices.csv", header = TRUE)
family <- read.csv("family.csv", header = TRUE)
locations <- read.csv("locations.csv", header = TRUE)

# Merge milkings with devices_location_bridge
milkings_ordered <- milkings %>%
  left_join(devices_location_bridge, by = c("farm_key", "device_key", "location_key"))

# Convert milking_visit_process_start_time_wall and milking_visit_process_end_time_wall to datetime
milkings_ordered <- milkings_ordered %>%
  mutate(
    milking_visit_process_start_time_wall = as.POSIXct(milking_visit_process_start_time_wall, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    milking_visit_process_end_time_wall = as.POSIXct(milking_visit_process_end_time_wall, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )

# Assuming milkings_ordered is the dataframe you're working with

# Step 1: Subset data for location-1 of farm-2
subset_data <- milkings_ordered %>%
  filter(location_key == "location-1", farm_key == "farm-2")

# Step 2: Identify unique pairs of cows (if there are at least 2 unique cows)
unique_cows <- subset_data %>%
  distinct(animal_key)

unique_cow_pairs <- combn(unique_cows$animal_key, 2, simplify = TRUE) 
unique_cow_pairs <- unique_cow_pairs%>%
  rbind(matrix(NA, nrow = 5, ncol = ncol(unique_cow_pairs)))

for (i in 1:ncol(unique_cow_pairs)){

  cow1 <- unique_cow_pairs[1,i]
  cow2 <- unique_cow_pairs[2,i]
  
  # Filter data for the current cow pair
  cow_pair_data <- subset_data %>%
    filter(animal_key %in% c(cow1, cow2)) %>%
    arrange(milking_visit_process_start_time_wall) %>%
    select(
      farm_key,
      location_key,
      device_key,
      animal_key,
      milking_visit_process_start_time_wall,
      milking_visit_process_end_time_wall
    )%>%
    dplyr::mutate(
      date = as.Date(milking_visit_process_start_time_wall),
    )
  
  # Identify days when both cows visited the machine
  visit_data <- cow_pair_data %>%
    group_by(date) %>%
    filter(n_distinct(animal_key) == 2) %>%
    summarise(
      visits_cow1 = sum(animal_key == cow1),
      visits_cow2 = sum(animal_key == cow2)
    )
  
  # Store the results in the unique_cow_pairs matrix
  # Total days that both cows visited
  unique_cow_pairs[3,i] <- nrow(visit_data)
  # average times cow1 visited the milking machine per day
  unique_cow_pairs[4,i] <- mean(visit_data$visits_cow1, na.rm = TRUE)
  # average times cow2 visited the milking machine per day
  unique_cow_pairs[5,i] <- mean(visit_data$visits_cow2, na.rm = TRUE)
  
  # filter out cows that visit machine too many times, and cows that didn't share enough days together
  if (max(as.numeric(unique_cow_pairs[4,i]),as.numeric(unique_cow_pairs[5,i]))>6|as.numeric(unique_cow_pairs[3,i])<30) {
    next  
  }
  
  x<-min(as.numeric(unique_cow_pairs[4,52]),as.numeric(unique_cow_pairs[5,52]))
  
  # Extract numeric part from animal_key
  cow_pair_data <- cow_pair_data %>%
    mutate(animal_key_numeric = as.numeric(sub("cow-", "", animal_key)))
  
  # Identify consecutive uninterupted visits for each cow
  cow_pair_data <- cow_pair_data %>%
    group_by(
      consecutive_visit_group = cumsum(c(TRUE, diff(animal_key_numeric) != 0)),
      farm_key,
      location_key,
      animal_key
    ) %>%
    summarise(
      start_time = min(milking_visit_process_start_time_wall),
      end_time = max(milking_visit_process_end_time_wall)
    )%>%
    dplyr::mutate(
      date = as.Date(start_time),
    )
  
  # could be improved but works fine for now
  cow_pair_data$time_diff <- rep(NA, nrow(cow_pair_data))
  for (j in 2:nrow(cow_pair_data)){
    cow_pair_data$time_diff[j]<-abs(difftime(cow_pair_data$start_time[j], cow_pair_data$end_time[j-1], units = "secs"))
  }
  
  # Assuming cow_pair_data is the dataframe you're working with
  
  # Filter rows with time_diff below 40,000
  filtered_data <- cow_pair_data %>%
    filter(time_diff < 40000)
  
  # Group by cow pair and date
  grouped_data <- filtered_data %>%
    group_by(date) %>%
    filter(n() >= 2) %>%
    summarise(
      average_time_diff = min(time_diff, na.rm = TRUE)
    )
  unique_cow_pairs[6,i] <- nrow(grouped_data)
  unique_cow_pairs[7,i] <- mean(grouped_data$average_time_diff, na.rm = TRUE)
  
}

# Find the threshold for the 5% lowest values
threshold <- as.numeric(quantile(as.numeric(unique_cow_pairs[7,]), probs = 0.05, na.rm = TRUE))

unique_cow_pairs <- rbind(unique_cow_pairs, rep(NA, ncol(unique_cow_pairs)))

# Define whether pair is buddy or not
for(i in 1:ncol(unique_cow_pairs))
{
  if(!is.na(unique_cow_pairs[7, i]) & as.numeric(unique_cow_pairs[7,i])<threshold)
  {
   unique_cow_pairs[8,i] <- 1 
  }
  else{
    unique_cow_pairs[8,i] <- 0
  }
}

# Maak dataframe met alleen buddies
buddies <- unique_cow_pairs[, unique_cow_pairs[8, ] == 1]

# Find relocations in current farm and location
cow_relocations_farm2_location1 <- cow_relocations[cow_relocations$farm_key == "farm-2" & cow_relocations$old_location == "location-1",]

remaining_cows <- data.frame()

# Retrieve cows whose buddies were relocated
for (i in 1:nrow(cow_relocations_farm2_location1)){
  relocated_cow <- cow_relocations_farm2_location1[i, 2]
  
  buddies_row1 <- which(best_buddies[1, ] == relocated_cow)
  for (j in buddies_row1){
    remaining_cows <- rbind(remaining_cows, c(best_buddies[2, j], cow_relocations_farm2_location1[i, 5]))
  }
  buddies_row2 <- which(best_buddies[2, ] == relocated_cow)
  for (j in buddies_row2){
    remaining_cows <- rbind(remaining_cows, c(best_buddies[1, j], cow_relocations_farm2_location1[i, 5]))
  }
}

index <- 1

# plot the milk yield (sum of every two days) of cows whose buddies were relocated
for (remaining_cow in remaining_cows[, 1]){
  current_subset <- subset_data[subset_data$animal_key == remaining_cow, ]
  current_subset <- current_subset[order(current_subset$milking_visit_process_start_time_wall),]
  
  current_subset <- current_subset %>%
    mutate(day = as.Date(milking_visit_process_start_time_wall))
  
  daily_milk_yield <- current_subset %>%
    group_by(day) %>%
    summarise(total_milk_yield = sum(milk_yield_kg, na.rm = TRUE))
  
  combined_milk <- daily_milk_yield$total_milk_yield[1:308][seq(1, length(daily_milk_yield$total_milk_yield[1:308]), by = 2)] +
                   daily_milk_yield$total_milk_yield[1:308][seq(2, length(daily_milk_yield$total_milk_yield[1:308]), by = 2)]
  
  combined_milk_df <- data.frame(combined_yield = combined_milk)
  
  buddy_relocation_index <- which(daily_milk_yield$day == as.Date(remaining_cows[index, 2]))
  
  if (length(buddy_relocation_index) > 0) {
    p <- ggplot(combined_milk_df, aes(x = 1:nrow(combined_milk_df), y = combined_milk_df$combined_yield)) +
      geom_line() +
      geom_vline(xintercept = buddy_relocation_index/2, linetype = "dashed", color = "red") +
      labs(x = "Day", y = "First difference milk yield (kg)", title = paste("Daily Milk Yield for Cow ID:", remaining_cow)) +
      theme_minimal()
    
    print(p)
  }
  
  index <- index + 1 
  
  
}

# VANAF HIER NIET ZO BELANGRIJK

visiting_data <- subset_data %>%
  mutate(date = as.Date(milking_visit_process_start_time_wall))

# Group by cow ID and count unique dates
days_visited <- visiting_data %>%
  group_by(animal_key) %>%
  summarise(visited_days = n_distinct(date))

visits_per_day <- c()

na_data <- data.frame(matrix(NA, nrow = 2, ncol = 1252))
colnames(na_data) <- paste("V", seq_along(na_data), sep = "")

buddies <- rbind(buddies, na_data)

for (cow in unique_cows){
  n_buddies <- sum(check_buddies[, 1] == cow) + sum(check_buddies[, 2] == cow)
  n_days <- days_visited$visited_days[days_visited$animal_key == cow]
  n_visits <- nrow(subset_data[subset_data$animal_key == cow,])
  visits_per_day <- c(visits_per_day, n_visits/n_days)
  print(paste(cow, " visited total number of ",  n_visits, " times on ", n_days ," days and has ", n_buddies, " friends, and visits ", n_visits/n_days, " times per day"))
}

subset_data <- subset_data %>%
  mutate(milking_date = as.Date(milking_visit_process_start_time_wall))

# Group by cow ID and milking date, then calculate the number of milking visits per day
daily_visits <- subset_data %>%
  group_by(animal_key, milking_date) %>%
  summarise(num_visits = n())

# Calculate the average number of visits per day per cow
average_visits_per_day <- daily_visits %>%
  group_by(animal_key) %>%
  summarise(average_visits = mean(num_visits))

ggplot(average_visits_per_day, aes(x = average_visits)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(x = "Average Visits per Cow", y = "Frequency", title = "Distribution of Average Visits per Cow") +
  theme_minimal()

best_buddies <- buddies[, as.numeric(buddies[4, ]) < 6 & as.numeric(buddies[5, ]) < 6]

