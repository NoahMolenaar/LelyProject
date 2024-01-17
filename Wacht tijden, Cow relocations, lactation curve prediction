install.packages("dplyr")
install.packages("minpack.lm")
install.packages("ggplot2")
install.packages("randomForest")
library("dplyr")
library(minpack.lm)
library(ggplot2)
library(randomForest)

milk <- read.csv("/Volumes/LaCie/Econometrie Master/Blok 3/Lely case study/Data/milkings.csv")
devices <- read.csv("/Volumes/LaCie/Econometrie Master/Blok 3/Lely case study/Data/devices.csv")
devices_location <- read.csv("/Volumes/LaCie/Econometrie Master/Blok 3/Lely case study/Data/devices_location_bridge.csv")
locations <- read.csv("/Volumes/LaCie/Econometrie Master/Blok 3/Lely case study/Data/locations.csv")

#unique farms
farms <- unique(milk$farm_key)

all_average_waittimes <- data.frame()

# find average wait times per farm, location, device
for (farm in farms){
  current_farm <- milk[milk$farm_key == farm, ]
  locations <- unique(current_farm$location_key)
  for (location in locations){
    current_farm_location <- current_farm[current_farm$location_key == location, ]
    devices <- unique(current_farm_location$device_key)
    for (device in devices[devices != ""]){
      # extract current farm location device
      current_farm_location_device <- current_farm_location[current_farm_location$device_key == device, ]
      # convert starttime column into POSIXct format
      current_farm_location_device$milking_visit_process_start_time_wall <- as.POSIXct(current_farm_location_device$milking_visit_process_start_time_wall, format = "%Y-%m-%d %H:%M:%S")
      # order by startdate
      ordered_current_farm_location_device <- current_farm_location_device[order(current_farm_location_device$milking_visit_process_start_time_wall), ]
      
      missing_values_indices_start_time <- which(is.na(ordered_current_farm_location_device$milking_visit_process_start_time_wall))
      missing_values_indices_end_time <- which(is.na(ordered_current_farm_location_device$milking_visit_process_end_time_wall))
      
      nrows <- nrow(ordered_current_farm_location_device)
      
      current_total_waittime <- 0
      
      for (i in 1:(nrows-1)){
        waittime_secs <- as.numeric(difftime(ordered_current_farm_location_device[i+1, ]$milking_visit_process_start_time_wall, ordered_current_farm_location_device[i, ]$milking_visit_process_end_time_wall, units = "secs"))
        if (is.na(waittime_secs)){
          next
        }
        current_total_waittime <- current_total_waittime + waittime_secs
      }
      
      current_average_waittime <- current_total_waittime / nrows
  
      all_average_waittimes <- rbind(all_average_waittimes, c(farm, location, device, current_average_waittime))
    
    }
  }
}

# Begin relocations code

#sorteer op farm dan koe dan tijd
sorted_data <- milk[order(milk$farm_key, milk$animal_key, milk$milking_visit_process_start_time_wall), ]

cow_relocations <- data.frame()

nrows_sorted_data <- nrow(sorted_data)

# Als we nog op dezelfde farm zijn en het over dezelfde koe hebben, als de locatie verandert weten we dat er 
# een herplaatsing was
for (j in 1:(nrows_sorted_data-1)){
  if (sorted_data[j, ]$farm_key == sorted_data[j+1, ]$farm_key & sorted_data[j, ]$animal_key == sorted_data[j+1, ]$animal_key & sorted_data[j, ]$location_key != sorted_data[j+1, ]$location_key){
    date_time1 <- as.POSIXct(sorted_data[j, ]$milking_visit_process_end_time_wall, format="%Y-%m-%d %H:%M:%S")
    date_time2 <- as.POSIXct(sorted_data[j + 1, ]$milking_visit_process_start_time_wall, format="%Y-%m-%d %H:%M:%S")
    
    # we bepalen tijdstip van herplaatsing nu al midpunt tussen einde van laatste melking vorige locatie
    # en begin van eerste melking nieuwe locatie
    midpoint <- format(date_time1 + (date_time2 - date_time1)/2, "%Y-%m-%d %H:%M:%S")
    cow_relocations <- rbind(cow_relocations, c(sorted_data[j, ]$farm_key, sorted_data[j, ]$animal_key, sorted_data[j, ]$location_key, sorted_data[j+1, ]$location_key, midpoint))
  }
}

# kolommen naam geven
names(cow_relocations) <- c("farm_key", "animal_key", "old_location", "new_location", "time_of_relocation")

# Einde relocations code

unique_lactations <- unique(milk$lactation_number)
unique_days_in_lactation <- unique(milk$days_in_lactation)

hist(milk$days_in_lactation,
     main = "Distribution of lactation days",
     xlab = "days in lactation",
     ylab = "Frequency",
     col = "lightblue",
     border = "black",
     xlim = c(0, 1073),
     breaks = 1073)  # You can adjust the number of breaks to control the bin width

hist(milk$lactation_number,
     main = "Distribution of lactation number",
     xlab = "lactation number",
     ylab = "Frequency",
     col = "lightblue",
     border = "black",
     xlim = c(0, 9),
     breaks = 10)

par(mfrow = c(2, 3))

remaining_data <- data.frame()

x <- c(0:350)

results_list <- list()


for (j in 1:6) {
  lactation_averages_train <- c()
  lactation_averages_test <- c()
  for (i in 0:350) {
    filtered_data <- milk %>%
      filter(days_in_lactation == i & lactation_number == j)
    # Sample 80% of the filtered data
    sampled_data <- filtered_data %>%
      sample_frac(0.8, replace = FALSE)  # Adjust the fraction as needed
    # Store the remaining 20%
    remaining_data <- setdiff(filtered_data, sampled_data)
    # Append mean values to the vectors
    lactation_averages_train <- c(lactation_averages_train, mean(sampled_data$milk_yield_kg, na.rm = TRUE))
    lactation_averages_test <- c(lactation_averages_test, mean(remaining_data$milk_yield_kg, na.rm = TRUE))
  }
  rf.fit <- randomForest(lactation_averages_train ~ x, ntree = 1000)
  predictions <- predict(rf.fit, data.frame(x = x))
  
  actual_data <- data.frame(
    Days = 0:350,
    Averages = lactation_averages_test
  )
  
  predicted_data <- data.frame(
    Days = 0:350,
    Averages = predictions
  )
  
  # Combine actual and predicted data into a single dataframe
  plot_data <- bind_rows(
    mutate(actual_data, Type = "Actual"),
    mutate(predicted_data, Type = "Predicted")
  )
  
  # Add the plot data to the results list
  results_list[[j]] <- plot_data
}

# Combine all plots in a grid using facet_wrap
result_plot <- ggplot() +
  geom_line(data = results_list[[1]], aes(x = Days, y = Averages, color = Type), size = 1.5, alpha = 0.7) +
  labs(title = "Actual vs Predicted Values",
       x = "Days in Lactation",
       y = "Average Value") +
  theme_minimal() +
  facet_wrap(~Type, ncol = 1)  # Change ncol to control the number of columns in the grid

# Print the combined plot
print(result_plot)







for (j in 1:6) {
  for (i in 0:350) {
    filtered_data <- milk %>%
      filter(days_in_lactation == i & lactation_number == j)
    # Sample 80% of the filtered data
    sampled_data <- filtered_data %>%
      sample_frac(0.8, replace = FALSE)  # Adjust the fraction as needed
    # Store the remaining 20%
    remaining_data <- setdiff(filtered_data, sampled_data)
    # Append mean values to the vectors
    lactation_averages_train <- c(lactation_averages_train, mean(sampled_data$milk_yield_kg, na.rm = TRUE))
    lactation_averages_test <- c(lactation_averages_test, mean(remaining_data$milk_yield_kg, na.rm = TRUE))
  }
  rf.fit <- randomForest(lactation_averages_train ~ x, ntree = 1000)
  predictions <- predict(rf.fit, x)
  
  actual_data <- data.frame(
    Days = 0:350,
    Averages = lactation_averages_test
  )
  
  predicted_data <- data.frame(
    Days = 0:350,
    Averages = predictions
  )
}


# Plot the actual data and the predicted values
ggplot() +
  geom_line(data = actual_data, aes(x = Days, y = Averages), color = "blue", linetype = "solid", size = 1.5, alpha = 0.7) +
  geom_line(data = predicted_data, aes(x = Days, y = Averages), color = "red", linetype = "solid", size = 1.5, alpha = 0.7) +
  labs(title = "Actual vs Predicted Values",
       x = "Days in Lactation",
       y = "Average Value") +
  theme_minimal()





