# Load in the tidyverse, raster, and sf packages
library(tidyverse)
library(raster)
library(sf)

# Read the climate data from an rds file
climate <- read_rds("climate_raster.rds")

# Have a look at the variables in the climate data
colnames(climate)

# Convert to SpatialPixelDataFrame for plotting
climate_df <- mutate(
  .data = climate, 
  rasters = map(
    .x = rasters, 
    ~ as_tibble(as(.x, "SpatialPixelsDataFrame")))) %>%
  unnest(cols = c(rasters))



library(ggthemes)

# Filter the data to plot
ggp_temperature <- climate_df %>%
  filter(decade %in% c(1970, 2010)) %>%
  # Create the plot
  ggplot(aes(x = x, y = y)) + geom_tile(aes(fill = minimum.temperature)) +
  # Style the plot with options ideal for maps
  theme_map() + coord_equal() + 
  facet_grid(~ decade) + scale_fill_distiller(palette = "Spectral") + 
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs(title = "Minimum of Average Monthly Temperature (Celsius)", caption = 'Source: MetOffice UK')

ggp_temperature

library(rgbif)
source("search.R")

# Call the API to get the occurrence records of this species
gbif_response <- occ_search(
  scientificName = "Loxia scotica", country = "GB",
  hasCoordinate = TRUE, hasGeospatialIssue = FALSE, limit = 2000)

# Inspect the class and names of gbif_response
class(gbif_response)
names(gbif_response)

# Show a sample of the data, ignoring the metadata
head(gbif_response[["data"]])


library(dplyr)     # Load dplyr for mutate function
library(lubridate) # Load lubridate for year function

birds_dated <- mutate(
  .data = gbif_response$data,
  # Create a new column specifying the decade of observation
  decade = ymd_hms(eventDate) %>% round_date("10y") %>% year()
)


library(stringr)     # Load stringr for string manipulation functions

birds_cleaned <- birds_dated %>%
  filter(
    issues == "" &
      str_detect(license, "http://creativecommons.org/") &
      # No records before 1970s decade or after 2010s decade
      decade >= 1970 & decade <= 2010
  ) %>%
  transmute(decade = decade, x = decimalLongitude, y = decimalLatitude) %>%
  arrange(decade)



# "Nest" the bird data
birds_nested <- birds_cleaned %>%
  group_by(decade) %>% 
  nest(.key = "presences")

head(birds_nested)

# Calculate the total number of records per decade
birds_counted <- dplyr::mutate(birds_nested,
                               n = purrr::map_dbl(.x = presences, .f = nrow))


head(birds_counted)




# Define geographical projections
proj_latlon <- st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj_ukgrid <- st_crs("+init=epsg:27700")

# Convert records to spatial points and project them


birds_presences <- dplyr::mutate(birds_counted,
                                 presences = purrr::map(presences, ~ .x %>%
                                                          st_as_sf(coords = c("x", "y"), crs = proj_latlon) %>%
                                                          st_transform(crs = proj_ukgrid)))





  # Combine the bird data and the climate data in one data frame
  birds_climate <- full_join(birds_presences, climate, by = "decade")

presence_data <- map2_df(
  .x = birds_climate[["rasters"]],
  .y = birds_climate[["presences"]],
  # extract the raster values at presence locations
  ~ raster::extract(x = .x, y = .y) %>%
    as_tibble() %>% 
    mutate(observation = "presence"))




  # Define helper function for creating pseudo-absence data
  create_pseudo_absences <- function(rasters, n, ...) {
    set.seed(12345)
    sampleRandom(rasters, size = n * 5, sp = TRUE) %>% 
      raster::extract(rasters, .) %>% as_tibble() %>%
      mutate(observation = "pseudo_absence")
  }

# Create pseudo-absence proportional to the total number of records per decade
pseudo_absence_data <- pmap_df(.l = birds_climate, .f = create_pseudo_absences)

# Combine the two datasets
model_data <- bind_rows(presence_data, pseudo_absence_data) %>%
  mutate(observation = factor(observation)) %>% na.omit()




# Load caret and set a reproducible seed
library(caret)
set.seed(12345)

# Create a tuning grid with sets of hyperparameters to try
tuneGrid <- expand.grid(alpha = c(0, 0.5, 1), lambda = c(.003, .01, .03, .06))

# Create settings for model training
trControl <- trainControl(method = 'repeatedcv', number = 5, repeats = 1,
                          classProbs = TRUE, verboseIter = FALSE, summaryFunction = twoClassSummary)

# Fit a statistical model to the data and plot
model_fit <- train(
  observation ~ ., data = model_data,
  method = "glmnet", family = "binomial", metric = "ROC",
  tuneGrid = tuneGrid, trControl = trControl)

plot(model_fit)

  # Use our model to make a prediction
  climate_df$prediction <- predict(
    object = model_fit,
    newdata = climate_df,
    type = "prob")[["presence"]]

head(climate_df)

  # Load packages gganimate and viridis
  library(viridis)

library(ggplot2)

# Define your plot
ggp_changemap <- ggplot(data = climate_df, aes(x = x, y = y, fill = prediction)) +
  geom_tile() +
  coord_equal() +  # Ensure aspect ratio is preserved
  scale_fill_viridis(option = "A") +
  theme(legend.position = "bottom") +
  facet_grid(~ decade) +
  labs(title = 'Habitat Suitability', subtitle = 'by decade',
       caption = 'Source:\nGBIF data and\nMetOffice UK climate data',
       fill = 'Habitat Suitability [0 low - high 1]')

# Print the plot
print(ggp_changemap)
