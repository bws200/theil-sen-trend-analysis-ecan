### A script to use the original excel sheet Teresa sent to the Ministry for the Environment

# Load data and clean the names
df_mfE_data <- readxl::read_excel(path = "MfE Daily PM 2014 to 2023.xlsx")

# Sites and remove spaces
site <- df_mfE_data[2,] |> 
  str_replace_all(fixed(" "), "")

# Parameters and remove spaces
parameter <- df_mfE_data[3,] |> 
  str_replace_all(fixed(" "), "")

# Instruments and remove spaces
instrument <- df_mfE_data[4,] |> 
  str_replace_all(fixed(" "), "")

# Combine names to make a super name with all descriptors
combine_names <- paste(site, parameter, instrument, sep = "_")

# Rename first potential column name
combine_names[1] <- "date"

# Apply new column names
names(df_mfE_data) <- combine_names

# Select only relevant rows removing header data
df_mfE_data_clean <- df_mfE_data[5:nrow(df_mfE_data),]

# Remove non-STP columns which were included in mfE data export. Found indices manually
df_mfE_data_clean <- df_mfE_data_clean[,-c(47,51,64,68)]

# Data wrangling to get the data into a long format from wide
df_mfE_data_clean_long <-df_mfE_data_clean |> 
  pivot_longer(cols = !date, names_sep = "_",names_to = c(".value", "pollutant", "instrument")) |> 
  pivot_longer(cols = !c(date, pollutant, instrument), names_to = "site") |> 
  mutate(
    # Force date conversion
    date = openxlsx::convertToDate(date),
    # Force numeric conversion
    value = as.numeric(value)
    ) |> 
  # Date range filtering
  filter(date >= "2016-01-01") |> 
  filter(date <= "2023-12-31")

# Theil Sen trend analysis for just Rangiora PM2.5 at this stage
df_mfE_data_clean_long |> 
  filter(site == "Rangiora") |> 
  filter(pollutant == "PM2.5") |> 
  # Select relevant instruments
  filter(instrument %in% c("Amix", "Fidas200E", "TEOMFDMS", "ES642")) |>
  # Select only relevant columns
  select(c(date, instrument, value)) |> 
  openair::TheilSen(
    type = "instrument",
    pollutant = "value",
    deseason = TRUE,
    date.format = "%Y",
    data.thresh = 75,
    # fontsize = 40
  )











### Single Rangiora excel file done for speed
### 
### This file was manually modified to just Rangiora and simple structure and column names to make it easy and fast to work with
### 

# Read excel file
df <- readxl::read_excel(path = "rangiora.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) |> 
  pivot_longer(
    cols = !date,
    names_to = c(".value", "Instrument"),
    names_sep = "_") |>
  mutate(date = as.Date(date))

df |> 
  filter(Instrument == "TEOM") |> 
  openair::summaryPlot(
    df,
    pollutant = "PM2.5"
    )

df |> 
  filter(Instrument == "Fidas") |> 
  openair::summaryPlot(
    df,
    pollutant = "PM10"
  )


df |> 
  filter(date >= "2016-01-01") |> 
  openair::TheilSen(
    type = "Instrument",
    pollutant = "PM2.5",
    deseason = TRUE,
    date.format = "%Y",
    # type = "site",
    data.thresh = 75,
    # fontsize = 40
  )
