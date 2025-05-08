# BMA-RCodes



# DATA CLEANING

# ROAD ACCIDENT DATASET IN TASMANIA

```{r comment=NA}
# Read data and process
data_road <- read_excel("C:/Users/Admin/Downloads/bitre_fatal_crashes_feb2025.xlsx", sheet = 2, skip = 4)

# Create complete year-month counts for TAS
month_year_counts <- data_road %>%
  filter(Year >= 1989 & Year <= 2023) %>%
  filter(State == "TAS") %>%
  count(Year, Month, name = "Count") %>%
  complete(Year, Month = 1:12, fill = list(Count = 0)) %>%
  arrange(Year, Month)

kable(month_year_counts, caption = "Fatal Road Accident Count in Tasmania", align = "c")

# Print the year range to confirm filtering worked correctly
cat("Year range in analysis:", min(month_year_counts$Year), "to", max(month_year_counts$Year), "\n")
cat("Total number of records used:", nrow(month_year_counts), "\n")
```

# Northern Dataset (91)

```{r comment=NA}
# Define reusable function for data processing
precip_data1 <- function(file_name) {
  # Read data - Fixed file path with proper formatting
  data1 <- read_csv(paste0("C:/Users/Admin/Downloads/thesis data/thesis data/Tasmanian Rainfall Data/91/", file_name, ".csv"), 
                   show_col_types = FALSE)
  
  # Filter data and calculate mean precipitation
  filtered_data1 <- data1 %>%
    filter(Year >= 1989 & Year <= 2023) %>%
    mutate(Month = as.integer(Month)) %>%
    select(`Station number`, Year, Month, `Monthly Precipitation Total (millimetres)`)
    
  mean_precip1 <- mean(filtered_data1$`Monthly Precipitation Total (millimetres)`, 
                      na.rm = TRUE)
    
  # Create complete dataset with all year-month combinations and fill missing values
  complete_data1 <- expand_grid(Year = 1989:2023, Month = 1:12) %>%
    arrange(Year, Month) %>%
    left_join(filtered_data1, by = c("Year", "Month")) %>%
    mutate(`Monthly Precipitation Total (millimetres)` = 
             coalesce(`Monthly Precipitation Total (millimetres)`, mean_precip1))
  
  return(complete_data1)
}

# Process each dataset
beaconsfield <- precip_data1("BEACONSFIELD (MINE & HERITAGE CENTRE)")
cape_grim <- precip_data1("CAPE GRIM (WOOLNORTH)")
connorville <- precip_data1("CONNORVILLE (LAKE RIVER)")
epping_forest <- precip_data1("EPPING FOREST (FORTON)") 
south_forest <- precip_data1("SOUTH FOREST (PLUMMERS ROAD)")
northdown <- precip_data1("NORTHDOWN (HAMLEY)")
hillwood <- precip_data1("HILLWOOD (HILLWOODVILLE)")
lilydale_postoffice <- precip_data1("LILYDALE POST OFFICE")
mawbanna <- precip_data1("MAWBANNA (MAWBANNA ROAD)")
launceston <- precip_data1("LAUNCESTON (KINGS MEADOWS)")
ringarooma <- precip_data1("RINGAROOMA (MAIN STREET)")
selbourne <- precip_data1("SELBOURNE (KIRNBRAE)")
yolla <- precip_data1("YOLLA (SEA VIEW)")
devonport <- precip_data1("DEVONPORT AIRPORT")
western_creek <- precip_data1("WESTERN CREEK (SOMER HILL)")
barrington <- precip_data1("BARRINGTON POST OFFICE")
weetah <- precip_data1("WEETAH (WEETAH ROAD)")
parkham <- precip_data1("PARKHAM (AVENUE ROAD)")
perth <- precip_data1("PERTH (MIDLAND HIGHWAY)")
east_sassafras <- precip_data1("EAST SASSAFRAS (ELPHIN GROVE)")
tomahawk <- precip_data1("TOMAHAWK (CARISBROOKE)")
forthside <- precip_data1("FORTHSIDE RESEARCH STATION")
                                                                                          
# Combine data from all locations into a wide format table
combined_data1 <- beaconsfield %>%
  select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
  rename(`Beaconsfield (Mine & Heritage Centre)` = `Monthly Precipitation Total (millimetres)`) %>%
  left_join(
    cape_grim %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Cape Grim (Woolnorth)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    connorville %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Connorville (Lake River)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    epping_forest %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Epping Forest (Forton)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    south_forest %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`South Forest (Plummers Road)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    northdown %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Northdown (Hamley)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    hillwood %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Hillwood (Hillwoodville)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    lilydale_postoffice %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Lilydale Post Office` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    mawbanna %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Mawbanna (Mawbanna Road)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    launceston %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Launceston (Kings Meadows)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    ringarooma %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Ringarooma (Main Street)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    selbourne %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Selbourne (Kirnbrae)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    yolla %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Yolla (Sea View)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    devonport %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Devonport Airport` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    western_creek %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Western Creek (Somer Hill)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    barrington %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Barrington Post Office` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    weetah %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Weetah (Weetah Road)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    parkham %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Parkham (Avenue Road)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    perth %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Perth (Midland Highway)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    east_sassafras %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`East Sassafras (Elphin Grove)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    tomahawk %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Tomahawk (Carisbrooke)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    forthside %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Forthside Research Station` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  mutate(
    Average = rowMeans(
      select(., -Year, -Month),
      na.rm = TRUE
    )
  )

# Round all precipitation values to one decimal place
combined_data1 <- combined_data1 %>%
  mutate(across(-c(Year, Month), ~round(., 1)))

# Create a formatted table
table1 <- kable(combined_data1, 
                caption = "Average Monthly Precipitation Total (millimetres) in Northern",
                align = "c")

# Display the table
table1

# Export to CSV
write_csv(combined_data1, "northern_precipitation.csv")
```

# East Coast Dataset (92)

```{r comment=NA}
# Define reusable function for data processing
precip_data2 <- function(file_name) {
  # Read data - Fixed file path with proper formatting
  data2 <- read_csv(paste0("C:/Users/Admin/Downloads/thesis data/thesis data/Tasmanian Rainfall Data/92/", file_name, ".csv"), 
                   show_col_types = FALSE)
  
  # Filter data and calculate mean precipitation
  filtered_data2 <- data2 %>%
    filter(Year >= 1989 & Year <= 2023) %>%
    mutate(Month = as.integer(Month)) %>%
    select(`Station number`, Year, Month, `Monthly Precipitation Total (millimetres)`)
    
  mean_precip2 <- mean(filtered_data2$`Monthly Precipitation Total (millimetres)`, 
                      na.rm = TRUE)
    
  # Create complete dataset with all year-month combinations and fill missing values
  complete_data2 <- expand_grid(Year = 1989:2023, Month = 1:12) %>%
    arrange(Year, Month) %>%
    left_join(filtered_data2, by = c("Year", "Month")) %>%
    mutate(`Monthly Precipitation Total (millimetres)` = 
             coalesce(`Monthly Precipitation Total (millimetres)`, mean_precip2))
  
  return(complete_data2)
}

# Process each dataset
coles_bay <- precip_data2("COLES BAY (FREYCINET)")
larapuna <- precip_data2("LARAPUNA (EDDYSTONE POINT)")
lewis_hill <- precip_data2("LEWIS HILL (ST PAULS RIVER)")
nugent <- precip_data2("NUGENT (TWILIGHT VALLEY)")
orford_south <- precip_data2("ORFORD SOUTH")
ormley <- precip_data2("ORMLEY")
pioneer <- precip_data2("PIONEER (MAIN ROAD)")
pyengana <- precip_data2("PYENGANA (FOREST LODGE ROAD)")
stonehouse <- precip_data2("STONEHOUSE")

# Combine data from all locations into a wide format table
combined_data2 <- coles_bay %>%
  select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
  rename(`Coles Bay (Freycinet)` = `Monthly Precipitation Total (millimetres)`) %>%
  left_join(
    larapuna %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Larapuna (Eddystone Point)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    lewis_hill %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Lewis Hill (St Pauls River)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    nugent %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Nugent (Twilight Valley)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    orford_south %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Orford South` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    ormley %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Ormley` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    pioneer %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Pioneer (Main Road)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    pyengana %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Pyengana (Forest Lodge Road)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    stonehouse %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Stonehouse` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  mutate(
    Average = rowMeans(
      select(., -Year, -Month),
      na.rm = TRUE
    )
  )

# Round all precipitation values to one decimal place
combined_data2 <- combined_data2 %>%
  mutate(across(-c(Year, Month), ~round(., 1)))

# Create a formatted table
table2 <- kable(combined_data2, 
                         caption = "Average Monthly Precipitation Total (millimetres) in East Coast",
                         align = "c")

# Display the table
table2

# Export to CSV
write_csv(combined_data2, "east_coast_precipitation.csv")
```

# Midlands Dataset (93)

```{r comment=NA}
precip_data3 <- function(file_name) {
  data3 <- read_csv(paste0("C:/Users/Admin/Downloads/thesis data/thesis data/Tasmanian Rainfall Data/93/", file_name, ".csv"), 
                   show_col_types = FALSE)
  
  # Filter data and calculate mean precipitation
  filtered_data3 <- data3 %>%
    filter(Year >= 1989 & Year <= 2023) %>%
    mutate(Month = as.integer(Month)) %>%
    select(`Station number`, Year, Month, `Monthly Precipitation Total (millimetres)`)
    
  mean_precip3 <- mean(filtered_data3$`Monthly Precipitation Total (millimetres)`, 
                      na.rm = TRUE)
  complete_data3 <- expand_grid(Year = 1989:2023, Month = 1:12) %>%
    arrange(Year, Month) %>%
    left_join(filtered_data3, by = c("Year", "Month")) %>%
    mutate(`Monthly Precipitation Total (millimetres)` = 
             coalesce(`Monthly Precipitation Total (millimetres)`, mean_precip3))
  
  return(complete_data3)
}

woodbury <- precip_data3("WOODBURY (WARRINGA)")
york_plains <- precip_data3("YORK PLAINS (HANDROYD)")
ross_villa <- precip_data3("ROSS (ROSE VILLA)")
rotherwood <- precip_data3("ROTHERWOOD")


combined_data3 <- woodbury %>%
  select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
  rename(`Woodbury (Warringa)` = `Monthly Precipitation Total (millimetres)`) %>%
  left_join(
    york_plains %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`York Plains (Handroyd)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    ross_villa %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Ross (Rose Villa)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    rotherwood %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Rotherwood` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  mutate(
    Average = rowMeans(
      select(., -Year, -Month),
      na.rm = TRUE
    )
  )

# Round all precipitation values to one decimal place
combined_data3 <- combined_data3 %>%
  mutate(across(-c(Year, Month), ~round(., 1)))

# Create a formatted table
table3 <- kable(combined_data3, 
                caption = "Average Monthly Precipitation Total (millimetres) in Midlands",
                align = "c")
table3

# Export to CSV
write_csv(combined_data3, "midlands_precipitation.csv")
```

# Southeast Dataset (94)

```{r comment=NA}
# Define reusable function for data processing
precip_data4 <- function(file_name) {
  data4 <- read_csv(paste0("C:/Users/Admin/Downloads/thesis data/thesis data/Tasmanian Rainfall Data/94/", file_name, ".csv"), 
                   show_col_types = FALSE)
  
  # Filter data and calculate mean precipitation
  filtered_data4 <- data4 %>%
    filter(Year >= 1989 & Year <= 2023) %>%
    mutate(Month = as.integer(Month)) %>%
    select(`Station number`, Year, Month, `Monthly Precipitation Total (millimetres)`)
    
  mean_precip4 <- mean(filtered_data4$`Monthly Precipitation Total (millimetres)`, 
                      na.rm = TRUE)
    
  # Create complete dataset with all year-month combinations and fill missing values
  complete_data4 <- expand_grid(Year = 1989:2023, Month = 1:12) %>%
    arrange(Year, Month) %>%
    left_join(filtered_data4, by = c("Year", "Month")) %>%
    mutate(`Monthly Precipitation Total (millimetres)` = 
             coalesce(`Monthly Precipitation Total (millimetres)`, mean_precip4))
  
  return(complete_data4)
}

# Process each dataset
apsley <- precip_data4("APSLEY (PARKI)")
hobart <- precip_data4("HOBART (ELLERSLIE ROAD)")
maatsuyker <- precip_data4("MAATSUYKER ISLAND LIGHTHOUSE")
sandford <- precip_data4("SANDFORD (MAYDENA)")
wattle_hill <- precip_data4("WATTLE HILL")  # Fixed variable name
taroona <- precip_data4("TAROONA (TAROONA CRESCENT)")
geeveston <- precip_data4("GEEVESTON (RIAWUNNA)")
margate <- precip_data4("MARGATE (SUNNYSIDE)")
orielton <- precip_data4("ORIELTON (EAST ORIELTON ROAD)")
longley <- precip_data4("LONGLEY (TELOPEA)")
taranna <- precip_data4("TARANNA (PARKS & WILDLIFE)")
tea_tree <- precip_data4("TEA TREE (THE POINT)")  # Fixed variable name
lucaston <- precip_data4("LUCASTON (BAKERS CREEK ROAD)")
richmond <- precip_data4("RICHMOND (STRATHAYR)")
glenorchy <- precip_data4("GLENORCHY (GLENORCHY RESERVOIR)")
snug_plains <- precip_data4("SNUG PLAINS (CATARACT FALLS)")  # Fixed variable name


# Combine data from all locations into a wide format table
combined_data4 <- apsley %>%
  select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
  rename(`Apsley (Parki)` = `Monthly Precipitation Total (millimetres)`) %>%
  left_join(
    hobart %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Hobart (Ellerslie Road)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    maatsuyker %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Maatsuyker Island Lighthouse` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    sandford %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Sandford (Maydena)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    wattle_hill %>%  # Fixed variable name
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Wattle Hill` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    taroona %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Taroona (Taroona Crescent)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    geeveston %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Geeveston (Riawunna)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    margate %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Margate (Sunnyside)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    orielton %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Orielton (East Orielton Road)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    longley %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Longley (Telopea)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    taranna %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Taranna (Parks & Wildlife)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    tea_tree %>%  # Fixed variable name
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Tea Tree (The Point)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    lucaston %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Lucaston (Bakers Creek Road)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    richmond %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Richmond (Strathayr)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    glenorchy %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Glenorchy (Glenorchy Reservoir)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    snug_plains %>%  # Fixed variable name
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Snug Plains (Cataract Falls)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  mutate(
    Average = rowMeans(
      select(., -Year, -Month),
      na.rm = TRUE
    )
  )

# Round all precipitation values to one decimal place
combined_data4 <- combined_data4 %>%
  mutate(across(-c(Year, Month), ~round(., 1)))

# Create a formatted table
table4 <- kable(combined_data4, 
                caption = "Average Monthly Precipitation Total (millimetres) in Southeast",
                align = "c")
table4

# Export to CSV
write_csv(combined_data4, "southeast_precipitation.csv")
```

# Derwent Valley Dataset (95)

```{r comment=NA}
precip_data5 <- function(file_name) {
  data5 <- read_csv(paste0("C:/Users/Admin/Downloads/thesis data/thesis data/Tasmanian Rainfall Data/95/", file_name, ".csv"), 
                   show_col_types = FALSE)
  
  # Filter data and calculate mean precipitation
  filtered_data5 <- data5 %>%
    filter(Year >= 1989 & Year <= 2023) %>%
    mutate(Month = as.integer(Month)) %>%
    select(`Station number`, Year, Month, `Monthly Precipitation Total (millimetres)`)
    
  mean_precip5 <- mean(filtered_data5$`Monthly Precipitation Total (millimetres)`, 
                      na.rm = TRUE)
  complete_data5 <- expand_grid(Year = 1989:2023, Month = 1:12) %>%
    arrange(Year, Month) %>%
    left_join(filtered_data5, by = c("Year", "Month")) %>%
    mutate(`Monthly Precipitation Total (millimetres)` = 
             coalesce(`Monthly Precipitation Total (millimetres)`, mean_precip5))
  
  return(complete_data5)
}

bothwell <- precip_data5("BOTHWELL (FRANKLIN STREET)")
bushy_park <- precip_data5("BUSHY PARK (BUSHY PARK ESTATES)")
ouse <- precip_data5("OUSE (MILLBROOK)")
osterly <- precip_data5("OSTERLEY")
ellendale <- precip_data5("ELLENDALE POST OFFICE")


combined_data5 <- bothwell %>%
  select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
  rename(`Bothwell (Franklin Street)` = `Monthly Precipitation Total (millimetres)`) %>%
  left_join(
    bushy_park %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Bushy Park (Bushy Park Estates)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    ouse %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Ouse (Millbrook)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    osterly %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Osterley` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    ellendale %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Ellendale Post Office` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  mutate(
    Average = rowMeans(
      select(., -Year, -Month),
      na.rm = TRUE
    )
  )

combined_data5 <- combined_data5 %>%
  mutate(across(-c(Year, Month), ~round(., 1)))


table5 <- kable(combined_data5, 
                caption = "Average Monthly Precipitation Total (millimetres) in Derwent Valley",
                align = "c")
table5

# Export to CSV
write_csv(combined_data5, "derwent_valley_precipitation.csv")
```

# Central Plateau Dataset (96)

```{r comment=NA}
precip_data6 <- function(file_name) {
  data6 <- read_csv(paste0("C:/Users/Admin/Downloads/thesis data/thesis data/Tasmanian Rainfall Data/96/",file_name, ".csv"), 
                   show_col_types = FALSE)
  
  # Filter data and calculate mean precipitation
  filtered_data6 <- data6 %>%
    filter(Year >= 1989 & Year <= 2023) %>%
    mutate(Month = as.integer(Month)) %>%
    select(`Station number`, Year, Month, `Monthly Precipitation Total (millimetres)`)
    
  mean_precip6 <- mean(filtered_data6$`Monthly Precipitation Total (millimetres)`, 
                      na.rm = TRUE)
  complete_data6 <- expand_grid(Year = 1989:2023, Month = 1:12) %>%
    arrange(Year, Month) %>%
    left_join(filtered_data6, by = c("Year", "Month")) %>%
    mutate(`Monthly Precipitation Total (millimetres)` = 
             coalesce(`Monthly Precipitation Total (millimetres)`, mean_precip6))
  
  return(complete_data6)
}

bronte_heights <- precip_data6("BRONTE HEIGHTS")
butlers_gorge <- precip_data6("BUTLERS GORGE")
miena_dam <- precip_data6("MIENA DAM")
lake_st <- precip_data6("LAKE ST CLAIR NATIONAL PARK")


combined_data6 <- bronte_heights %>%
  select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
  rename(`Bronte Heights` = `Monthly Precipitation Total (millimetres)`) %>%
  left_join(
    butlers_gorge %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Butlers Gorge` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    miena_dam %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Miena Dam` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    lake_st %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Lake St Clair National Park` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  mutate(
    Average = rowMeans(
      select(., -Year, -Month),
      na.rm = TRUE
    )
  )

combined_data6 <- combined_data6 %>%
  mutate(across(-c(Year, Month), ~round(., 1)))


table6 <- kable(combined_data6, 
                caption = "Average Monthly Precipitation Total (millimetres) in Central Plateau",
                align = "c")
table6

# Export to CSV
write_csv(combined_data6, "central_plateau_precipitation.csv")
```

# West Coast (Mountain Region) Dataset (97)

```{r comment=NA}
precip_data7 <- function(file_name) {
  data7 <- read_csv(paste0("C:/Users/Admin/Downloads/thesis data/thesis data/Tasmanian Rainfall Data/97/", file_name, ".csv"), 
                   show_col_types = FALSE)
  
  # Filter data and calculate mean precipitation
  filtered_data7 <- data7 %>%
    filter(Year >= 1989 & Year <= 2023) %>%
    mutate(Month = as.integer(Month)) %>%
    select(`Station number`, Year, Month, `Monthly Precipitation Total (millimetres)`)
    
  mean_precip7 <- mean(filtered_data7$`Monthly Precipitation Total (millimetres)`, 
                      na.rm = TRUE)
  complete_data7 <- expand_grid(Year = 1989:2023, Month = 1:12) %>%
    arrange(Year, Month) %>%
    left_join(filtered_data7, by = c("Year", "Month")) %>%
    mutate(`Monthly Precipitation Total (millimetres)` = 
             coalesce(`Monthly Precipitation Total (millimetres)`, mean_precip7))
  
  return(complete_data7)
}

lake_margaret_dam <- precip_data7("LAKE MARGARET DAM")
lake_margaret_powerstation <- precip_data7("LAKE MARGARET POWER STATION")
arve_valley <- precip_data7("ARVE VALLEY (ARVE ROAD)")
zeehan <- precip_data7("ZEEHAN (WEST COAST PIONEERS MUSEUM)")
tahune <- precip_data7("TAHUNE RESERVE")


combined_data7 <- lake_margaret_dam %>%
  select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
  rename(`Lake Margaret Dam` = `Monthly Precipitation Total (millimetres)`) %>%
  left_join(
    lake_margaret_powerstation %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Lake Margaret Power Station` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    arve_valley %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Arve Valley (Arve Road)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    zeehan %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Zeehan (West Coast Pioneers Museum)` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  left_join(
    tahune %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Tahune Reserve` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  mutate(
    Average = rowMeans(
      select(., -Year, -Month),
      na.rm = TRUE
    )
  )

combined_data7 <- combined_data7 %>%
  mutate(across(-c(Year, Month), ~round(., 1)))


table7 <- kable(combined_data7, 
                caption = "Average Monthly Precipitation Total (millimetres) in West Coast (Mountain Region)",
                align = "c")
table7

# Export to CSV
write_csv(combined_data7, "west_coast (mountain region)_precipitation.csv")
```

# King Island Dataset (98)

```{r comment=NA}
precip_data8 <- function(file_name) {
  data8 <- read_csv(paste0("C:/Users/Admin/Downloads/thesis data/thesis data/Tasmanian Rainfall Data/98/", file_name, ".csv"), 
                   show_col_types = FALSE)
  
  # Filter data and calculate mean precipitation
  filtered_data8 <- data8 %>%
    filter(Year >= 1989 & Year <= 2023) %>%
    mutate(Month = as.integer(Month)) %>%
    select(`Station number`, Year, Month, `Monthly Precipitation Total (millimetres)`)
    
  mean_precip8 <- mean(filtered_data8$`Monthly Precipitation Total (millimetres)`, 
                      na.rm = TRUE)
  complete_data8 <- expand_grid(Year = 1989:2023, Month = 1:12) %>%
    arrange(Year, Month) %>%
    left_join(filtered_data8, by = c("Year", "Month")) %>%
    mutate(`Monthly Precipitation Total (millimetres)` = 
             coalesce(`Monthly Precipitation Total (millimetres)`, mean_precip8))
  
  return(complete_data8)
}

melbourne_bay <- precip_data8("CITY OF MELBOURNE BAY")
yarra_creek <- precip_data8("YARRA CREEK")

combined_data8 <- melbourne_bay %>%
  select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
  rename(`City Of Melbourne Bay` = `Monthly Precipitation Total (millimetres)`) %>%
  left_join(
    yarra_creek %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Yarra Creek` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  mutate(
    Average = rowMeans(
      select(., -Year, -Month),
      na.rm = TRUE
    )
  )

combined_data8 <- combined_data8 %>%
  mutate(across(-c(Year, Month), ~round(., 1)))


table8 <- kable(combined_data8, 
                caption = "Average Monthly Precipitation Total (millimetres) in King Island",
                align = "c")
table8

# Export to CSV
write_csv(combined_data8, "king_island_precipitation.csv")
```

# Flinders Island Dataset (99)

```{r comment=NA}
precip_data9 <- function(file_name) {
  data9 <- read_csv(paste0("C:/Users/Admin/Downloads/thesis data/thesis data/Tasmanian Rainfall Data/99/", file_name, ".csv"), 
                   show_col_types = FALSE)
  
  # Filter data and calculate mean precipitation
  filtered_data9 <- data9 %>%
    filter(Year >= 1989 & Year <= 2023) %>%
    mutate(Month = as.integer(Month)) %>%
    select(`Station number`, Year, Month, `Monthly Precipitation Total (millimetres)`)
    
  mean_precip9 <- mean(filtered_data9$`Monthly Precipitation Total (millimetres)`, 
                      na.rm = TRUE)
  complete_data9 <- expand_grid(Year = 1989:2023, Month = 1:12) %>%
    arrange(Year, Month) %>%
    left_join(filtered_data9, by = c("Year", "Month")) %>%
    mutate(`Monthly Precipitation Total (millimetres)` = 
             coalesce(`Monthly Precipitation Total (millimetres)`, mean_precip9))
  
  return(complete_data9)
}

deal_island <- precip_data9("DEAL ISLAND")
whitemark <- precip_data9("WHITEMARK POST OFFICE")



combined_data9 <- deal_island %>%
  select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
  rename(`Deal Island` = `Monthly Precipitation Total (millimetres)`) %>%
  left_join(
    whitemark %>%
      select(Year, Month, `Monthly Precipitation Total (millimetres)`) %>%
      rename(`Whitemark Post Office` = `Monthly Precipitation Total (millimetres)`),
    by = c("Year", "Month")
  ) %>%
  mutate(
    Average = rowMeans(
      select(., -Year, -Month),
      na.rm = TRUE
    )
  )

combined_data9 <- combined_data9 %>%
  mutate(across(-c(Year, Month), ~round(., 1)))


table9 <- kable(combined_data9, 
                caption = "Average Monthly Precipitation Total (millimetres) in Flinders Island",
                align = "c")
table9

# Export to CSV
write_csv(combined_data9, "flinders_island_precipitation.csv")
```

# RAINFALL DATASET IN TASMANIA

```{r comment=NA}
# Read all CSV files
northern <- read_csv("northern_precipitation.csv", show_col_types = FALSE)
east_coast <- read_csv("east_coast_precipitation.csv", show_col_types = FALSE)
midlands <- read_csv("midlands_precipitation.csv", show_col_types = FALSE)
southeast <- read_csv("southeast_precipitation.csv", show_col_types = FALSE)
derwent_valley <- read_csv("derwent_valley_precipitation.csv", show_col_types = FALSE)
central_plateau <- read_csv("central_plateau_precipitation.csv", show_col_types = FALSE)
west_coast <- read_csv("west_coast (mountain region)_precipitation.csv", show_col_types = FALSE)
king_island <- read_csv("king_island_precipitation.csv", show_col_types = FALSE)
flinders_island <- read_csv("flinders_island_precipitation.csv", show_col_types = FALSE)

# Select and rename columns for consistent merging
northern <- northern %>% select(Year, Month, Northern = Average)
east_coast <- east_coast %>% select(Year, Month, `East Coast` = Average)
midlands <- midlands %>% select(Year, Month, Midlands = Average)
southeast <- southeast %>% select(Year, Month, Southeast = Average)
derwent_valley <- derwent_valley %>% select(Year, Month, `Derwent Valley` = Average)
central_plateau <- central_plateau %>% select(Year, Month, `Central Plateau` = Average)
west_coast <- west_coast %>% select(Year, Month, `West Coast (Mountain Region)` = Average)
king_island <- king_island %>% select(Year, Month, `King Island` = Average)
flinders_island <- flinders_island %>% select(Year, Month, `Flinders Island` = Average)

# Join all data frames by Year and Month
precipitation_table <- northern %>%
  full_join(east_coast, by = c("Year", "Month")) %>%
  full_join(midlands, by = c("Year", "Month")) %>%
  full_join(southeast, by = c("Year", "Month")) %>%
  full_join(derwent_valley, by = c("Year", "Month")) %>%
  full_join(central_plateau, by = c("Year", "Month")) %>%
  full_join(west_coast, by = c("Year", "Month")) %>%
  full_join(king_island, by = c("Year", "Month")) %>%
  full_join(flinders_island, by = c("Year", "Month"))

# Calculate row averages
precipitation_table <- precipitation_table %>%
  mutate(Average = rowMeans(select(., -Year, -Month), na.rm = TRUE))

kable(precipitation_table,
      caption = "Average Monthly Precipitation Total (millimetres) in Tasmania", align = "c")

# Save as CSV
write_csv(precipitation_table, "tasmania_precipitation_table.csv")
```

# BAR CHART

```{r comment=NA}
# Read the CSV file
precipitation_data <- read.csv("tasmania_precipitation_table.csv")

# Convert Month numbers to month names
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Ensure Month is treated as a factor with correct ordering
precipitation_data$MonthName <- factor(month_names[precipitation_data$Month], 
                                      levels = month_names)

# Calculate average precipitation by month across all years (1989-2023)
monthly_avg <- precipitation_data %>%
  group_by(Month, MonthName) %>%
  summarize(Average_Precipitation = mean(Average, na.rm = TRUE), .groups = 'drop')

# Create the bar chart with value labels
ggplot(monthly_avg, aes(x = MonthName, y = Average_Precipitation)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "navy") +
  geom_text(aes(label = round(Average_Precipitation, 1)), 
            vjust = -0.5, # Position above the bar
            color = "black", 
            size = 3) +
  labs(title = "Bar Chart of the Average Monthly Precipitation Total (millimetres) in Tasmania",
       x = "Month",
       y = "Average Monthly Precipitation Total (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, face = "bold"))
```


# Bivariate Block Maxima Analysis

This document extends the univariate analysis to perform a bivariate block maxima analysis of road accident and rainfall data in Tasmania. The analysis follows a structured approach to understand the joint behavior of extreme events.

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo    = FALSE,   
  message = FALSE,
  warning = FALSE    
)
# Load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(extRemes)
library(evd)
library(copula)
library(VineCopula)
library(tseries)
library(trend)
library(Kendall)
library(moments)
library(fitdistrplus)
library(lmomco)
library(knitr)
library(kableExtra)
```

## 1. Data Preparation

```{r echo=FALSE}
# Road Accident Data
data_road <- read_excel("C:/Users/Admin/Downloads/bitre_fatal_crashes_feb2025.xlsx", sheet = 2, skip = 4)

# Create complete year-month counts for TAS
month_year_counts <- data_road %>%
  filter(Year >= 1989 & Year <= 2023) %>%
  filter(State == "TAS") %>%
  count(Year, Month, name = "Count") %>%
  complete(Year, Month = 1:12, fill = list(Count = 0)) %>%
  arrange(Year, Month)

# Extract yearly maximums for road accidents
yearly_max_road <- month_year_counts %>%
  group_by(Year) %>%
  summarize(Max = max(Count))

# Rainfall Data
precipitation_data <- read.csv("tasmania_precipitation_table.csv")

# Extract yearly maximums for rainfall
yearly_max_rain <- precipitation_data %>%
  group_by(Year) %>%
  summarise(Max = max(Average, na.rm = TRUE))
# Merge datasets to ensure they cover the same years
combined_data <- inner_join(yearly_max_road, yearly_max_rain, by = "Year") %>%
  rename(Road_Max = Max.x, Rain_Max = Max.y)
```

## Exploratory Data Analysis

```{r echo=FALSE}
# Calculate average fatal crashes by month across all years (1989-2023)
monthly_avg <- month_year_counts %>%
  group_by(Month) %>%
  summarize(Average_Crashes = mean(Count, na.rm = TRUE), .groups = 'drop')

# Convert Month numbers to month names and ensure correct ordering
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
monthly_avg$MonthName <- factor(month_names[monthly_avg$Month], 
                                levels = month_names)

# Create a color palette similar to the original
bar_colors <- "skyblue"
border_colors <- "navy"

# Create the bar chart with base R
par(mar = c(5, 5, 4, 2)) # Adjust margins: bottom, left, top, right
barplot_result <- barplot(monthly_avg$Average_Crashes, 
                         names.arg = monthly_avg$MonthName,
                         col = bar_colors,
                         border = border_colors,
                         main = "Average Monthly Fatal Road Accidents in Tasmania (1989-2023)",
                         xlab = "Month",
                         ylab = "Average Count",
                         ylim = c(0, max(monthly_avg$Average_Crashes) * 1.2), # Add space for labels
                         cex.main = 1.2,
                         cex.names = 0.9)

# Add value labels above each bar
text(x = barplot_result, 
     y = monthly_avg$Average_Crashes + max(monthly_avg$Average_Crashes) * 0.05, 
     labels = round(monthly_avg$Average_Crashes, 1),
     cex = 0.8)

# Add a box around the plot
box()
```

```{r echo=FALSE, comment=NA}
# Calculate average precipitation by month across all years
monthly_avg <- precipitation_data %>%
  group_by(Month) %>%
  summarize(Average_Precipitation = mean(Average, na.rm = TRUE), .groups = 'drop')

# Convert Month numbers to month names and ensure correct ordering
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
monthly_avg$MonthName <- factor(month_names[monthly_avg$Month], 
                                levels = month_names)

# Create a color palette similar to the original
bar_colors <- "palegreen3"
border_colors <- "darkolivegreen"

# Create the bar chart with base R
par(mar = c(5, 5, 4, 2)) # Adjust margins: bottom, left, top, right
barplot_result <- barplot(monthly_avg$Average_Precipitation, 
                         names.arg = monthly_avg$MonthName,
                         col = bar_colors,
                         border = border_colors,
                         main = "Average Monthly Precipitation Total in Tasmania (1989-2023)",
                         xlab = "Month",
                         ylab = "Average Precipitation (mm)",
                         ylim = c(0, max(monthly_avg$Average_Precipitation) * 1.2), # Add space for labels
                         cex.main = 1.2,
                         cex.names = 0.9)

# Add value labels above each bar
text(x = barplot_result, 
     y = monthly_avg$Average_Precipitation + max(monthly_avg$Average_Precipitation) * 0.05, 
     labels = round(monthly_avg$Average_Precipitation, 1),
     cex = 0.8)

# Add a box around the plot
box()
```

```{r}
# Time series plots
plot(combined_data$Year, combined_data$Road_Max, type = "o", pch = 16,
     xlab = "Year", ylab = "Maximum Fatal Road Accidents",
     main = "Annual Maximum Fatal Road Accidents in Tasmania (1989-2023)",
     ylim = c(0, max(combined_data$Road_Max, na.rm = TRUE)))

plot(combined_data$Year, combined_data$Rain_Max, type = "o", pch = 16,
     xlab = "Year", ylab = "Maximum Average Precipitation (mm)",
     main = "Annual Maximum Precipitation in Tasmania (1989-2023)",
     ylim = c(0, max(combined_data$Rain_Max, na.rm = TRUE)))

# Scatterplot with switched axes
plot(combined_data$Rain_Max, combined_data$Road_Max, 
     xlab = "Maximum Average Precipitation (mm)", 
     ylab = "Maximum Fatal Road Accidents",
     main = "Relationship between Annual Maxima",
     pch = 19, col = "blue",
     ylim = c(0, max(combined_data$Road_Max, na.rm = TRUE)))

# Add a linear regression line
model <- lm(Road_Max ~ Rain_Max, data = combined_data)
abline(model, col = "red", lwd = 2)

```

```{r echo=FALSE}
# Set up the plotting area for histograms and QQ plots
par(mfrow = c(2, 2))  # 2 rows, 2 columns

# Histogram for Rainfall Distribution
hist(precipitation_data$Average, 
     main = "Rainfall Distribution", 
     xlab = "Rainfall (mm)", 
     col = "lightgreen", 
     border = "darkgreen")

# Histogram for Accidents Distribution
hist(month_year_counts$Count, 
     main = "Accidents Distribution", 
     xlab = "Number of Accidents", 
     col = "skyblue", 
     border = "navy")

# QQ Plot for Rainfall
qqnorm(precipitation_data$Average, main = "QQ Plot of Rainfall")
qqline(precipitation_data$Average, col = "red")

# QQ Plot for Accidents
qqnorm(month_year_counts$Count, main = "QQ Plot of Accidents")
qqline(month_year_counts$Count, col = "red")

# Reset plotting area
par(mfrow = c(1, 1))
```
## 2. Univariate Block Maxima Analysis

```{r univariate_models, echo=FALSE}
# Based on the previous univariate analysis, the best models for each dataset were: - Road Accident data: Non-stationary GEV with linear trend in location parameter (Model 2) - Rainfall data: Stationary GEV (Model 1). We'll use these models for our bivariate analysis.

# Set up visualization grid (2 rows, 2 columns)
par(mfrow = c(2, 2))

# Prepare data for fitting
years_norm_road <- combined_data$Year - min(combined_data$Year)  # Normalize years

# Road Accident Model (Model 2: Non-stationary GEV with linear trend in location)
fit_model2_road <- fevd(combined_data$Road_Max, type = "GEV", method = "MLE", 
                        location.fun = ~ years_norm_road)

# Rainfall Model (Model 1: Stationary GEV)
fit_model1_rain <- fevd(combined_data$Rain_Max, type = "GEV", method = "MLE")

# Q-Q Plots with more informative titles
plot(fit_model2_road, type = "qq", main = "Road Accidents: Quantile Plot")
plot(fit_model1_rain, type = "qq", main = "Rainfall: Quantile Plot")

# Density plots for road accidents and rainfall with both empirical and modeled curves
# Road accidents density plot
plot(fit_model2_road, type = "density", main = "Road Accidents: Density Plot", 
     xlab = "Maximum Road Accidents", ylab = "Density", 
     legend = TRUE, col = c("black", "blue"), lty = c(1, 2))

# Rainfall density plot
plot(fit_model1_rain, type = "density", main = "Rainfall: Density Plot", 
     xlab = "Maximum Rainfall", ylab = "Density", 
     legend = TRUE, col = c("black", "blue"), lty = c(1, 2))
```

## 3. Dependence Structure Analysis

```{r dependence_analysis, echo=FALSE}
# Extract parameters from road accident model (non-stationary)
road_params <- fit_model2_road$results$par
# For each year, calculate the location parameter
road_locations <- road_params["mu0"] + road_params["mu1"] * years_norm_road
road_scale <- road_params["scale"]
road_shape <- road_params["shape"]

# Extract parameters from rainfall model (stationary)
rain_params <- fit_model1_rain$results$par
rain_location <- rain_params["location"]
rain_scale <- rain_params["scale"]
rain_shape <- rain_params["shape"]

# Transform to uniform margins
# Function to calculate GEV CDF
pgev_custom <- function(q, loc, scale, shape) {
  if(abs(shape) < 1e-6) {
    # Gumbel case (shape ≈ 0)
    return(exp(-exp(-(q - loc)/scale)))
  } else {
    # General GEV case
    t <- 1 + shape * (q - loc)/scale
    return(ifelse(t > 0, exp(-t^(-1/shape)), 0))
  }
}

# Transform road accident data (accounting for non-stationarity)
road_uniform <- numeric(length(combined_data$Road_Max))
for (i in 1:length(combined_data$Road_Max)) {
  road_uniform[i] <- pgev_custom(combined_data$Road_Max[i], 
                                road_locations[i], 
                                road_scale, 
                                road_shape)
}

# Transform rainfall data (stationary)
rain_uniform <- pgev_custom(combined_data$Rain_Max, 
                           rain_location, 
                           rain_scale, 
                           rain_shape)

# Create scatterplot of transformed data
plot(combined_data$Road_Max, combined_data$Rain_Max, 
     main = "Original Data", 
     xlab = "Maximum Fatal Road Accidents", 
     ylab = "Maximum Average Precipitation (mm)",
     pch = 19)

plot(road_uniform, rain_uniform, 
     main = "Transformed to Uniform Margins", 
     xlab = "Road Accidents (Uniform Scale)", 
     ylab = "Rainfall (Uniform Scale)",
     pch = 19)

# Calculate dependence measures
kendall_tau <- cor(road_uniform, rain_uniform, method = "kendall")
spearman_rho <- cor(road_uniform, rain_uniform, method = "spearman")

# Create table for dependence measures
dependence_table <- data.frame(
  Measure = c("Kendall's tau", "Spearman's rho"),
  Value = c(round(kendall_tau, 3), round(spearman_rho, 3))
)

kable(dependence_table, caption = "Dependence Measures between Road Accidents and Rainfall")
```

## 4. Copula Fitting

```{r echo=FALSE}
# Create a data frame of transformed variables
uniform_data <- data.frame(road = road_uniform, rain = rain_uniform)

# Define copula families to test
copula_families <- c("Gaussian", "Student's t", "Clayton", "Gumbel", "Frank", "Joe")

# Function to fit copulas and calculate AIC/BIC
fit_copulas <- function(data, families) {
  results <- data.frame(
    Family = character(),
    Parameters = character(),
    LogLik = numeric(),
    AIC = numeric(),
    BIC = numeric(),
    stringsAsFactors = FALSE
  )
  
  n <- nrow(data)
  
  for (family in families) {
    tryCatch({
      # Fit the copula
      if (family == "Gaussian") {
        cop <- normalCopula(0, dim = 2)
        fit <- fitCopula(cop, data = data, method = "ml")
        param_count <- 1
        param_value <- round(coef(fit), 3)
        param_string <- paste(param_value)
      } else if (family == "Student's t") {
        cop <- tCopula(0, dim = 2)
        fit <- fitCopula(cop, data = data, method = "ml")
        param_count <- 2
        param_value <- round(coef(fit), 3)
        param_string <- paste(param_value[1], "(df =", param_value[2], ")")
      } else if (family == "Clayton") {
        cop <- claytonCopula(1, dim = 2)
        fit <- fitCopula(cop, data = data, method = "ml")
        param_count <- 1
        param_value <- round(coef(fit), 3)
        param_string <- paste(param_value)
      } else if (family == "Gumbel") {
        cop <- gumbelCopula(1.5, dim = 2)
        fit <- fitCopula(cop, data = data, method = "ml")
        param_count <- 1
        param_value <- round(coef(fit), 3)
        param_string <- paste(param_value)
      } else if (family == "Frank") {
        cop <- frankCopula(1, dim = 2)
        fit <- fitCopula(cop, data = data, method = "ml")
        param_count <- 1
        param_value <- round(coef(fit), 3)
        param_string <- paste(param_value)
      } else if (family == "Joe") {
        cop <- joeCopula(1.5, dim = 2)
        fit <- fitCopula(cop, data = data, method = "ml")
        param_count <- 1
        param_value <- round(coef(fit), 3)
        param_string <- paste(param_value)
      }
      
      # Calculate log-likelihood, AIC, and BIC
      loglik <- logLik(fit)
      aic <- -2 * loglik + 2 * param_count
      bic <- -2 * loglik + param_count * log(n)
      
      # Add results to the dataframe
      results <- rbind(results, data.frame(
        Family = family,
        Parameters = param_string,
        LogLik = round(as.numeric(loglik), 3),
        AIC = round(aic, 3),
        BIC = round(bic, 3),
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      cat("Error fitting", family, "copula:", e$message, "\n")
    })
  }
  
  return(results)
}

# Fit copulas and get results
copula_results <- fit_copulas(uniform_data, copula_families)

# Display results
kable(copula_results, caption = "Copula Model Comparison")

# Identify the best model based on AIC
best_copula_aic <- copula_results$Family[which.min(copula_results$AIC)]
best_copula_bic <- copula_results$Family[which.min(copula_results$BIC)]

cat("Best copula model based on AIC:", best_copula_aic, "\n")
cat("Best copula model based on BIC:", best_copula_bic, "\n")

# Fit the best copula model for further analysis
best_family <- best_copula_aic  # Using AIC as selection criterion

if (best_family == "Gaussian") {
  best_copula <- normalCopula(0, dim = 2)
} else if (best_family == "Student's t") {
  best_copula <- tCopula(0, dim = 2)
} else if (best_family == "Clayton") {
  best_copula <- claytonCopula(1, dim = 2)
} else if (best_family == "Gumbel") {
  best_copula <- gumbelCopula(1.5, dim = 2)
} else if (best_family == "Frank") {
  best_copula <- frankCopula(1, dim = 2)
} else if (best_family == "Joe") {
  best_copula <- joeCopula(1.5, dim = 2)
}

best_fit <- fitCopula(best_copula, data = uniform_data, method = "ml")
```
## 5. Goodness-of-Fit Testing

```{r goodness_of_fit, echo=FALSE}
# Generate a sample from the fitted copula for visual comparison
set.seed(123)
sim_data <- rCopula(nrow(uniform_data), best_fit@copula)
colnames(sim_data) <- c("road", "rain")

# Visual diagnostics - compare observed and simulated data
plot(uniform_data, main = "Observed Data", 
     xlab = "Road Accidents (Uniform Scale)", 
     ylab = "Rainfall (Uniform Scale)",
     pch = 19)

plot(sim_data, main = paste("Simulated Data from", best_family, "Copula"), 
     xlab = "Road Accidents (Uniform Scale)", 
     ylab = "Rainfall (Uniform Scale)",
     pch = 19)

# Formal goodness-of-fit test using the gofCopula function from VineCopula package
set.seed(123)
gof_result <- gofCopula(best_copula, uniform_data, N = 1000)

# Display goodness-of-fit results
gof_table <- data.frame(
  Test = "Cramér-von Mises",
  Statistic = round(gof_result$statistic, 3),
  `p-value` = round(gof_result$p.value, 3)
)

# ensure all table columns are character and valid UTF-8
gof_table[] <- lapply(gof_table, function(col) {
  col <- as.character(col)
  iconv(col, from = "", to = "UTF-8", sub = "")  # drop or replace any bad bytes
})

# build a UTF-8–safe caption
caption_text <- paste("Goodness-of-Fit Test for", best_family, "Copula")
caption_text <- iconv(caption_text, from = "", to = "UTF-8", sub = "")

# now call kable
knitr::kable(
  gof_table,
  caption = caption_text,
  align   = "c"
)
```

```{r}
# Function to create QQ-plot for Gaussian copula
create_copula_qqplot <- function(copula_fit, data) {
  # Prepare data
  original_data <- data.frame(
    u1 = data$road,
    u2 = data$rain
  )
  
  # Generate theoretical quantiles using the fitted copula
  n <- nrow(data)
  param <- coef(copula_fit)  # This is rho for Gaussian copula
  
  # Generate random data from the fitted copula
  set.seed(123) # For reproducibility
  simulated_data <- rCopula(n, normalCopula(param, dim = 2))
  
  # Calculate empirical copula values (pseudo-observations)
  empirical_copula <- pobs(as.matrix(original_data))
  
  # Calculate the Kendall's function values
  # For empirical data
  K_emp <- numeric(n)
  for (i in 1:n) {
    K_emp[i] <- mean(empirical_copula[,1] <= empirical_copula[i,1] & 
                      empirical_copula[,2] <= empirical_copula[i,2])
  }
  
  # For simulated data
  K_sim <- numeric(n)
  for (i in 1:n) {
    K_sim[i] <- mean(simulated_data[,1] <= simulated_data[i,1] & 
                      simulated_data[,2] <= simulated_data[i,2])
  }
  
  # Sort the values for QQ-plot
  K_emp_sorted <- sort(K_emp)
  K_sim_sorted <- sort(K_sim)
  
  # Create the QQ-plot
  plot(K_sim_sorted, K_emp_sorted, 
       xlab = "Theoretical Quantiles (Gaussian Copula)",
       ylab = "Empirical Quantiles", 
       main = "QQ-Plot for Gaussian Copula",
       pch = 16, col = "blue")
  abline(0, 1, col = "red", lwd = 2)
  
  # Add grid for better readability
  grid()
  
  # Return the quantiles for further analysis if needed
  return(list(theoretical = K_sim_sorted, empirical = K_emp_sorted))
}

# Create the QQ-plot
qq_results <- create_copula_qqplot(best_fit, uniform_data)

# You can also add a more informative title with the parameter value
title(sub = paste("Gaussian Parameter (rho) =", round(coef(best_fit), 3)))

# To check the quality of fit more quantitatively, we can calculate the R²
lm_fit <- lm(qq_results$empirical ~ qq_results$theoretical)
r_squared <- summary(lm_fit)$r.squared
cat("R² of the QQ-plot fit:", round(r_squared, 4), "\n")

```

## 6. Joint Return Period Calculation

```{r joint_return, echo=FALSE}
# Define a grid for return level calculation
grid_size <- 50
u_grid <- v_grid <- seq(0.01, 0.99, length.out = grid_size)
grid <- expand.grid(u = u_grid, v = v_grid)

# Calculate joint CDF values using the fitted copula
joint_cdf <- pCopula(as.matrix(grid), best_fit@copula)

# Transform from uniform scale back to original scale
# For road accidents (accounting for non-stationarity)
# We'll use the most recent year's parameters
latest_year_index <- length(years_norm_road)
latest_road_location <- road_locations[latest_year_index]

# Function to calculate GEV quantile
qgev_custom <- function(p, loc, scale, shape) {
  if(abs(shape) < 1e-6) {
    # Gumbel case (shape ≈ 0)
    return(loc - scale * log(-log(p)))
  } else {
    # General GEV case
    return(loc + scale * ((-log(p))^(-shape) - 1) / shape)
  }
}

# Transform grid points back to original scale
grid$road_original <- qgev_custom(grid$u, latest_road_location, road_scale, road_shape)
grid$rain_original <- qgev_custom(grid$v, rain_location, rain_scale, rain_shape)

# Calculate joint return periods for "AND" scenario (both variables exceed thresholds)
# T_AND = 1/(1 - u - v + C(u,v))
grid$return_period_and <- 1 / (1 - grid$u - grid$v + joint_cdf)

# Calculate joint return periods for "OR" scenario (at least one variable exceeds threshold)
# T_OR = 1/(1 - C(u,v))
grid$return_period_or <- 1 / (1 - joint_cdf)

# Create contour plots
# First, reshape the data for contour plotting
contour_data_and <- matrix(grid$return_period_and, nrow = grid_size, ncol = grid_size)
contour_data_or <- matrix(grid$return_period_or, nrow = grid_size, ncol = grid_size)

# Define return periods of interest
return_periods <- c(10, 20, 30, 50, 100)

# Create contour plot for "AND" scenario
contour(u_grid, v_grid, contour_data_and, 
        levels = return_periods,
        labels = return_periods,
        xlab = "Road Accidents (Original Scale)",
        ylab = "Rainfall (Original Scale)",
        main = "Joint Return Periods (AND Scenario)",
        labcex = 0.8)
points(uniform_data, pch = 20, cex = 0.8)

# Create contour plot for "OR" scenario
contour(u_grid, v_grid, contour_data_or, 
        levels = return_periods,
        labels = return_periods,
        xlab = "Road Accidents (Original Scale)",
        ylab = "Rainfall (Original Scale)",
        main = "Joint Return Periods (OR Scenario)",
        labcex = 0.8)
points(uniform_data, pch = 20, cex = 0.8)

# Calculate specific joint return levels for different return periods
calculate_joint_return_levels <- function(return_period, copula_model, road_loc, road_scale, road_shape, 
                                         rain_loc, rain_scale, rain_shape) {
  # Find the threshold p such that the joint return period is T
  # For the "AND" scenario: p = 1 - 1/T
  p <- 1 - 1/return_period
  
  # Calculate univariate return levels
  road_level <- qgev_custom(p, road_loc, road_scale, road_shape)
  rain_level <- qgev_custom(p, rain_loc, rain_scale, rain_shape)
  
  # Calculate joint probability for these levels
  u <- pgev_custom(road_level, road_loc, road_scale, road_shape)
  v <- pgev_custom(rain_level, rain_loc, rain_scale, rain_shape)
  joint_prob <- pCopula(c(u, v), copula_model)
  
  # Calculate actual joint return period
  joint_return_and <- 1 / (1 - u - v + joint_prob)
  joint_return_or <- 1 / (1 - joint_prob)
  
  return(list(
    return_period = return_period,
    road_level = road_level,
    rain_level = rain_level,
    joint_return_and = joint_return_and,
    joint_return_or = joint_return_or
  ))
}

# Calculate joint return levels for different return periods
joint_results <- lapply(return_periods, function(T) {
  calculate_joint_return_levels(T, best_fit@copula, 
                               latest_road_location, road_scale, road_shape,
                               rain_location, rain_scale, rain_shape)
})

# Create a table of joint return levels
joint_table <- data.frame(
  `Return Period` = sapply(joint_results, function(x) x$return_period),
  `Road Accidents Level` = round(sapply(joint_results, function(x) x$road_level), 2),
  `Rainfall Level (mm)` = round(sapply(joint_results, function(x) x$rain_level), 2),
  `Joint Return Period (AND)` = round(sapply(joint_results, function(x) x$joint_return_and), 1),
  `Joint Return Period (OR)` = round(sapply(joint_results, function(x) x$joint_return_or), 1)
)
# Rename
names(joint_table) <- c(
  "Return Period",
  "Road Accidents Level",
  "Rainfall Level (mm)",
  "Joint Return Period AND",
  "Joint Return Period OR"
)
knitr::kable(
  joint_table,
  caption = "Joint Return Levels for Different Return Periods",
  align = "c"     # a single "c" will be recycled to all columns
)
```
