library(pdftools)
library(dplyr)
library(tidyr)

setwd("C:/Users/dafl/OneDrive - Folkehelseinstituttet/VKM Data/Vekstmedium_2023.08.09/Risk_Norway_plant_health_soil_growing_media/")

# Function to parse a line and return a data frame
parse_line <- function(line) {
  parts <- unlist(strsplit(line, "\\s{2,}"))
  
  # Skip header-like lines
  if (length(parts) >= 5 && parts[1] != "Export Country") {
    export_country <- parts[1]
    commodity_type <- parts[2]
    plant_species <- parts[3]
    harmful_organism <- parts[4]
    occurrences <- tryCatch(as.integer(parts[5]), warning = function(w) NA)
    
    return(data.frame(ExportCountry = export_country,
                      CommodityType = commodity_type, 
                      PlantSpecies = plant_species, 
                      HarmfulOrganism = harmful_organism, 
                      NumberOfOccurrences = occurrences, 
                      stringsAsFactors = FALSE))
  } else {
    return(NULL)
  }
}

# Function to process a single PDF file
process_pdf <- function(file_path) {
  pdf_text <- pdf_text(file_path)
  lines <- unlist(strsplit(pdf_text, "\n"))
  all_data <- data.frame()
  
  for (line in lines) {
    if (nchar(line) > 0) {
      df <- parse_line(line)
      if (!is.null(df)) {
        all_data <- rbind(all_data, df)
      }
    }
  }
  
  return(all_data)
}

# Set the directory containing your PDF files
setwd("C:/Users/dafl/Downloads/")

# List all PDF files in the directory
pdf_files <- list.files(pattern = "\\.pdf$")

# Process each file and combine the results
all_data <- do.call(rbind, lapply(pdf_files, process_pdf))

all_data <- na.omit(all_data)

all_data <- all_data %>%   mutate(across(where(is.character), ~na_if(., "")))

# Apply tidyr::fill()
all_data <- as_tibble(all_data) %>% tidyr::fill(ExportCountry, .direction = "down")

# View the first few rows of the combined data frame
all_data

all_data$HarmfulOrganism <- tolower(all_data$HarmfulOrganism)



# -----------------------------------------------------------------------------------------------------------------------------------------------
library(RSQLite)
library(DBI)

eppo_con <- dbConnect(SQLite(), dbname = "C:/Users/dafl/Desktop/EPPO DB/EPPO_DB.db")

eppo_data <- tbl(eppo_con, "EPPO") %>% 
  collect() %>% 
  filter(codelang=="la") %>%  
  filter(preferred==1) %>%
  filter(status=="A") %>%
  select(eppocode, fullname) %>% 
  as_tibble()

eppo_data$fullname <- tolower(eppo_data$fullname)

dbDisconnect(eppo_con)

# -----------------------------------------------------------------------------------------------------------------------------------------------

# filter join and keep only hits in all data
all_data <- full_join(all_data, eppo_data, by = c("HarmfulOrganism"="fullname"), na_matches = c("na")) 

backup <- all_data
all_data<- backup 

# clean -----------------------------------------------------------------------------------------------------------------------------------------


unique(all_data$CommodityType)
unique(all_data$PlantSpecies)

all_data <- all_data %>% mutate(across(1:3, tolower))

all_data$PlantSpecies <- gsub("sp.", "", all_data$PlantSpecies)
all_data$PlantSpecies <- gsub("\\s+$", "", all_data$PlantSpecies)


all_data$HarmfulOrganism <- gsub("sp.", "", all_data$HarmfulOrganism)
all_data$HarmfulOrganism <- gsub("\\s+$", "", all_data$HarmfulOrganism)





all_data <-all_data %>%
  mutate(CommodityType = str_replace_all(CommodityType, "_", " ")) %>%
  mutate(CommodityType = str_replace_all(CommodityType, ": ", " "))

all_data$CommodityType <- gsub("flowers and branches with", "flowers and branches", all_data$CommodityType)

# trim whitespace
all_data <- all_data %>% mutate(across(1:3, str_trim))

# add unique id column
all_data <- all_data %>% mutate(id = row_number())

# -----------------------------------------------------------------------------------------------------------------------------------------------

eppo_con <- dbConnect(SQLite(), dbname = "C:/Users/dafl/Desktop/EPPO DB/EPPO_DB.db")

eppo_tax<- tbl(eppo_con, "EPPO_GBIF_TAX") %>% 
  collect() %>% 
  as_tibble()

dbDisconnect(eppo_con)

eppo_tax$canonicalname <- tolower(eppo_tax$canonicalname)

pest_tax <- all_data %>% left_join(eppo_tax, by=c("HarmfulOrganism"="canonicalname"))

pest_tax <- pest_tax  %>%   
  select(id, ExportCountry, CommodityType,   PlantSpecies, HarmfulOrganism, NumberOfOccurrences, eppocode, genus, family,  order, phylum, kingdom,  rank)

pest_tax <- rename(pest_tax, 
                   pest_rank = rank, 
                   pest_kingdom = kingdom, 
                   pest_phylum = phylum, 
                   pest_order = order, 
                   pest_family = family, 
                   pest_genus = genus)

plant_tax <- all_data %>% left_join(eppo_tax, by=c("PlantSpecies"="canonicalname"))

plant_tax <- plant_tax  %>%   
  select(id, ExportCountry, CommodityType,   PlantSpecies, HarmfulOrganism, NumberOfOccurrences, eppocode, genus, family,  order, phylum, kingdom,  rank)

plant_tax <- rename(plant_tax, 
                    plant_rank = rank, 
                    plant_kingdom = kingdom, 
                    plant_phylum = phylum, 
                    plant_order = order, 
                    plant_family = family, 
                    plant_genus = genus)


data <- left_join(pest_tax, plant_tax)

# -----------------------------------------------------------------------------------------------------------------------------------------------

data %>%
  group_by(HarmfulOrganism, eppocode) %>%
  na.omit() %>%
  summarize(TotalOccurrences = sum(NumberOfOccurrences, na.rm = TRUE)) %>% 
  arrange(desc(TotalOccurrences))

data %>%
  group_by(ExportCountry) %>%
  summarize(TotalOccurrences = sum(NumberOfOccurrences, na.rm = TRUE)) %>% 
  arrange(desc(TotalOccurrences))

data %>%
  group_by(PlantSpecies) %>%
  summarize(TotalOccurrences = sum(NumberOfOccurrences, na.rm = TRUE))%>% 
  arrange(desc(TotalOccurrences))

# Summary of occurrences by commodity type
data %>%
  group_by(CommodityType) %>%
  summarize(TotalOccurrences = sum(NumberOfOccurrences, na.rm = TRUE)) %>% 
  arrange(desc(TotalOccurrences)) %>% 
  ungroup()

data %>%
  group_by(PlantSpecies, CommodityType, HarmfulOrganism, eppocode) %>%
  na.omit() %>%
  summarize(TotalOccurrences = sum(NumberOfOccurrences, na.rm = TRUE)) %>% 
  arrange(desc(TotalOccurrences))

sum_data <- data %>%
  group_by(PlantSpecies, HarmfulOrganism, eppocode) %>%
  na.omit() %>%
  summarize(TotalOccurrences = sum(NumberOfOccurrences, na.rm = TRUE)) %>% 
  arrange(desc(TotalOccurrences))

order <- data %>% 
  group_by(pest_order, plant_order) %>% 
  summarize(TotalOccurrences = sum(NumberOfOccurrences, na.rm = TRUE)) %>% 
  arrange(desc(TotalOccurrences))

# -----------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(ggalluvial)

ggplot(sum_data[1:20,], aes(axis1 = PlantSpecies, axis2 = HarmfulOrganism, y = TotalOccurrences)) +
  geom_alluvium(aes(fill = eppocode)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Alluvial Plot of most frequent interceptions in EUROPHYTE") +
  xlab("Categories") +
  ylab("Total Occurrences")

# -----------------------------------------------------------------------------------------------------------------------------------------------

genus <- data %>% 
  group_by(pest_genus, plant_genus) %>% 
  summarize(TotalOccurrences = sum(NumberOfOccurrences, na.rm = TRUE)) %>% 
  arrange(desc(TotalOccurrences)) %>% 
  na.omit()

ggplot(genus[1:25,], aes(axis1 =  pest_genus, axis2 = plant_genus, y = TotalOccurrences)) +
  geom_alluvium(aes(fill = pest_genus)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Alluvial Plot of most frequent interceptions in EUROPHYTE") +
  xlab("Categories") +
  ylab("Total Occurrences")

# -----------------------------------------------------------------------------------------------------------------------------------------------

family <- data %>% 
  group_by(pest_family, plant_family) %>% 
  summarize(TotalOccurrences = sum(NumberOfOccurrences, na.rm = TRUE)) %>% 
  arrange(desc(TotalOccurrences)) %>% 
  na.omit()


ggplot(family[1:20,], aes(axis1 =  pest_family, axis2 = plant_family, y = TotalOccurrences)) +
  geom_alluvium(aes(fill = pest_family)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Alluvial Plot of most frequent intercepted families in EUROPHYTE") +
  xlab("Categories") +
  ylab("Total Occurrences")

# -----------------------------------------------------------------------------------------------------------------------------------------------


# Load necessary library
library(stringr)

# Filter rows where 'CommodityType' contains 'soil'
soil_occurrences <- all_data %>% filter(str_detect(CommodityType, pattern = "soil"))

# View the rows where 'soil' is found in 'CommodityType'
head(soil_occurrences)