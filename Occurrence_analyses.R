# Library ----
# library(dplyr)
# library(ggplot2)
# MB: replacing both of the above with tidyverse, unless you'd rather not
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Read in CSV file (GBIF) ----
# occurrence_data <- read.csv("C:/Users/taram/Documents/Lab Analysis/NSERC USRA/Clarkia_OccurrenceData_GBIF/GBIF_occurrence_cleaned.csv")
occurrence_data <- read.csv("GBIF_occurrence_cleaned.csv")
# MB: I added these files to the gitignore, so if you want, you can put them directly in your project folder and git won't try to add them. this is nice because it lets us keep them in the same place and use the same path. 

# Getting rid of more (possibly) unnecessary columns ----
occurrence_data_columnclean <- occurrence_data %>% 
  select(-c(gbifID, references, type, institutionID, collectionID, datasetID, 
            # institutionCode, 
            collectionCode, datasetName, occurrenceID, recordNumber, 
            recordedBy, reproductiveCondition, occurrenceStatus, preparations, 
            organismID, organismName, previousIdentifications, fieldNumber, eventDate, eventTime, verbatimEventDate, 
            locationID, higherGeographyID, higherGeography, island, identificationID, identifiedBy, dateIdentified,
            taxonID, scientificNameID, acceptedNameUsageID, acceptedNameUsage, parentNameUsage, higherClassification, 
            kingdom, phylum, class, order, family, genericName, taxonRank, verbatimTaxonRank, nomenclaturalCode, taxonomicStatus, 
            datasetKey, publishingCountry, mediaType, hasCoordinate, taxonKey, acceptedTaxonKey, kingdomKey, phylumKey, classKey, 
            orderKey, familyKey, genusKey, speciesKey, acceptedScientificName, verbatimScientificName, protocol, lastParsed, 
            lastCrawled, repatriated, isSequenced, gbifRegion, lastInterpreted,level0Gid, level0Name,level1Gid, level1Name, 
            level2Gid, level2Name, iucnRedListCategory, publishedByGbifRegion))
  #Check to see if you should get rid of any more columns
# MB: switched the above from subset to select (unless you were using subset for a specific reason, in which case you can go back to it, i just usually use select for this type of thing)
# MB: modified to keep institution code and catalog number because those could actually be useful for identifying duplicate records
# MB, after doing this, realize we should have picked the ones we need rather than listing the ones we didn't need!


# Example code for how to select and rename columns, and filter to a certain range of latitudes
# new_data = occurrence_data %>% 
#   select(taxon = species, yr = year) %>% 
#   filter(decimalLatitude > 35 & decimalLatitude < 55)

occurrence_data_columnclean_2.0 <- occurrence_data_columnclean %>%
  filter(species != "" & !is.na(species))


occurrence_speciesclean <- occurrence_data_columnclean_2.0 %>% 
  filter(!species %in% c("Clarkia modesta", "Clarkia springvillensis", "Clarkia heterandra", "Clarkia jolonensis", "Clarkia australis", "Godetia sparsiflora", "Clarkia romanzovii")) %>%
  # MB: also adding a line to combine species, subspecies
  unite(col = speciesSubspecies, sep = " ", remove = FALSE, na.rm = TRUE, genus, specificEpithet, infraspecificEpithet) %>% 
  select(species, genus, specificEpithet, infraspecificEpithet, speciesSubspecies, everything())

#I kind of want the species column first and everything else to the right of that; can do that later I think
# MB: we can do that! I've added it here. You can reorder further by listing column names in select, then use everything() to add all the rest that you haven't named.


# Making a table of occurrences ----

species_count_table <- table(occurrence_speciesclean$species)
species_count_df <- as.data.frame(species_count_table)
# Rename the columns
names(species_count_df) <- c("Species", "Count")
print(species_count_df)

#Table issues: ----
  #I don't think GBIF shows the subspecies, is that a problem?
# MB: they're in another column, I'll show you one way to handle it
  #blank cell at beginning of table, possibly indicating NAs in the 'species' column (fixed this)
# MB: nice job on that
  # I think C. mildredae needs to be combined with C. mildrediae because these are the same, no? How do I do that?
# MB: I noticed that too, I'll show you how
  # Some of these species are not in Kimmy's phylogeny; I removed them
      # C. modesta, C. springvillensis, C. heterandra, C. jolonensis, C. australis
# MB: Nice job on that!
  #Random occurrence not part of genus(???): Godetia sparsiflora (removed this but can put back)
# MB: Okay to remove I think, this probably is a clarkia but hasn't been reidentified in the current genus




# Read in other occurrence data to supplement occurrences ----
## Read in CSV file (INAT) ----
# INAT_occurrence_data <- read.csv("C:/Users/taram/Documents/Lab Analysis/NSERC USRA/Clarkia_OccurrenceData_INAT/INAT_occurrence_cleaned.csv")
INAT_occurrence_data <- read.csv("INAT_occurrence_cleaned.csv")

#Getting rid of more (possibly) unnecessary columns
INAT_occurrence_data_columnclean <- subset(INAT_occurrence_data, select = -c(id, observed_on_string, user_id, user_login, user_name, url, image_url, sound_url, oauth_application_id, positioning_method, positioning_device))

#Table of occurrences  
INAT_species_count_table <- table(INAT_occurrence_data_columnclean$scientific_name)
INAT_species_count_df <- as.data.frame(INAT_species_count_table)
# Rename the columns
names(INAT_species_count_df) <- c("Species", "Count")
print(INAT_species_count_df)



## Read in CSV file (CCH2) ----
# CCH2_occurrence_data <- read.csv("C:/Users/taram/Documents/Lab Analysis/NSERC USRA/Clarkia_OccurrenceData_CCH2/CCH2_occurrences_cleaned.csv")
CCH2_occurrence_data <- read.csv("CCH2_occurrences_cleaned.csv")

CCH2_occurrence_data_columnclean <- subset(CCH2_occurrence_data, select = -c(id, institutionCode, collectionCode, basisOfRecord, occurrenceID, catalogNumber, otherCatalogNumbers, scientificNameAuthorship, identifiedBy, identificationReferences, identificationQualifier, typeStatus, recordedBy, associatedCollectors, recordNumber, endDayOfYear, cultivationStatus))

#Table of occurrences
CCH2_species_count_table <- table(CCH2_occurrence_data_columnclean$scientificName)
CCH2_species_count_df <- as.data.frame(CCH2_species_count_table)
# Rename the columns
names(CCH2_species_count_df) <- c("Species", "Count")
print(CCH2_species_count_df)





## Read in CSV file (PNW Herbaria) ----
PNW_occurrence_data <- read.csv("C:/Users/taram/Documents/Lab Analysis/NSERC USRA/Clarkia_Occurrence_Data_PNW/PNW_occurrence_cleaned.csv")
PNW_occurrence_data <- read.csv("PNW_occurrence_cleaned.csv")

PNW_occurrence_data_columnclean <- subset(PNW_occurrence_data, select = -c(OccurrenceID, GUID, Catalog, Barcode, Notho.Genus, Name.Qualifier, Qualifier.Position, Type.Status, Collector, Collector.Number, Other.Collectors, Maximum.Elevation.in.Meters, Georeferenced.By))

#Table of occurrences
PNW_species_count_table <- table(PNW_occurrence_data_columnclean$Scientific.Name)
PNW_species_count_df <- as.data.frame(PNW_species_count_table)
# Rename the columns
names(PNW_species_count_df) <- c("Species", "Count")
print(PNW_species_count_df)



# Now that I have all of the occurrence data, how do I go about combining data from the above three to make sure all occurrences are >100? 
  # Based on the GBIF data, we need more occurrences for C. exilis (none have higher occurrence than GBIF), C. imbricata (none have higher occurrence than GBIF), C. lingulata (none have higher occurrence than GBIF), and C. virgata (none have higher occurrence than GBIF)



# Maps ----
## GBIF Map ----
#convert to spatial format but first get rid of NA values 
  #but does this mean I am losing data???
#Getting rid of NA values 
occurrence_speciesclean <- occurrence_speciesclean %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>% 
  filter(species =="Clarkia arcuata")
#Converting to spatial format 
occurrence_sf <- st_as_sf(occurrence_speciesclean, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
#Plotting
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = occurrence_sf, aes(color = species), size = 2, alpha = 0.7) +
  theme_minimal() +
  coord_sf(xlim = c(-125, -115), ylim = c(30, 50), expand = FALSE) +
  labs(title = "GBIF Occurrence Map: Genus Clarkia",
       x = "Longitude",
       y = "Latitude",
       color = "Species")

## INAT Map ----
#Removing lat/long rows with missing data 
INAT_occurrence_data_columnclean <- INAT_occurrence_data_columnclean %>%
  filter(!is.na(latitude) & !is.na(longitude))
#Convert to spatial format 
INAT_occurrence_sf <- st_as_sf(INAT_occurrence_data_columnclean, coords = c("longitude", "latitude"), crs = 4326)

#Plotting
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = INAT_occurrence_sf, aes(color = scientific_name), size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "INAT Occurrence Map: Genus Clarkia",
    x = "Longitude",
    y = "Latitude",
    color = "Species"
  )


## CCH2 Map ---- 
#Removing lat/long rows with missing data
CCH2_occurrence_data_columnclean <- CCH2_occurrence_data_columnclean %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude))
#Convert to spatial format
CCH2_occurrence_sf <- st_as_sf(CCH2_occurrence_data_columnclean, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

#Plotting
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = CCH2_occurrence_sf, aes(color = scientificName), size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "CCH2 Occurrence Map: Genus Clarkia",
    x = "Longitude",
    y = "Latitude",
    color = "Species"
  ) +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0, "cm"),
  )
#It literally looks so ugly and smushed. Idk, fix this Tara 

## PNW Map ----
# Remove rows with missing values 
PNW_occurrence_data_columnclean <- PNW_occurrence_data %>%
  filter(!is.na(Decimal.Latitude) & !is.na(Decimal.Longitude))
#Convert to spatial format 
PNW_occurrence_sf <- st_as_sf(PNW_occurrence_data_columnclean, coords = c("Decimal.Longitude", "Decimal.Latitude"), crs = 4326)

#Plotting 
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = PNW_occurrence_sf, aes(color = Scientific.Name), size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "PNW Occurrence Map: Genus Clarkia",
    x = "Longitude",
    y = "Latitude",
    color = "Species"
  )
#Idk why I can't see the map 






