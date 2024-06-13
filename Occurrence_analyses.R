# Library ----
library(dplyr)

# Read in CSV file (GBIF) ----
occurrence_data <- read.csv("C:/Users/taram/Documents/Lab Analysis/NSERC USRA/Clarkia_OccurrenceData_GBIF/GBIF_occurrence_cleaned.csv")

# Getting rid of more (possibly) unnecessary columns ----
occurrence_data_columnclean <- subset(occurrence_data, select = -c(gbifID, references, type, institutionID, collectionID, datasetID, institutionCode, occurrenceID, catalogNumber, recordNumber, recordedBy, occurrenceRemarks, organismID, organismName, fieldNumber, verbatimEventDate, island, identifiedBy, datasetKey, lastInterpreted, publishedByGbifRegion))
  #Check to see if you should get rid of any more columns

occurrence_data_columnclean_2.0 <- occurrence_data_columnclean %>%
  filter(species != "" & !is.na(species))

occurrence_speciesclean <- subset(occurrence_data_columnclean_2.0, !species %in% c("Clarkia modesta", "Clarkia springvillensis", "Clarkia heterandra", "Clarkia jolonensis", "Clarkia australis", "Godetia sparsiflora", "Clarkia romanzovii"))
#I kind of want the species column first and everything else to the right of that; can do that later I think

# Making a table of occurrences ----
species_count_table <- table(occurrence_speciesclean$species)
species_count_df <- as.data.frame(species_count_table)
# Rename the columns
names(species_count_df) <- c("Species", "Count")
print(species_count_df)
#Table issues: ----
  #I don't think GBIF shows the subspecies, is that a problem?
  #blank cell at beginning of table, possibly indicating NAs in the 'species' column (fixed this)
  # I think C. mildredae needs to be combined with C. mildrediae because these are the same, no? How do I do that?
  # Some of these species are not in Kimmy's phylogeny; I removed them
      # C. modesta, C. springvillensis, C. heterandra, C. jolonensis, C. australis
  #Random occurrence not part of genus(???): Godetia sparsiflora (removed this but can put back)



# Read in other occurrence data to supplement occurrences ----
## Read in CSV file (INAT) ----
INAT_occurrence_data <- read.csv("C:/Users/taram/Documents/Lab Analysis/NSERC USRA/Clarkia_OccurrenceData_INAT/INAT_occurrence_cleaned.csv")

#Getting rid of more (possibly) unnecessary columns
INAT_occurrence_data_columnclean <- subset(INAT_occurrence_data, select = -c(id, observed_on_string, user_id, user_login, user_name, url, image_url, sound_url, oauth_application_id, positioning_method, positioning_device))

#Table of occurrences  
INAT_species_count_table <- table(INAT_occurrence_data_columnclean$scientific_name)
INAT_species_count_df <- as.data.frame(INAT_species_count_table)
# Rename the columns
names(INAT_species_count_df) <- c("Species", "Count")
print(INAT_species_count_df)



## Read in CSV file (CCH2) ----
CCH2_occurrence_data <- read.csv("C:/Users/taram/Documents/Lab Analysis/NSERC USRA/Clarkia_OccurrenceData_CCH2/CCH2_occurrences_cleaned.csv")

CCH2_occurrence_data_columnclean <- subset(CCH2_occurrence_data, select = -c(id, institutionCode, collectionCode, basisOfRecord, occurrenceID, catalogNumber, otherCatalogNumbers, scientificNameAuthorship, identifiedBy, identificationReferences, identificationQualifier, typeStatus, recordedBy, associatedCollectors, recordNumber, endDayOfYear, cultivationStatus))

#Table of occurrences
CCH2_species_count_table <- table(CCH2_occurrence_data_columnclean$scientificName)
CCH2_species_count_df <- as.data.frame(CCH2_species_count_table)
# Rename the columns
names(CCH2_species_count_df) <- c("Species", "Count")
print(CCH2_species_count_df)



## Read in CSV file (PNW Herbaria) ----
PNW_occurrence_data <- read.csv("C:/Users/taram/Documents/Lab Analysis/NSERC USRA/Clarkia_Occurrence_Data_PNW/PNW_occurrence_cleaned.csv")

PNW_occurrence_data_columnclean <- subset(PNW_occurrence_data, select = -c(OccurrenceID, GUID, Catalog, Barcode, Notho.Genus, Name.Qualifier, Qualifier.Position, Type.Status, Collector, Collector.Number, Other.Collectors, Maximum.Elevation.in.Meters, Georeferenced.By))

#Table of occurrences
PNW_species_count_table <- table(PNW_occurrence_data_columnclean$Scientific.Name)
PNW_species_count_df <- as.data.frame(PNW_species_count_table)
# Rename the columns
names(PNW_species_count_df) <- c("Species", "Count")
print(PNW_species_count_df)



# Now that I have all of the occurrence data, how do I go about combining data from the above three to make sure all occurrences are >100? 
  # Based on the GBIF data, we need more occurrences for C. exilis (none have higher occurrence than GBIF), C. imbricata (none have higher occurrence than GBIF), C. lingulata (none have higher occurrence than GBIF), and C. virgata (none have higher occurrence than GBIF)



# Map ----

