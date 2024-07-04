#link to OSF and get the metadata
library(tidyverse)
library(osfr)
library(leaflet)
library(sf)
library(readxl)
library(LivingNorwayR)

osfr::osf_auth(Sys.getenv("osf"))# you need to get a token and save it to .Renvironment to use osfr
wf_project <- osf_retrieve_node("9r6vn")
wf_project
files<-osfr::osf_ls_files(wf_project)

metadata<-osfr::osf_download(files[2,],conflicts ="overwrite" )

Metadata_file <- read_excel("Metadata_for_all/Metadata file.xlsx")

Metadata_file

datasets<-osfr::osf_download(files[1,], recurse = TRUE)


#Andersvatn
Andersvatn_fisk <- read_excel("Datasets/Svorka hydropower_Andersvatn 2009/Andersvatn_fisk.xls", 
                                    col_names = FALSE)

###split out extra fish section
Extrafish<- Andersvatn_fisk[57:131,] ## Not sure what to do with extrafisk!


## Need to tidy up the dataframe so the header is correct

##Fish numbers = 1 Trout
              #  2 Char
              #  3 Minnow
              #  4 Stickleback
## Sex = 1 Male 2 Female

## Stage?? 1 to 7

## Meat colour 1 white 2 pink 3 red
# Stomach fullness 1 to 5
# Parasites 1 to 4 (1 =no, 4 = a lot)
# VatnID 2164

sfc <- readRDS(paste0(here::here(), "/data/sfc.RDS"))
sfc<-sfc %>% 
  filter(vatnLnr==2164)

sfc$geom2<-st_centroid(sfc)

plotdata<-sf::st_zm(sfc)

leaflet() %>% 
    addProviderTiles(
      provider = providers$CartoDB.Positron,
      options = providerTileOptions(
        noWrap = FALSE
      )
    ) %>% 
    setView(
      lat = 63,
      lng = 9,
      zoom = 4
    ) %>% 
    addPolygons(data =plotdata,
                layerId =~ vatnLnr, 
                popup=paste("ID: ", sfc$vatnLnr, "<br>",
                            "navn: ", sfc$navn, "<br>"))%>% 
  addCircles(data = plotdata$geom2, fill = TRUE, stroke = TRUE, color = "#000", fillColor = "blue", radius = 10000, weight = 5)   ## I increased the radius to get it to display 


Andersvatn_fisk_tibble<-Andersvatn_fisk[-(1:3),]
Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble[1:53,]
colnames(Andersvatn_fisk_tibble)<-Andersvatn_fisk[3,]

### Add type, datasetName, ownerInstitutionCode

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(ownerInstitutionCode="NINA",
         type="Event",
         datasetName="Andersvatn_fisk")

### Event ID (all the same for each row)

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(eventID=uuid::UUIDgenerate(use.time = FALSE))



### Event date - use year??

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(year=år)

### Event date - convert date col to ISO 8601-1:2019
Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(eventDate=paste0(dato,year))

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(eventDate=as.Date(eventDate, format="%d.%m.%Y"))

### How to add day/night (all are night - so perhaps this can go in metadata but not sure about other datasets) verbatimEventDate


Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(verbatimEventDate=
           case_when(
             `dag/natt`=="dag"~"daytime",
             `dag/natt`=="natt"~"nighttime"
           ),
         verbatimEventDate=paste0(eventDate, " ", verbatimEventDate))


### Each row is a sample? USe meshsize?
Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(sampleSizeValue=`maskev [mm]`,
         sampleSizeUnit="mm")


### CountryCode is NO

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(countryCode="NO",
         country="Norway")

### locationID - use the VatnID 2164??

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(locationID="VatnID:2164")

### Use centroid for location 

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(decimalLatitude=63.03226,
         decimalLongitude=8.870878)



### use centroid (from above) and geodeticDatum

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(geodeticDatum="WGS 84")

#8.870878 ymin: 63.03226


Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(municipality="Surnadal")

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(waterBody="Andersvatn")



## Sampling net=garn

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(samplingProtocol="Test fishing with nets (Jensen-series and Nordic bottom net)")

### Garn number is fieldNumber
Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(fieldNumber=`garn (D/Nr)`)


### create Event

eventDF<-Andersvatn_fisk_tibble %>% 
  select(c(ownerInstitutionCode,type,datasetName,
           eventID, year, eventDate,
           sampleSizeValue,sampleSizeUnit,
           countryCode, country,locationID,
           decimalLatitude, decimalLongitude, geodeticDatum,municipality, waterBody,
           fieldNumber,samplingProtocol,
           verbatimEventDate))


eventDF
### Make Event Core
GBIF_Event=initializeGBIFEvent(eventDF, idColumnInfo = "eventID", nameAutoMap = TRUE)

### Occurrence extension

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(type="Occurrence",
         collectionCode="Andersvatn_fisk") %>% 
  rowwise() %>% 
    mutate(occurrenceID=uuid::UUIDgenerate(use.time=FALSE))

### Organism quantity and type are 1 individual fish

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(organismQuantity=1,
         organismQuantityType="individual")
### scientificName vernacularName

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(scientificName=
           case_when(art==1 ~ "Salmo trutta",
                     art==2 ~"Salvelinus alpinus",
                     art==3 ~ "Phoxinus phoxinus",
                     art==4 ~"Gasterosteus aculeatus"))

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(vernacularName=
           case_when(art==1 ~ "Trout",
                     art==2 ~"Char",
                     art==3 ~ "Minnow",
                     art==4 ~"Stickleback"))



Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(kingdom="Animalia",
         phylum="Chordata",
         class="Actinopterygii")


Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(order=
           case_when(art==1 ~ "Salmoniformes",
                     art==2 ~"Salmoniformes",
                     art==3 ~ "Cypriniformes",
                     art==4 ~"Gasterosteiformes"
    
  ))

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(family=
           case_when(art==1 ~ "Salmonidae",
                     art==2 ~"Salmonidae",
                     art==3 ~ 	"Leuciscidae",
                     art==4 ~"Gasterosteidae"
                     
           ))

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(genus=
           case_when(art==1 ~ "Salmo",
                     art==2 ~"Salvelinus",
                     art==3 ~ 	"Phoxinus"	,
                     art==4 ~"Gasterosteus"
                     
           )) %>% 
  mutate(taxonRank="species")


Andersvatn_fisk_tibble <-Andersvatn_fisk_tibble %>% 
  mutate(sex=case_when(
    kjønn==1~ "male",
    kjønn==2~"female"
  ))

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(basisOfRecord="Event")

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(LifeStage=stadium) # This should be controlled vocab

Andersvatn_fisk_tibble<-Andersvatn_fisk_tibble %>% 
  mutate(LifeStage=alder) # This should be controlled vocab


occ_ext=Andersvatn_fisk_tibble %>% 
  select(type,collectionCode,basisOfRecord,occurrenceID, organismQuantity, organismQuantityType, 
         eventID, eventDate, scientificName,kingdom, phylum, class, order, family, genus,vernacularName,taxonRank, sex)

GBIF_Occ=initializeGBIFOccurrence(occ_ext, idColumnInfo = "occurrenceID",nameAutoMap = TRUE )

GBIF_Occ

### Measurement or fact

M_or_f=Andersvatn_fisk_tibble %>% 
  mutate(measurementID=uuid::UUIDgenerate(use.time = FALSE))

M_or_f_length<-M_or_f %>% 
  select(measurementID, `lengde [mm]`, occurrenceID, eventID) %>% 
  mutate(measurementType= "length",
         measurementValue= `lengde [mm]`,
         measurementUnit= "mm") %>% 
  select(-`lengde [mm]`)
  

M_or_f_vekt<-M_or_f %>% 
  select(measurementID, `vekt [g]`, occurrenceID, eventID) %>% 
  mutate(measurementType= "weight",
         measurementValue= `vekt [g]`,
         measurementUnit= "g") %>% 
  select(-`vekt [g]`)

  
M_or_f_mage<-M_or_f %>% 
  select(measurementID, mage, occurrenceID, eventID) %>% 
  mutate(measurementType= "Stomach fullness",
         measurementValue= mage,
         measurementUnit= "scale 1 (empty) to 5 (full)") %>% 
  select(-mage)

## Not sure what to do with mageprøve

## Meat colour 1 white 2 pink 3 red

M_or_f_kjøtt<-M_or_f %>% 
  select(measurementID, kjøttfarge, occurrenceID, eventID) %>% 
  mutate(measurementType= "Meat colour",
         measurementValue= 
           case_when(
           kjøttfarge==1~ "white",
           kjøttfarge==2~ "pink",
           kjøttfarge==3~ "red"),
           measurementUnit= "visual assessment of colour") %>% 
  select(-kjøttfarge)

# Parasites 1 to 4 (1 =no, 4 = a lot)

M_or_f_parasites<-M_or_f %>% 
  select(measurementID, parasitter, occurrenceID, eventID) %>% 
  mutate(measurementType= "Parasite load",
         measurementValue= 
           case_when(
            parasitter==1~ "none",
            parasitter==2~ "few",
            parasitter==3~ "some",
            parasitter==4~ " a lot"),
         measurementUnit= "Assessment parasite load") %>% 
  select(-parasitter)




M_or_f_sample<-M_or_f %>%
  select(measurementID, lenke, eventID) %>%
  mutate(measurementType="link number on net",
         measurementValue= lenke,
         measurementUnit= "link") %>%
  mutate(occurrenceID=NA) %>% 
  select(-lenke)



M_or_f<-M_or_f %>% 
  select(measurementID, eventID, occurrenceID) %>% 
  bind_rows(.,M_or_f_kjøtt, M_or_f_length, 
            M_or_f_mage, M_or_f_parasites, 
            M_or_f_sample, M_or_f_vekt)

M_or_f<-M_or_f %>% 
  drop_na(measurementType)

GBIFMeasurementOrFact<-initializeGBIFMeasurementOrFact(M_or_f,idColumnInfo = "eventID",nameAutoMap = TRUE )
  
GBIFMeasurementOrFact

GBIFMeasurementOrFactEXT<-LivingNorwayR:::initializeGBIFExtendedMeasurementOrFact(M_or_f,idColumnInfo = "eventID",nameAutoMap = TRUE )

GBIFMeasurementOrFactEXT

## Metadata (bypass the LN approach as that is being redesigned)

library(EML)

# - eml
#   - dataset
#     - creator
#     - title
#     - publisher
#     - pubDate
#     - keywords
#     - abstract 
#     - intellectualRights
#     - contact
#     - methods
#     - coverage
#       - geographicCoverage
#       - temporalCoverage
#       - taxonomicCoverage
#   - dataTable
#     - entityName
#     - entityDescription
#     - physical
#     - attributeList




geographicDescription <- paste0(eventDF$waterBody[1]," lake in ", Metadata_file$`Geographic scope`)
begin = min(eventDF$eventDate)
end = max(eventDF$eventDate)
sci_names=unique(occ_ext$scientificName)

boundingbox=sf::st_bbox(sfc)

coverage <- 
  set_coverage(begin = begin, end = end,
               sci_names = paste0(sci_names[1], ", ", sci_names[2]),
               geographicDescription = geographicDescription,
               west = boundingbox[2], east = boundingbox[4], 
               north = boundingbox[1], south = boundingbox[3])
coverage

title<- paste0(eventDF$samplingProtocol[1], "in the ", eventDF$waterBody[1], ", ", eventDF$municipality[1], ", ", eventDF$country[1])

R_person <- person("Oyvind", "Solem", ,"Oyvind.solem@nina.no", "cre")
OS <- as_emld(R_person)


organizationName<-"Norwegian Institute for Nature Research"


contact<-
  list(
    individualName=OS$individualName,
    electronicMailAddress=OS$electronicMailAddress,
    organizationName=organizationName
  )

dataset <- list(
  title = title,
  creator = OS,
  pubDate = Sys.Date(),
  #intellectualRights = intellectualRights,
  #abstract = abstract,
  #associatedParty = associatedParty,
  #keywordSet = keywordSet,
  coverage = coverage,
  contact = contact
  #,
  #methods = methods,
  #dataTable = dataTable
  )

eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid", # type of identifier
  dataset = dataset)

write_eml(eml, "eml.xml")
eml_validate("eml.xml")

## create the DWCArchive

GBIFmeta=initializeDwCMetadata("eml.xml")

DWCA<-LivingNorwayR::initializeDwCArchive(GBIF_Event, c(GBIF_Occ, GBIFMeasurementOrFactEXT), metadata = GBIFmeta)
DWCA

