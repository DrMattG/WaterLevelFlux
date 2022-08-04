#' Update_data
#'
#' @return sf dataframe of regulated lakes with biodiversity data in Norway
#' @export
#'

update_data=function(){
  
library(tidyverse)
library(sf)
library(readxl)

Overview_biodiversity_data_in_reservoirs <- read_excel(paste0(here::here(),"/data/Overview biodiversity data in reservoirs.xlsx"), 
                                                       sheet = "Main data sheet")
overview=Overview_biodiversity_data_in_reservoirs %>% 
  janitor::clean_names()
overview=overview %>% 
  separate_rows(.,"year")
overview=overview %>%
  dplyr::select(!abstract_if_available)

regulerte_innsjoer <- readRDS(paste0(here::here(),"/data/regulerte_innsjoer.rds"))
regulerte_innsjoer<-regulerte_innsjoer %>% 
  janitor::clean_names()
shape <- read_sf(dsn =paste0(here::here(),"/data/NVEData/Innsjo/Innsjo_Innsjo.shp"), layer = "Innsjo_Innsjo")
subsetShape = subset(shape, vatnLnr %in% overview$id_vannlopenr_nve)
subsetShape_bio=subsetShape %>% 
  left_join(.,y = overview, by=c("vatnLnr"= "id_vannlopenr_nve"))
sfc = st_transform(subsetShape_bio, crs = "+proj=longlat +datum=WGS84")
write_rds(sfc, paste0(here::here(),"/data/sfc.RDS"))

}



