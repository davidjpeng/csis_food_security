library(tidyverse)
library(haven)
library(readxl)

library(sf)
library(sfheaders)
library(exactextractr)
library(terra)
library(tidyterra)  
library(flexurba)

options(scipen=999)
setwd('~/Desktop/Food/')

# Read-in FAO FIES data. Requests for access can be made here: https://microdata.fao.org/index.php/catalog
fies_data <- read_dta('PAK_2022_FIES_v01_EN_M_v01_A_OCS.dta')

## Note that the data used in our research is augmented with GPS coordinates at the ultimate cluster level of the survey. These coordinates underwent differential privacy
## techniques to protect respondents while still enabling us to approximate reconstructing DEGURBA from the bottom up. We were able to access this through collaboration with the FAO and their partners such as the Gallup World Poll. 

gps_augmentation <- read_xlsx('~/Desktop/Food/gallup_provided_gps/Pakistan_FIES_2022_coordinates.xlsx')

fies_data <- fies_data %>%
  left_join(gps_augmentation, by=c("Random_ID"="WPID_RANDOM"))

# Download and transform GADM boundaries for country of interest. 

download.file(
  "https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_PAK.gpkg",
  "~/Desktop/Food/GADM/gadm41_PAK.gpkg"
)

# Check the layers available 
st_layers('~/Desktop/Food/GADM/gadm41_PAK.gpkg')

# Convert to Mollweide, and select only relevant attributes
units_country <-
  sf::st_read('~/Desktop/Food/GADM/gadm41_PAK.gpkg', layer='ADM_ADM_3') %>% #layer is administrative level 3, which matches what GHS_DUC_Globe shows. We choose to match this to retain consistency. 
  sf::st_transform('ESRI:54009')

#Ensure coordinates of FIES are on the same CRS: 
fies_sf <- fies_data %>%
  select(Random_ID, LONGITUDE_DISPLACED, LATITUDE_DISPLACED) %>%
  st_as_sf(coords = c("LONGITUDE_DISPLACED", "LATITUDE_DISPLACED"), crs = 4326) %>% 
  sf::st_transform('ESRI:54009')

##QC
st_crs(fies_sf) == st_crs(units_country)

#Visual check to ensure projections are correct: 
ggplot() +
  geom_sf(data = units_country, fill = NA, color = "blue") +  # Polygons
  geom_sf(data = fies_sf, color = "red")  # Points (surveys)

#Join the two spatial objects: 
interviews_with_units <- st_join(fies_sf, units_country, left = FALSE)

##QC
table(is.na(interviews_with_units$GID_3)) #are there any unassigned interviews?

#Pull in raster data, in our case, from Worldpop, split by the two age groups that match the FIES methodology. We elect to use this particular set
#from Worldpop: https://hub.worldpop.org/geodata/listing?id=88

## NOTE: prior to reading in the Worldpop data, the rasters need to re-projected to Mollweide and the same grid sizes that the DEGURBA methodology utilizes (1km2). Doing so, while preserving the population counts correctly is not trivial. 
## Thanks to Luca Maffenini at the Eurpean Commission perfoming a fix to the tool, we are able to leverage GHS-POPWARP https://human-settlement.emergency.copernicus.eu/tools/GHS-POPWARP_User_Guide.pdf?rnd=0.9094042696948472 to perform the projection 
## to the Worldpop rasters. This conversion is done outside R. 

##0-14 

under_15_file_list <- list.files("/home/davidpeng/Desktop/DEGURBA/pakistan/unadj_constrained/warped/under_15", pattern=".tif", full.names = TRUE)
under_15_rasters <- lapply(under_15_file_list, terra::rast)
under_15_sprc <- terra::sprc(under_15_rasters)
under_15_mosaic <- terra::mosaic(under_15_sprc, fun="sum")

##15+

over_15_file_list <- list.files("/home/davidpeng/Desktop/DEGURBA/pakistan/unadj_constrained/warped/15+", pattern=".tif", full.names = TRUE)
over_15_rasters <- lapply(over_15_file_list, terra::rast)
over_15_sprc <- terra::sprc(over_15_rasters)
over_15_mosaic <- terra::mosaic(over_15_sprc, fun="sum")

##The baseline data that we will use for classification is overall population. 

combined <- terra::mosaic(under_15_mosaic, over_15_mosaic, fun="sum")

##QC 

###Check on joining individual .tif
as_tibble(under_15_mosaic) %>% summarise(total_0to14 = sum(pak_f_0_2020_constrained_UNadj_WARPED, na.rm=T))

as_tibble(over_15_mosaic) %>% summarise(total_15_plus = sum(pak_f_15_2020_constrained_UNadj_WARPED, na.rm=T))

as_tibble(combined) %>% summarise(total = sum(pak_f_0_2020_constrained_UNadj_WARPED, na.rm=T))

####Check if the sum of each age group is equal to the total: 
(as_tibble(under_15_mosaic) %>% summarise(total_0to14 = sum(pak_f_0_2020_constrained_UNadj_WARPED, na.rm=T)) ) + (as_tibble(over_15_mosaic) %>% summarise(total_15_plus = sum(pak_f_15_2020_constrained_UNadj_WARPED, na.rm=T)) ) == as_tibble(combined) %>% summarise(total = sum(pak_f_0_2020_constrained_UNadj_WARPED, na.rm=T))

###Set missing values in mosaics to 0
terra::set.values(under_15_mosaic, which(is.na(under_15_mosaic[])), 0)
terra::set.values(over_15_mosaic, which(is.na(over_15_mosaic[])), 0)
terra::set.values(combined, which(is.na(combined[])), 0)

###crs of mosaics 
terra::crs(under_15_mosaic, describe=TRUE)
terra::crs(over_15_mosaic, describe=TRUE)
terra::crs(combined, describe=TRUE)

###extents of the mosaics to ensure they are consistent 
terra::ext(under_15_mosaic)
terra::ext(over_15_mosaic)
terra::ext(combined)

# # download the GHSL data on a global scale
# download_GHSLdata(output_directory = "/home/davidpeng/Desktop/data-global", 
#                   products = c('BUILT_S', 'LAND'),
#                   filenames = c('BUILT_S.tif', 'LAND.tif'), 
#                   crs = 4326, 
#                   resolution = 30
#                   )

#Crop the global grids to the same geographical extent as our WorldPop population grid. Since we know they are the same for all 3 mosaic datasets, we can use any: 

# # create a directory to save the data
#dir.create('/home/davidpeng/Desktop/Food/data-pak')

#built-up grid
built_grid <- terra::crop(rast('/home/davidpeng/Desktop/Food/data-global/BUILT_S.tif'), 
                          terra::ext(under_15_mosaic), 
                          filename='/home/davidpeng/Desktop/Food/data-pakistan/BUILT_S.tif', overwrite=TRUE)

#land grid
land_grid <- terra::crop(rast('/home/davidpeng/Desktop/Food/data-global/LAND.tif'), 
                         terra::ext(under_15_mosaic), 
                         filename='/home/davidpeng/Desktop/Food/data-pakistan/LAND.tif', overwrite=TRUE)

#Resample population grids across population mosaics

under_15_mosaic <- terra::resample(under_15_mosaic, 
                                   built_grid, 
                                   method='sum')
over_15_mosaic <- terra::resample(over_15_mosaic, 
                                  built_grid, 
                                  method='sum')
combined <- terra::resample(combined, 
                            built_grid, 
                            method='sum')

## QC grids aligned
ext(under_15_mosaic) == ext(built_grid) &&  ext(under_15_mosaic) == ext(land_grid)
ext(over_15_mosaic) == ext(built_grid) &&  ext(over_15_mosaic) == ext(land_grid)
ext(combined) == ext(built_grid) &&  ext(combined) == ext(land_grid)

#Save pre-processed population grids. 
writeRaster(under_15_mosaic ,'/home/davidpeng/Desktop/Food/data-pakistan/POP_under_15.tif', overwrite=TRUE)
writeRaster(over_15_mosaic ,'/home/davidpeng/Desktop/Food/data-pakistan/POP_15_over.tif', overwrite=TRUE)
writeRaster(combined ,'/home/davidpeng/Desktop/Food/data-pakistan/POP_gen.tif', overwrite=TRUE)

# Use Flexurba package to apply DEGURBA, using the Worldpop data. More info here: https://flexurba-spatial-networks-lab-research-projects--e74426d1c66ecc.pages.gitlab.kuleuven.be/
combined_preprocessed <- preprocess_grid(
  directory = '/home/davidpeng/Desktop/Food/data-pakistan/', 
  filenames = c('BUILT_S.tif', 'POP_gen.tif', 'LAND.tif') #we use the pop_gen file here because we want DEGURBA for overall population
)

## Level 1 - Stage 1 (at the 1km grid cell level) 
### 0 corresponds to Water, 1 Rural, 2 Urban Cluster, 3 Urban Center 
un_degurba <- classify_grid(combined_preprocessed)

## Level 1 - Stage 2 (classfying small spatial units)
units_country <- units_country %>%
  rename(UID = GID_3) #UID convention for Flexurba 

### Preprocess data 
data1 <- preprocess_units(
  units = units_country,
  classification = un_degurba, #object from Level 1, Stage 1
  pop = "/home/davidpeng/Desktop/Food/data-pakistan/POP_gen.tif" #total population mosaic 
)

### Unit classification 
units_classification_country <- classify_units(data1)

### National summary
units_classification_country %>%
  group_by(flexurba_L1) %>%
  summarise(total_population = sum(Tot_Pop)) %>%
  mutate(proportion = total_population/sum(total_population))

### Resulting unit classifications based on DEGURBA when using Worldpop, joined to survey data 
country_degurba_labels <- interviews_with_units %>%
  select(Random_ID, GID_3) %>%
  left_join(
    units_classification_country %>%
      select(UID, flexurba_L1), by=c("GID_3" = "UID")
  )

fies_data <- fies_data %>%
  left_join(as_tibble(country_degurba_labels) %>%
              select(Random_ID, GID_3, flexurba_L1) %>%
              rename(DEGURBA = flexurba_L1), by = "Random_ID"
  ) 

fies_data <- fies_data %>%
  select(-GID_3.x, -DEGURBA.x) %>% #Dropping the GWP variables 
  rename(GID.3 = GID_3.y) %>%
  rename(DEGURBA = DEGURBA.y) 

# Estimation

## MOE Function (authored by Sara Viviani from the FAO FIES team). It has been edited to extract the MOE to use more cleanly in a tibble
### Since we do not have strata/PSU in the public facing FAO FIES data an assumption is made that the design effect is 2. 

moe=function(prob,rs,wt,sd=NULL,psu=NULL,strata=NULL,conf.level=0.9){
  library(survey)
  # Computes margins of error around the estimated prevalence, as the result of the
  # combination of sampling error and measurement error
  # prob = probability of being beyond the threshold for each case
  # rs = raw score for each case
  # wt = weight to be assigned to each case
  # psu = "cluster" variable (primary sampling unit)
  # strata (as there are strata with singleton PSU,need to use "survey.lonely.psu" options)
  # Compute the average probability for each rs
  n = length(prob)
  if (is.null(wt)) {wt = rep(1,n)}
  if (is.null(sd)) {sd = 1}
  if(is.null(strata) | sum(is.na(strata))==length(strata)) strata=rep(1,n)
  if(is.null(psu) | sum(is.na(psu))==length(psu)) psu=rep(1,n)
  options(survey.lonely.psu = "adjust")
  # Impute all missing in psu and strata
  if(sum(is.na((psu)))==length(psu)) psu=rep(1, length(prob))
  if(sum(is.na((strata)))==length(strata)) strata=rep(1, length(prob))
  psu[is.na(psu)]=rep(1, sum(is.na(psu)))
  strata[is.na(strata)]=rep(1, sum(is.na(strata)))
  if(sum(psu==1)==length(psu) & sum(strata==1)==length(strata)){
    svydesign=svydesign(id=~1, weights = ~ wt, lonely.psu=getOption("survey.lonely.psu"="adjust"),
                        data=data.frame(prob))
  } else{
    svydesign=svydesign(id=psu,weights = ~ wt,strata=strata,nest=T,lonely.psu=getOption("survey.lonely.psu"="adjust"),
                        data=data.frame(prob))
  }
  se_s = SE(svymean(~prob, svydesign,  deff = T,na.rm=T))*sd
  # if(length(unique(sort(prob)))!=length(unique(sort(rs)))){
  #   k=length(unique(sort(prob)))
  #   rs=rowSums(XX[,1:(k-1)])
  # }
  p = sort(unique(prob))%*%table(prob,rs)/colSums(table(prob,rs))
  
  ##as you can see, new weights are created here. Th
  wrs = NULL
  n1 = NULL
  for (i in sort(unique(rs))){
    wrs[i+1] = sum(wt[which(rs==i)])/sum(wt)*length(wt)
    n1 [i+1] = sum(rs==i)
  }
  
  wrs=wrs[!is.na(wrs)]
  n1=n1[!is.na(n1)]
  var_m = (p*(1-p)/n1)%*%(wrs/sum(wrs))^2
  se_m = sqrt(var_m)
  
  se = sqrt(se_s^2+se_m^2)
  moe = se*(-qnorm((1-conf.level)/2))
  # return(list(moe=moe, se_m=se_m,se_s=se_s)) I modify the return for ease of use in our analysis 
  return(moe[[1]])
}

## FIES Prevalence Estimates 
### These follow the guidance from the FAO FIES documentation, available once permission is granted in the FAO microdata portal: Derived_variables_and_Computation_indicator-6.pdf

### 15+ 
plus_15_country <- fies_data %>%
  filter(!is.na(Raw_score)) %>% 
  group_by(as_factor(DEGURBA)) %>% #our newly appended DEGURBA labels using Worldpop data 
  summarise(prevalence_mod_sev = (sum(Prob_Mod_Sev*wt,na.rm=T)) / (sum(wt,na.rm=T)),
            prevalence_mod_sev_moe = moe(prob = Prob_Mod_Sev, rs=Raw_score, wt=wt, sd=2, conf.level=.95),
            prevalence_sev = (sum(Prob_sev*wt,na.rm=T)) / (sum(wt,na.rm=T)),
            prevalence_sev_moe = moe(prob = Prob_sev, rs=Raw_score, wt=wt, sd=2, conf.level=.95)
  ) %>%
  ungroup() 

### 14 and under 

#### Step 1: Estimate "children weight" 

fies_data <- fies_data %>%
  mutate(N_adults_numeric = case_when(
    N_adults == 0 ~ 1,             # Convert households with "0" adults to 1. While imperfect, it assumes that there is at least one adult in the household -- the person responding to the survey
    N_adults == "10+" ~ 10,        # Cap at n_adult = 10 given labels used by FIES dataset 
    TRUE ~ as.numeric(N_adults)    # Convert to numeric
  ), 
  N_child_numeric = case_when(
    N_child == "10+" ~ 10,
    TRUE ~ as.numeric(N_child)     # Convert to numeric
  )
  ) %>%
  mutate(child_weight = (wt / N_adults_numeric  ) * N_child_numeric )

#### Step 2: Calculate weighted average using children weights 

under_14_country <- fies_data %>%
  filter(!is.na(child_weight)) %>% 
  filter(!is.na(Raw_score)) %>%
  group_by(as_factor(DEGURBA)) %>%
  summarise(prevalence_mod_sev = (sum(Prob_Mod_Sev*child_weight,na.rm=T)) / (sum(child_weight,na.rm=T)), 
            prevalence_mod_sev_moe = moe(prob = Prob_Mod_Sev, rs=Raw_score, wt=child_weight, sd=2, conf.level=.95),
            prevalence_sev = (sum(Prob_sev*child_weight,na.rm=T)) / (sum(child_weight,na.rm=T)),
            prevalence_sev_moe = moe(prob = Prob_sev, rs=Raw_score, wt=child_weight, sd=2, conf.level=.95)
  ) %>%
  ungroup() 

### Combine and clean in single summary table

prevalence_summary <- plus_15_country %>%
  rename(prevalence_mod_sev_15_and_over = prevalence_mod_sev, prevalence_sev_15_and_over = prevalence_sev) %>%
  mutate(prevalence_mod_sev_15_and_over_lb = prevalence_mod_sev_15_and_over - prevalence_mod_sev_moe, 
         prevalence_mod_sev_15_and_over_ub = prevalence_mod_sev_15_and_over + prevalence_mod_sev_moe,
         prevalence_sev_15_and_over_lb = prevalence_sev_15_and_over - prevalence_sev_moe, 
         prevalence_sev_15_and_over_ub = prevalence_sev_15_and_over + prevalence_sev_moe
  ) %>%
  select("as_factor(DEGURBA)", prevalence_mod_sev_15_and_over_lb, prevalence_mod_sev_15_and_over, prevalence_mod_sev_15_and_over_ub, prevalence_sev_15_and_over_lb, prevalence_sev_15_and_over, prevalence_sev_15_and_over_ub) %>%
  left_join(
    under_14_country %>%
      rename(prevalence_mod_sev_14_and_under = prevalence_mod_sev, prevalence_sev_14_and_under = prevalence_sev) %>%
      mutate(prevalence_mod_sev_14_and_under_lb = prevalence_mod_sev_14_and_under - prevalence_mod_sev_moe, 
             prevalence_mod_sev_14_and_under_ub = prevalence_mod_sev_14_and_under + prevalence_mod_sev_moe, 
             prevalence_sev_14_and_under_lb = prevalence_sev_14_and_under - prevalence_sev_moe, 
             prevalence_sev_14_and_under_ub = prevalence_sev_14_and_under + prevalence_sev_moe
      ) %>%
      select("as_factor(DEGURBA)", prevalence_mod_sev_14_and_under_lb, prevalence_mod_sev_14_and_under, prevalence_mod_sev_14_and_under_ub, prevalence_sev_14_and_under_lb, prevalence_sev_14_and_under, prevalence_sev_14_and_under_ub)
    , by=c("as_factor(DEGURBA)")
  ) %>%
  rename(DEGURBA = "as_factor(DEGURBA)") 

#### Convert data to long format for plotting, clean labels 

prevalence_summary_long <- prevalence_summary %>%
  pivot_longer(
    cols = starts_with("prevalence_"),
    names_to = c("severity", "age_group", "bound"),
    names_pattern = "prevalence_(mod_sev|sev)_(15_and_over|14_and_under)(_lb|_ub)?",
    values_to = "value"
  ) %>%
  mutate(
    severity = recode(severity, "mod_sev" = "Moderate + Severe", "sev" = "Severe"),
    bound = recode(bound, "_lb" = "lower", "_ub" = "upper", .default = "estimate")
  ) %>%
  mutate(DEGURBA = fct_recode(DEGURBA, "Rural areas" = "1", "Towns and semi-dense areas" = "2",  "Cities"="3"))

#### Plot prevalence estimates and confidence intervals 

ggplot(prevalence_summary_long %>% 
         filter(bound == "estimate"), aes(x = DEGURBA, y = value, color = severity)) +
  geom_point(size = 3, position = position_dodge(width = 0.8)) +
  geom_errorbar(
    data = prevalence_summary_long %>% 
      filter(bound %in% c("lower", "upper")),
    aes(ymin = ifelse(bound == "lower", value, NA),
        ymax = ifelse(bound == "upper", value, NA)),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  geom_text(
    aes(label = sprintf("%.2f", value)),  
    position = position_dodge(width = 0.8),
    vjust = -0.5,  
    size = 3,
    show.legend = FALSE  # 
  ) +
  facet_wrap(~ age_group) +
  scale_color_manual(values = c("Moderate + Severe" = "blue", "Severe" = "red")) +
  labs(
    x = "DEGURBA",
    y = "Prevalence",
    title = "Pakistan (PAK) - 2022 FIES Prevalence Estimates by DEGURBA, Severity, and Age Group",
    color = "Severity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95"))

## Extending to people estimates 

### Now that the small spatial units are classified through both stages of level 1, we need to use the Worldpop data by our two age groups to produce the population breakdown for the same
### polygons as defined by GADM. 

### However, from our testing, the boundaries of GADM do not always line up with those from Worldpop for units on the outermost border of a country. To ensure that the population figures we use 
### match those from Worldpop, and therefore the 2020 UN estimates, a buffering to the outermost boundaries is applied.

country_vect <- vect(units_country)

#### Step 1: Rasterize the original polygons
polygon_id_raster <- terra::rasterize(country_vect, under_15_mosaic, field = "UID", background = NA)

#### Step 2: Buffer the outermost boundary of the country
outermost_boundary <- terra::union(country_vect)  # Get the entire boundary of the country
##### Add the GID_X field to the buffered boundary
outermost_boundary$GID_3 <- "country_boundary"  # Assign a unique identifier for the entire country boundary

#### Step 3: Rasterize the buffered boundary to create a raster for the outer boundary
buffered_country <- terra::buffer(outermost_boundary, width = 10000)  # Buffer by 1000 meters
buffered_country_raster <- terra::rasterize(buffered_country, under_15_mosaic, field = "GID_3", background = NA)
#### Visual check of the buffered boundary raster
plot(buffered_country_raster)

#### Step 4: Update the polygon_id_raster with the buffered area (but only in the outer region)
polygon_id_raster[is.na(polygon_id_raster)] <- buffered_country_raster[is.na(polygon_id_raster)]

##### Step 5: Run zonal statistics to compute the sum per polygon GID_X
zonal_stats_under15 <- terra::zonal(under_15_mosaic, polygon_id_raster, fun = "sum", na.rm = TRUE)
zonal_stats_over15 <- terra::zonal(over_15_mosaic, polygon_id_raster, fun = "sum", na.rm = TRUE)

##### Step 6: Check the results
print(zonal_stats_under15)
print(zonal_stats_over15) 

##### Step 7: Worldpop totals as a comparison
terra::global(under_15_mosaic, "sum", na.rm = TRUE)[[1]]
terra::global(over_15_mosaic, "sum", na.rm = TRUE)[[1]]

##### Step 8: Rasterized zonal stats totals -- these should match the prior figures now
sum(zonal_stats_under15[, 2])  
sum(zonal_stats_over15[, 2]) 

### Summary tables
degurba_by_agegrp_buffered <- units_classification_country %>%
  select(UID, flexurba_L1) %>%
  left_join(
    zonal_stats_under15 %>%
      rename(under_15_population = pak_f_0_2020_constrained_UNadj_WARPED), by="UID" #note the name is carried over from the Worldpop files. Update for other countries. 
  ) %>%
  left_join(
    zonal_stats_over15 %>%
      rename(fifteen_plus_population = pak_f_15_2020_constrained_UNadj_WARPED), by="UID" #note the name is carried over from the Worldpop files. Update for other countries. 
  ) %>%
  group_by(flexurba_L1) %>%
  summarise(under_15_population = sum(under_15_population, na.rm=T), fifteen_plus_population = sum(fifteen_plus_population, na.rm=T)) %>%
  ungroup() %>%
  group_by(flexurba_L1) %>%
  mutate(gen_pop = sum(under_15_population, fifteen_plus_population)) %>%
  ungroup() %>%
  mutate(gen_pop_percent = gen_pop / sum(gen_pop)) %>%
  mutate(DEGURBA = case_when(
    flexurba_L1  == 1 ~ "Rural areas", 
    flexurba_L1  == 2 ~ "Towns and semi-dense areas",
    flexurba_L1  == 3 ~ "Cities")
  ) %>%
  filter(!is.na(DEGURBA))

### People estimates x DEGURBA x Age Groups 

people_estimates_by_agegrp <- prevalence_summary_long %>%
  mutate(
    people = case_when(
      DEGURBA == "Rural areas" & age_group == "15_and_over" ~ value * degurba_by_agegrp_buffered$fifteen_plus_population[degurba_by_agegrp_buffered$DEGURBA == "Rural areas"],
      DEGURBA == "Towns and semi-dense areas" & age_group == "15_and_over" ~ value * degurba_by_agegrp_buffered$fifteen_plus_population[degurba_by_agegrp_buffered$DEGURBA == "Towns and semi-dense areas"],
      DEGURBA == "Cities" & age_group == "15_and_over" ~ value * degurba_by_agegrp_buffered$fifteen_plus_population[degurba_by_agegrp_buffered$DEGURBA == "Cities"],
      DEGURBA == "Rural areas" & age_group == "14_and_under" ~ value * degurba_by_agegrp_buffered$under_15_population[degurba_by_agegrp_buffered$DEGURBA == "Rural areas"],
      DEGURBA == "Towns and semi-dense areas" & age_group == "14_and_under" ~ value * degurba_by_agegrp_buffered$under_15_population[degurba_by_agegrp_buffered$DEGURBA == "Towns and semi-dense areas"],
      DEGURBA == "Cities" & age_group == "14_and_under" ~ value * degurba_by_agegrp_buffered$under_15_population[degurba_by_agegrp_buffered$DEGURBA == "Cities"],
      TRUE ~ NA_real_
    )
  ) 

### People estimates x DEGURBA 

people_estimates_totalpopulation <- prevalence_summary_long %>%
  filter(bound=="estimate") %>%
  mutate(
    population = case_when(
      DEGURBA == "Rural areas" & age_group == "15_and_over" ~  degurba_by_agegrp_buffered$fifteen_plus_population[degurba_by_agegrp_buffered$DEGURBA == "Rural areas"],
      DEGURBA == "Towns and semi-dense areas" & age_group == "15_and_over" ~  degurba_by_agegrp_buffered$fifteen_plus_population[degurba_by_agegrp_buffered$DEGURBA == "Towns and semi-dense areas"],
      DEGURBA == "Cities" & age_group == "15_and_over" ~ degurba_by_agegrp_buffered$fifteen_plus_population[degurba_by_agegrp_buffered$DEGURBA == "Cities"],
      DEGURBA == "Rural areas" & age_group == "14_and_under" ~ degurba_by_agegrp_buffered$under_15_population[degurba_by_agegrp_buffered$DEGURBA == "Rural areas"],
      DEGURBA == "Towns and semi-dense areas" & age_group == "14_and_under" ~ degurba_by_agegrp_buffered$under_15_population[degurba_by_agegrp_buffered$DEGURBA == "Towns and semi-dense areas"],
      DEGURBA == "Cities" & age_group == "14_and_under" ~ degurba_by_agegrp_buffered$under_15_population[degurba_by_agegrp_buffered$DEGURBA == "Cities"],),
    MOE = case_when( #note that rural areas do not appear in our survey data through the DEGURBA application to 2022 FIES PAK, so they are commented out in this example: 
      #DEGURBA == "Rural areas" & age_group == "15_and_over" & severity == "Moderate + Severe" ~  plus_15_country$prevalence_mod_sev_moe[plus_15_country$`as_factor(DEGURBA)` == 1],
      DEGURBA == "Towns and semi-dense areas" & age_group == "15_and_over" & severity == "Moderate + Severe" ~  plus_15_country$prevalence_mod_sev_moe[plus_15_country$`as_factor(DEGURBA)` == 2],
      DEGURBA == "Cities" & age_group == "15_and_over" & severity == "Moderate + Severe" ~  plus_15_country$prevalence_mod_sev_moe[plus_15_country$`as_factor(DEGURBA)` == 3],
      #DEGURBA == "Rural areas" & age_group == "15_and_over" & severity == "Severe" ~  plus_15_country$prevalence_sev_moe[plus_15_country$`as_factor(DEGURBA)` == 1],
      DEGURBA == "Towns and semi-dense areas" & age_group == "15_and_over" & severity == "Severe" ~  plus_15_country$prevalence_sev_moe[plus_15_country$`as_factor(DEGURBA)` == 2],
      DEGURBA == "Cities" & age_group == "15_and_over" & severity == "Severe" ~  plus_15_country$prevalence_sev_moe[plus_15_country$`as_factor(DEGURBA)` == 3],
      #DEGURBA == "Rural areas" & age_group == "14_and_under" & severity == "Moderate + Severe" ~  under_14_country$prevalence_mod_sev_moe[under_14_country$`as_factor(DEGURBA)` == 1],
      DEGURBA == "Towns and semi-dense areas" & age_group == "14_and_under" & severity == "Moderate + Severe" ~  under_14_country$prevalence_mod_sev_moe[under_14_country$`as_factor(DEGURBA)` == 2],
      DEGURBA == "Cities" & age_group == "14_and_under" & severity == "Moderate + Severe" ~  under_14_country$prevalence_mod_sev_moe[under_14_country$`as_factor(DEGURBA)` == 3],
      #DEGURBA == "Rural areas" & age_group == "14_and_under" & severity == "Severe" ~  under_14_country$prevalence_sev_moe[under_14_country$`as_factor(DEGURBA)` == 1],
      DEGURBA == "Towns and semi-dense areas" & age_group == "14_and_under" & severity == "Severe" ~  under_14_country$prevalence_sev_moe[under_14_country$`as_factor(DEGURBA)` == 2],
      DEGURBA == "Cities" & age_group == "14_and_under" & severity == "Severe" ~  under_14_country$prevalence_sev_moe[under_14_country$`as_factor(DEGURBA)` == 3],
    ), 
   SE = MOE / 1.96, 
   variance = (SE*population)^2, 
   people_estimate = value * population
  ) %>%
  group_by(DEGURBA, severity) %>%
  summarise(people_estimate = sum(people_estimate), 
         MOE_total = 1.96 * sqrt(sum(variance))) %>%
  ungroup() %>%
  mutate(lower_bound = people_estimate - MOE_total, upper_bound = people_estimate + MOE_total) 

# INTERNAL 

## Export Tables 

### People estimates
write_csv(people_estimates_totalpopulation, "/home/davidpeng/Desktop/Food/results_for_ilab/pak/pak_people_estimates_totalpopulation.csv")
write_csv(people_estimates_by_agegrp, "/home/davidpeng/Desktop/Food/results_for_ilab/pak/pak_people_estimates_by_agegrp.csv")

### Population x DEGURBA 
write_csv(degurba_by_agegrp_buffered, "/home/davidpeng/Desktop/Food/results_for_ilab/pak/pak_degurba_population_by_agegrp.csv")

### DEGURBA x Small spatial units 
write_csv(units_classification_country, "/home/davidpeng/Desktop/Food/results_for_ilab/pak/pak_small_spatial_classified_DEGURBA.csv")