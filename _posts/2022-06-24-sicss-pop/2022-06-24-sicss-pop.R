#' ---
#' title: "SICSS-Covenant:Using gridded population to gain spatial insights in R"
#' description: |
#'   I have been invited to present at the Summer Institute for Computational Social Sciences in University of Covenant. My talks starts is about gridded population as a tool to link  demographics and geography. It starts with a global overview of the definition, production and use of this innovative data and pursues with a hands-on in R to gain spatial insights on demography in Nigeria.
#' author:
#'   - name: Edith Darin
#'     url: {}
#' date: 06-24-2022
#' output:
#'   distill::distill_article:
#'     self_contained: false
#'     includes:
#'       in_header: ../../wd/clicky.html
#' preview: './pic/final_map.png'
#' ---
#' 
#+ r setup, include=FALSE
data_path <- 'C:/Users/ecd1u18/Documents/SICSS-covenant-gridded-population/data'
knitr::opts_chunk$set(echo = TRUE)
#' 
#' > *Extract from the [github repo](https://github.com/wpgp/SICSS-covenant-gridded-population) that contains the session's material.*
#' 
#' #  Linking demographics and geography: Using gridded population to gain spatial insights in R
#' 
#' ##  Introduction
#' 
#' Access to **high-resolution population counts** is key for local, national and international decision-making and intervention. It supports data-driven planning of critical infrastructures, such as schools, health facilities and transportation networks.
#' 
#' **WorldPop** has developed modelling techniques to estimate population in grid cells of 100m by 100m by disaggregating census-based population totals for the entire world, leveraging the growing availability of products derived from satellite imagery. This level of detail offers the advantage of flexible aggregation of the population estimates within different administrative and functional units, for instance, school catchment areas and health zones.
#' 
#' This session will cover the notion of **gridded population**, a data format at the crossroad of demography and geography. We will then have a brief overview of openly available **satellite-imagery-based products** that can be used for modelling gridding population and beyond, such as settlement maps. Finally, we will have some hands-on to extract information from a gridded population covering the following R packages for geospatial analysis: `sf` [(Pebesma, E., 2018)](https://r-spatial.github.io/sf/), `raster` [(Hijmans, R., 2021)](https://cran.r-project.org/web/packages/raster/index.html), and `tmap` [(Tennekes, M., 2018)](https://r-tmap.github.io/tmap/index.html).
#' 
#' ###  Challenge
#' 
#' We will study the question: *How many women of childbearing age are struggling to access maternal health services?*
#' 
#' ![](./pic/final_map.png)
#' 
#' ###  Concepts
#' 
#' This tutorial covers the concepts of:
#' 
#' -   interactive mapping
#' 
#' -   vector file reading and filtering
#' 
#' -   raster file reading
#' 
#' -   buffering
#' 
#' -   rasterising
#' 
#' -   zonal statistics
#' 
#' -   masking
#' 
#' ###  Contents
#' On [github](https://github.com/wpgp/SICSS-covenant-gridded-population) is stored the raw material for this tutorial.
#' The script [`for_students.R`](https://github.com/wpgp/SICSS-covenant-gridded-population/blob/master/for_students.R) contains the workflow with the questions. The script [`teaching.R`](https://github.com/wpgp/SICSS-covenant-gridded-population/blob/master/teaching.R) contains the workflow with the answers. The powerpoint [`SICSS_20220624_griddedPop.pptx`](https://github.com/wpgp/SICSS-covenant-gridded-population/blob/master/SICSS_20220624_griddedPop.pptx) contains the presentation.
#' 
#' ###  Data used
#' 
#' For that purpose, we will need to access three data sources:
#' 
#' 1.  Population data from the Bottom-up gridded population estimates for Nigeria, version 2.0, produced jointly by WorldPop and the National Population Commission of Nigeria and accessible [here](https://wopr.worldpop.org/?NGA/Population/v2.0),
#' 
#' 2.  Health facilities locations produced by GRID3 Nigeria and accessible [here](https://data.grid3.org/datasets/GRID3::grid3-nigeria-health-care-facilities--1/about),
#' 
#' 3.  Local Government Area operational boundaries released by GRID3 Nigeria and accessible [here](https://data.grid3.org/datasets/GRID3::grid3-nigeria-local-government-area-boundaries/about)
#' On [github](https://github.com/wpgp/SICSS-covenant-gridded-population) the three data file are stored for this tutorial.
#' 
#' 
#'  ## Practical

library(tidyverse) #R library to manipulate table data
library(tmap) # R library to plot interactive maps
library(kableExtra) # R library for nice tables

tmap_mode('view') # set tmap as interactive
tmap_options(check.and.fix = TRUE) 


#' ### Defining the study area -------------------------------------------------
library(sf) # R library to manipulate spatial data

#' We load the vector file of administrative regions in Nigeria
lga <- st_read(
  paste0(
    data_path,
    '/GRID3_Nigeria_-_Local_Government_Area_Boundaries/GRID3_Nigeria_-_Local_Government_Area_Boundaries.shp'))

lga %>% 
      glimpse()

#' We plot using tmap interactive functions
tm_shape(lga)+
  tm_polygons()

#' We will focus our analysis in the local government area of Ado Odo/Ota
lga_Ado <- lga %>% 
  filter(lga_name_x=='Ado Odo/Ota')


tm_shape(lga_Ado)+
  tm_borders(col='orange', lwd=5)+
  tm_shape(lga)+
  tm_borders()+
  tm_basemap('OpenStreetMap')

#' ### Discovering the health facilities dataset -------------------------------


health_facilities <- st_read(
  paste0(
    data_path, 
    '/GRID3_Nigeria_-_Health_Care_Facilities_/GRID3_Nigeria_-_Health_Care_Facilities_.shp'))

health_facilities %>% 
  st_drop_geometry() %>% 
  glimpse()

health_facilities_Ado <- health_facilities %>% 
  filter(lga_name=='Ado Odo/Ota')

#' **EXERCISE: How many health facilities in Ado Odo/Ota?**
#' 
#' <details><summary>Click for the solution</summary>
  
dim(health_facilities_Ado)

#' </details>
#' We now represent the health facilities in Ado by facility types. 
#' We specify than on hoover the name  of the facility `primary_na` is shown
#' and on click the name, `primary_na`, the function status, `functional` and the source of the information, `source`.


tm_shape(health_facilities_Ado)+
  tm_dots(col='type', size=0.07, id='primary_na', 
          popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')+
  tm_shape(lga_Ado)+
  tm_borders(lwd=4)


#' **EXERCISE: How many health facilities are offering tertiary services in Ado Odo/Ota?**
#' 
#' <details><summary>Click for the solution</summary>
table(health_facilities_Ado$type)
#' </details>


#' ### Discovering the gridded population dataset ------------------------------

library(raster) # R library to manipulate raster data

pop <- raster(
  paste0(
    data_path, 
    '/NGA_population_v2_0_gridded/NGA_population_v2_0_gridded.tif'))

#' First spatial manipulation: cropping (or cutting a raster file to the extent of 
#' another spatial file, here the extent or bounding box of the Ado LGA)

pop_Ado <- crop(pop, lga_Ado)
plot(pop_Ado)


#' Second spatial manipulation: masking (or modifying the values of raster grid cells
#' designated by a second raster, here the values outside of the boundaries of the Ado LGA )

pop_Ado <- mask(pop_Ado, lga_Ado)
plot(pop_Ado)

#' And now we combine health facilities informaiton with gridded population

tm_shape(health_facilities_Ado)+
  tm_dots(col='type', size=0.07, id='primary_na', 
          popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')+
  tm_shape(pop_Ado)+
  tm_raster()+
  tm_shape(lga_Ado)+
  tm_borders(lwd=4)



#' ### Buffering points --------------------------------------------------------

#' We want to know how many people are living around each of the health facilities, that leads us to
#' our third spatial manipulation: buffering or drawing a circle of a giving radius around each point.

library(units) # R library to work with unit systems

health_facilities_Ado_buffered <- st_buffer(health_facilities_Ado, dist=set_units(1, km))

#' We visualise the first observation, that is about the *Ado Odo Ii Health Center*
health_facilities_Ado[1,]

tm_shape(health_facilities_Ado_buffered[1,])+
  tm_borders()+
  tm_shape(pop_Ado)+
  tm_raster()+
  tm_shape(health_facilities_Ado[1,])+
  tm_dots( size=0.08, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')



#' ### Computing the population ------------------------------------------------

#' We now want to compute the number of people living in 1 km of each facility by aggregating the related grid cells.
#' 
health_facilities_Ado_pop <- raster::extract(pop_Ado, health_facilities_Ado_buffered, 
                                             fun=sum, na.rm=T,df=T)

kbl(head(health_facilities_Ado_pop)) %>% 
  kable_minimal()

#' We see floating point number: this is due to gridded population that has floating point number in its grid cells, as a result
#' of a probabilistic model. It is the most likely value as an average of all the likely values of people count in the grid cell.
#' For more information: WorldPop. 2019. Bottom-up gridded population estimates for Nigeria, version 1.2. WorldPop, University of Southampton. doi:10.5258/SOTON/WP00655.

health_facilities_Ado_buffered$pop <- health_facilities_Ado_pop$NGA_population_v2_0_gridded

#' **EXERCISE:  How many people are living in 1km of Ado Odo Ii Health Center?**
#' 
#' <details><summary>Click for the solution</summary>
#' 
health_facilities_Ado_buffered[1,]
#' </details>


#' Let's look at the distribution of the number of people living at 1km of health facility in Ado.

summary(health_facilities_Ado_buffered$pop)
hist(health_facilities_Ado_buffered$pop, breaks=20)

tm_shape(health_facilities_Ado_buffered)+
  tm_fill('pop', style='pretty', id='pop')+
  tm_shape(health_facilities_Ado)+
  tm_dots( size=0.08, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')

#' We can highlight  in the less populated area
tm_shape(health_facilities_Ado_buffered %>% 
           filter(pop<1000))+
  tm_fill( id='pop', alpha=0.5, col='grey20')+
  tm_shape(pop_Ado)+
  tm_raster()+
  tm_basemap('OpenStreetMap')

#' ### How many people are not covered by health facilities? ------------------

#' We want now to compute the number of people that are not at 1km of a health facility
#' We first need to compute the total number of people at 1km from a health facility.
#' We can't just sum up the previous column as some people are at less than a 1km of several 
#' health facilities such that they will be counted several times.
#' 
#' We need a fourth spatial manipulation: rasterizing. We need to convert the buffers into a raster

health_facilities_Ado_buffered_rasterized <- rasterize(health_facilities_Ado_buffered,
                                                       pop_Ado, field=1)
plot(health_facilities_Ado_buffered_rasterized)

#' Then we mask the gridded population with the rasterized buffer, which gives us the
#' population covered by the health facilities
pop_Ado_masked <- mask(pop_Ado, health_facilities_Ado_buffered_rasterized)
plot(pop_Ado_masked)

sum(pop_Ado_masked[], na.rm=T)

#' **EXERCISE: How many people are not living in a 1km of an health facility?** 
#' For that purpose we need to compute the number of people living in Ado.
#' 
#' <details><summary>Click for the solution</summary>
#' first method
sum(pop_Ado[], na.rm=T) - sum(pop_Ado_masked[], na.rm=T)

#' second method
lga_Ado_pop <- raster::extract(pop, lga_Ado, fun=sum, na.rm=T,df=T)
sum(lga_Ado_pop)  - sum(pop_Ado_masked[], na.rm=T)

#' third method
lga_Ado$mean  - sum(pop_Ado_masked[], na.rm=T)
#' </details>


#' ### How many are not covered by a maternity home? ---------------------------
#' 
#' Let's focused on my health facility type: the **maternity homes**.

tm_shape(pop_Ado)+
  tm_raster()+
  tm_shape(health_facilities_Ado)+
  tm_dots( size=0.08, id='primary_na', 
           popup.vars=c('category','functional','source'))+
  tm_shape(health_facilities_Ado %>% 
             filter(category=='Maternity Home'))+
  tm_dots(col='darkgreen', size=0.08, id='primary_na', 
          popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')

#' **EXERCISE: How many maternity homes are listed in the LGA?** 
#' 
#' <details><summary>Click for the solution</summary>
health_facilities_Ado_buffered_maternity <- health_facilities_Ado_buffered %>% 
  filter(category=='Maternity Home')
nrow(health_facilities_Ado_buffered_maternity)
#' </details>

#' **EXERCISE:  How many people are not living in a 1km distance of a maternity center?**
#' 
#' <details><summary>Click for the solution</summary>
health_facilities_Ado_buffered_maternity_rasterized <- rasterize(
  health_facilities_Ado_buffered_maternity, pop_Ado, field=1
)
plot(health_facilities_Ado_buffered_maternity_rasterized)

pop_Ado_masked_maternity <- mask(pop_Ado, 
                                 health_facilities_Ado_buffered_maternity_rasterized)
plot(pop_Ado_masked_maternity)

sum(pop_Ado[], na.rm=T) - sum(pop_Ado_masked_maternity[], na.rm=T)
#' </details>



#' ### What is the furthest a woman has to travel to reach a maternity? --------

#' We first subset the maternity homes
health_facilities_Ado_maternity <- health_facilities_Ado %>% 
  filter(category=='Maternity Home')

#' Fifth spatial manipulation: We compute a gridded distance that quantifies the Euclidean 
#' distance from each of the grid cells to the nearest maternity
health_facilities_Ado_maternity_distance <- distanceFromPoints(pop_Ado, 
                                                               health_facilities_Ado_maternity)
plot(health_facilities_Ado_maternity_distance)

#' We then focused only on the grid cells that are populated

health_facilities_Ado_maternity_distance_pop <- mask(health_facilities_Ado_maternity_distance, 
                                                     pop_Ado)
plot(health_facilities_Ado_maternity_distance_pop)

#' We can now have a look at the distribution of the distance to maternity in the LGA.

summary(health_facilities_Ado_maternity_distance_pop[])

#' **EXERCISE:  What is the furthest people are leaving from a maternity in Ado?**
#' 
#' <details><summary>Click for the solution</summary>

max(health_facilities_Ado_maternity_distance_pop[], na.rm=T)
#' </details>


#' **EXERCISE: How many people are living at more than 8km from a maternity?** 
#' This exercise is based on all the previous analyses steps
#' 
#' <details><summary>Click for the solution</summary

#We compute a buffer of 8 km (rather than 1km)
health_facilities_Ado_maternity_buffered8km <- st_buffer(health_facilities_Ado_maternity,
                                                         dist=set_units(8, km))

#We subtract the population
health_facilities_Ado_maternity_buffered8km_rasterized <- rasterize(
  health_facilities_Ado_maternity_buffered8km, pop_Ado, field=1)
plot(health_facilities_Ado_maternity_buffered8km_rasterized)

pop_Ado_masked_maternity_8km <- mask(pop_Ado, 
                                     health_facilities_Ado_maternity_buffered8km_rasterized, 
                                     inverse=T)
plot(pop_Ado_masked_maternity_8km)
# We sum up the population
sum(pop_Ado_masked_maternity_8km[], na.rm=T)
#' </details>


#' 
#' ### And how many women of childbearing age? -------------------------------

#' Now we can further refine our analysis by focusing on the women of childbearing age.

women <- raster(
  paste0(
    data_path, 
    '/NGA_population_v2_0_agesex/NGA_population_v2_0_agesex_f15_49.tif'))

#' **EXERCISE: How many women of childbearing age are living at more than 8km from a maternity?**
#' 
#' <details><summary>Click for the solution</summary
women_Ado <- crop(women, lga_Ado)
women_Ado <- mask(women_Ado, lga_Ado)

women_Ado_masked_maternity_8km <- mask(women_Ado, 
                                       health_facilities_Ado_maternity_buffered8km_rasterized, 
                                       inverse=T)
sum(women_Ado_masked_maternity_8km[], na.rm=T)
#' </details>

#' ### What about the number of women of childbearing age living at more than 8km from a maternity in the country?
health_facilities_maternity <- health_facilities %>% 
  filter(category=='Maternity Home')
health_facilities_maternity_buffered8km <- st_buffer(health_facilities_maternity, dist=set_units(8, km))
health_facilities_maternity_buffered8km_rasterized <- rasterize(health_facilities_maternity_buffered8km, pop, field=1)
pop_masked_maternity_8km <- mask(pop, health_facilities_maternity_buffered8km_rasterized, inverse=T)

tm_shape(pop_masked_maternity_8km)+
  tm_raster()+
  tm_basemap('CartoDB.DarkMatter')

lga_pop_masked_maternity_8km <- raster::extract(pop_masked_maternity_8km, lga, fun=sum, na.rm=T,df=T)

lga$nonCovered_women <- lga_pop_masked_maternity_8km$NGA_population_v2_0_gridded
lga$nonCovered_women_perc <- round(lga$nonCovered_women / lga$mean * 100, 2)

tm_shape(lga)+
  tm_polygons(col='nonCovered_women_perc', id='nonCovered_women_perc', title='%women per LGA')+
  tm_shape(health_facilities_maternity)+
  tm_dots(size=0.1, legend.show = T)+
  tm_add_legend(type='symbol', labels='Maternity', col='black')+
  tm_layout( main.title = 'Women of childbearing age at more than 8km from a maternity')




#' ## Suggested citation
#'
#'Darin E, Tatem AJ. 2022. Linking demographics and geography: Using gridded population to gain spatial insights in R. Summer Institute of Computational Social Sciences, University of Covenant, Nigeria. <https://github.com/wpgp/SICSS-covenant-gridded-population>.
#'
#'![](./pic/sicss.png){width="10"}




