#########################################################################################
# Plot out the location of plots with their cluster assignments
#Used to generate figures 2 and S2
#########################################################################################
library(dplyr)
library(terra)
library(terrainr)
library(tidyterra)
library(sf)
library(sp)
library(maps)
library(ggspatial)
library(ggnewscale)
library(scales)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggplot2)
library(patchwork)
library(viridis)
library(tidyr)
library(ggalluvial)
library(ggfittext)

setwd("//groups.geos.ed.ac.uk/landteam/people/lorena_benitez/UMAP_maps")

# Load data
data<-read.csv('all_data_umap_clusters_parametric.csv')
locations<-read.csv('selected_seosaw_plots.csv')
train<-read.csv("training_data_embed_cluster_predicters_16_3.csv")
test<-read.csv("testing_data_embed_cluster_predicters_16_3.csv")

genera<-read.csv('UMAP_genus_scores_all_points.csv')

# Join together
data<-left_join(data,locations, by='plot_id')

# Attribute to training or testing
train<-train%>%select(plot_id)%>%mutate(train_test='Train')
test<-test %>%select(plot_id)%>%mutate(train_test='Validation')

tt<-rbind(train,test)

# Join with other data
data<-data%>%left_join(tt,by='plot_id')


###################################################
# Select genera

gen<-genera%>%filter(genus%in%c('Brachystegia','Julbernardia',
                                'Cryptosepalum','Baikiaea', 'Androstachys',
                                'Colophospermum','Combretum','Uapaca','Parinari',
                                'Terminalia','Vachellia','Guibourtia',
                                'Commiphora', 'Diplorhynchus','Pterocarpus',
                                'Burkea'))




############################################
# Replace with names
data <- data %>%
  mutate(cluster7 = recode(cluster7,
                           `0` = 'Mopane',
                           `1` = 'Cryptosepalum',
                           `2` = 'Baikiaea',
                           `3` = 'Combretum',
                           `4` = 'Uapaca',
                           `5` = 'Androstachys',
                           `6` = 'Julbernardia'
  ))

data <- data %>%
  mutate(cluster6 = recode(cluster6,
                           `0` = 'Julbernardia',
                           `1` = 'Mopane',
                           `2` = 'Baikiaea',
                           `3` = 'Combretum',
                           `4` = 'Cryptosepalum',
                           `5` = 'Androstachys'
  ))

data <- data %>%
  mutate(cluster5 = recode(cluster5,
                           `0` = 'Savanna',
                           `1` = 'Julbernardia',
                           `2` = 'Androstachys',
                           `3` = 'Mopane',
                           `4` = 'Cryptosepalum'
  ))


data <- data %>%
  mutate(cluster4 = recode(cluster4,
                           `0` = 'Miombo',
                           `1` = 'Savanna',
                           `2` = 'Androstachys',
                           `3` = 'Mopane'
                           
  ))
data <- data %>%
  mutate(cluster3 = recode(cluster3,
                           `0` = 'Other',
                           `1` = 'Mopane',
                           `2` = 'Androstachys'
                           
  ))


# Make sure that they are factors
data$cluster7<-as.factor(data$cluster7)
data$cluster6<-as.factor(data$cluster6)
data$cluster5<-as.factor(data$cluster5)
data$cluster4<-as.factor(data$cluster4)
data$cluster3<-as.factor(data$cluster3)


#######################################################################
# Make cluster colors consistent
all_clusters <- unique(c(data$cluster3,data$cluster4,
                         data$cluster5,
                         data$cluster6, data$cluster7))

pal <- viridisLite::turbo(length(all_clusters), direction=-1)
names(pal) <- all_clusters

# Save pallete
saveRDS(pal, "my_palette.rds")

##############################################################################
####
# Get Africa
africa <- map_data("world", region = c("Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi",
                                       "Cameroon","Central African Republic","Chad",
                                       "Democratic Republic of the Congo","Republic of Congo","Ivory Coast",
                                       "Djibouti","Egypt","Equatorial Guinea","Eritrea","Swaziland","Ethiopia",
                                       "Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia",
                                       "Libya","Malawi","Mali","Mauritania","Morocco",
                                       "Mozambique","Namibia","Niger","Nigeria","Rwanda",
                                       "Senegal","Sierra Leone","Somalia","South Africa","South Sudan",
                                       "Sudan","Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe", 'Western Sahara'))%>%filter(is.na(subregion))
# Get AOI
region <- map_data("world", 
                   region = c("Angola","Botswana",
                              "Democratic Republic of the Congo",
                              "Malawi", 
                              "Mozambique","Namibia",
                              "Tanzania",
                              "Zambia","Zimbabwe"))



lgregion <- map_data("world", 
                     region = c("Angola","Botswana","Burundi",
                                "Democratic Republic of the Congo","Republic of Congo",
                                "Swaziland",
                                "Kenya","Lesotho",
                                "Malawi",
                                "Mozambique","Namibia","Rwanda",
                                "South Africa",
                                "Tanzania","Uganda",
                                "Zambia","Zimbabwe"))%>%filter(is.na(subregion))


# Instead, rebuild polygons using st_polygonize or st_cast after grouping.

region_sf <- region %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%   # convert to sf first
  group_by(region, group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

region_lg <- lgregion %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%   # convert to sf first
  group_by(region, group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

africa<- africa %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%   # convert to sf first
  group_by(region, group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# Step 3: Convert sf to SpatVector
aoi <-  vect(region_sf)


lg <-  vect(region_lg)

africa<-vect(africa)

############
# Save vector files
writeVector(aoi, file='area_of_interest.shp')
writeVector(lg, file='area_of_interest_neighbors.shp')

##########################################################################
# Load in country boundaries
aoi<-vect('area_of_interest.shp')
lg<-vect('area_of_interest_neighbors.shp')
mask<-rast('landscape_1km.tif')


##########################
# Make plots into spatvectors
# Create SpatVector from data frame
all <- vect(data, geom = c("longitude_of_centre", "latitude_of_centre"), crs = "EPSG:4326")

############################################################################
# Plot inset
inset<-ggplot()+  geom_spatvector(data=africa,
                                  fill=NA,color='black')+
  geom_spatvector(data=aoi,
                  fill='black',color='white')+ theme_void() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
  )+theme(plot.margin = margin(0,0,0,0))

# Plot masked area
mask_area<-ggplot()+
  geom_raster(data=mask,aes(x, y, fill = factor(wetmonths_era))) +
  scale_fill_manual(
    values = c(
      "0" = NA,        # transparent
      "1" = "grey60"   # grey
    ),
    na.translate = FALSE
  ) +
  coord_equal()+geom_spatvector(data=lg, fill=NA,color='black')+
  #geom_spatvector(data=all, alpha=0.8,size=2,color='black',fill='black')+
  theme_bw()+theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = 'none')+
  annotation_scale(location = "br", width_hint = 0.3,  pad_y = unit(0.5, "cm" ),text_cex=2 )+labs(color = "")+
  theme(annotation_north_arrow(which_north = "grid", location='tr'))

mask_area


# PLt test train
map_tt<-ggplot()+  geom_spatvector(data=lg,
                                   fill=NA,color='black')+
  geom_spatvector(data=all, alpha=0.8,size=2, aes(color=train_test))+
  theme_bw()+theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 16))+
  annotation_scale(location = "br", width_hint = 0.3,  pad_y = unit(0.5, "cm" ),text_cex=2 )+labs(
    color = ""   # legend title
  )+theme(legend.position=c(0.1,0.95))+  annotation_north_arrow(which_north = "grid", location='tr') + 
  theme(plot.margin = margin(0,0,0,0))



#Facet
ggplot() +
  geom_spatvector(data = lg,
                  fill = NA, color = 'black',
                  inherit.aes = FALSE) +
  geom_spatvector(data = all,
                  alpha = 0.5,
                  aes(color = cluster7)) +
  scale_color_manual(values = pal)+
  theme_bw() +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank()
  ) +
  facet_wrap(~ cluster7, nrow=4,ncol=2)

# Facet cluster 4
facet_4<-ggplot() +
  geom_spatvector(data = lg,
                  fill = NA, color = 'black',
                  inherit.aes = FALSE) +
  geom_spatvector(data = all,
                  alpha = 0.5,
                  aes(color = cluster4)) +
  scale_color_manual(values = pal)+
  theme_bw() +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank()
  ) +
  facet_wrap(~ cluster4, nrow=2,ncol=2)+theme(strip.text = element_text(size = 20))

facet_4


#########################################################################
# Patchwork maps together

library(patchwork)

# Add inset to map
big<-map_tt +
  inset_element(
    inset,
    left = 0.75, right = 0.98,
    bottom = 0, top = 0.3
  )

big+facet_4