###########################################
###########################################
# Figure 1 code
# Takes the UMAP assignments and plots them using ggplot
# The lineage information generate in 02_UMAP is used to create Fig 1D
###########################################
###########################################


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
train<-read.csv("training_data_embed_cluster_predicters.csv")
test<-read.csv("testing_data_embed_cluster_predicters.csv")

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


###################################################
# Plotting for figure 1
###################################################


a<-ggplot(data, aes(x = UMAP1, y = UMAP2, color = cluster4)) +
  geom_point(size = 1.5, alpha = 0.8) +
  scale_color_manual(values = pal)+
  theme_bw() +
  labs(
    x = "UMAP 1",
    y = "UMAP 2",
    color = "Clusters"   # legend title
  ) + geom_text_repel(
    data = gen,
    aes(x = UMAP1, y = UMAP2, label = genus),
    inherit.aes = FALSE,
    max.overlaps = Inf, size = 5,
    force = 0.5,        # default is 1
    force_pull = 
  )+
  theme(
    legend.position =  c(0.8,0.8),   # move legend if needed
    axis.title = element_text(size = 30),                  # axis titles
    axis.text = element_text(size = 30),                   # axis tick labels
    legend.text = element_text(size = 20),                 # legend labels
    legend.title = element_text(size = 30)  
  )+ guides(
    color = guide_legend(override.aes = list(size = 6)),
    shape = guide_legend(override.aes = list(size = 6)))

a

ggplot(data, aes(x = UMAP1, y = UMAP3, color = cluster7))+
  geom_point(size = 1.5, alpha = 0.8) +
  scale_color_manual(values = pal)+
  guides(color = guide_legend(override.aes = list(size = 8))) +
  theme_bw() +
  labs(
    x = "UMAP 1",
    y = "UMAP 3",
    color = "Clusters"   # legend title
  ) +
  theme(
    #legend.position = c(-5,15),   # move legend if needed
    axis.title = element_text(size = 30),                  # axis titles
    axis.text = element_text(size = 30),                   # axis tick labels
    legend.text = element_text(size = 20),                 # legend labels
    legend.title = element_text(size = 30)  ,
    legend.key.size = unit(3, "cm"))


################################################
###############################################

b<-ggplot(data, aes(x = UMAP1, y = UMAP2, color = cluster7)) +
  geom_point(size = 1.5, alpha = 0.8) +
  
  scale_color_manual(values = pal)+
  theme_bw() +
  labs(
    x = "UMAP 1",
    y = "UMAP 2",
    color = "Clusters"   # legend title
  ) +
  theme(
    legend.position = "right",   # move legend if needed
    axis.title = element_text(size = 30),                  # axis titles
    axis.text = element_text(size = 30),                   # axis tick labels
    legend.text = element_text(size = 20),                 # legend labels
    legend.title = element_text(size = 30),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )+ guides(
    color = guide_legend(override.aes = list(size = 6)),
    shape = guide_legend(override.aes = list(size = 6)))
####

c<-ggplot(data, aes(x = UMAP1, y = UMAP3, color = cluster7))+
  geom_point(size = 1.5, alpha = 0.8) +
  scale_color_manual(values = pal)+
  theme_bw() +
  labs(
    x = "UMAP 1",
    y = "UMAP 3",
    color = "Clusters"   # legend title
  ) +
  theme(
    legend.position = "none",   # move legend if needed
    axis.title = element_text(size = 30),                  # axis titles
    axis.text = element_text(size = 30))              # axis tick labels



############################
# Creat figure 1D flow diagram
flow <- data %>%
  select(plot_id, cluster3, cluster4, cluster5, cluster6, cluster7) %>%
  pivot_longer(
    cols = starts_with("cluster"),
    names_to = "level",
    values_to = "cluster"
  )


flow <- flow %>%
  group_by(level, cluster) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  arrange(level, desc(n), cluster)


flow_plot<-ggplot(flow,
                  aes(x = level, stratum = cluster, alluvium = plot_id, fill = cluster)) +
  geom_flow(alpha = 0.4, width = 0) +
  geom_stratum(width=0.5) +
  geom_fit_text(
    stat = "stratum",
    aes(label = cluster),
    #angle = 90,
    min.size = 3,      # smallest allowed
    grow = FALSE,       # expand text until it fits
    reflow = TRUE,      # wrap if needed
    color='black',
    fill='white'
  ) +
  scale_fill_manual(values = pal) +
  theme_minimal()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 30),                  # axis titles
        axis.text = element_text(size = 20))+
  labs(x='Number of clusters',
       y='Number of plots')+ 
  scale_x_discrete(labels = function(x) gsub("cluster", "", x))

#############################################################################
# Patchwork the plots into the final figure
layout<-"AAA#BB
        AAA#BB
        AAA#BB
        AAA#CC
        AAA#CC
        AAA#CC
        DDDDDD
        DDDDDD
"
a+b+c+flow_plot+
  plot_layout(design = layout)



ggsave("umap_plot_flow.svg", width = 25, height = 18, units = "in",dpi=300)
