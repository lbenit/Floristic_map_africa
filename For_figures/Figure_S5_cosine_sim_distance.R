##########################################
###########################################
# Figure S5 code
# This is used to evaluate the maps by comparing the observed and predicted location of plots in UMAp space
# The predicted values are extracted from the final map
###########################################
###########################################

library(dplyr)

# Load data
# Not accessible in this repo
data<-read.csv('selected_seosaw_plots.csv')
train<-read.csv("train_extracted_100m.csv")
test<-read.csv("test_extracted_100m.csv")

# Get columns
cols<-colnames(train)
cols<-cols[2:11]# remove first column system.index

# Filter columns
train<-train%>%select(all_of(cols))
test<-test%>%select(all_of(cols))

# Attribute to training or validation
train<-train%>%mutate(train_test='Train')
test<-test%>%mutate(train_test='Validation')

tt<-full_join(train,VALIDATION)

# Join with other data
data<-data%>%left_join(tt,by='plot_id')


############################################
# Replace cluster codes with names
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
  mutate(cluster4 = recode(cluster4,
                           `0` = 'Miombo',
                           `1` = 'Savanna',
                           `2` = 'Androstachys',
                           `3` = 'Mopane'
                           
  ))



# Make sure that they are factors
data$cluster7<-as.factor(data$cluster7)
data$cluster4<-as.factor(data$cluster4)
data$cluster<-as.factor(data$cluster)

# Split back into VALIDATION and train

split<-split(data,data$train_test)

VALIDATION<-split$Validation
TRAIN<-split$Train



#############################################################################
# Calculate cosine similarity and distance
cosine_similarity <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}

VALIDATION<- VALIDATION %>%
  rowwise() %>%
  mutate(
    cosine_sim = cosine_similarity(
      c(UMAP1, UMAP2, UMAP3),
      c(UMAP1_pred, UMAP2_pred, UMAP3_pred)
    )
  ) %>%
  ungroup()


hist(VALIDATION$cosine_sim)
mean(VALIDATION$cosine_sim)
median(VALIDATION$cosine_sim)
sd(VALIDATION$cosine_sim)
sd(VALIDATION$cosine_sim)/length(VALIDATION$cosine_sim)
IQR(VALIDATION$cosine_sim)


# Euclidian distance

euclid_dist <- function(a, b) {
  sqrt(sum((a - b)^2))
}

VALIDATION <- VALIDATION %>%
  rowwise() %>%
  mutate(
    euclid = euclid_dist(
      c(UMAP1, UMAP2, UMAP3),
      c(UMAP1_pred, UMAP2_pred, UMAP3_pred)
    )
  ) %>%
  ungroup()

hist(VALIDATION$euclid)
mean(VALIDATION$euclid)
median(VALIDATION$euclid)
sd(VALIDATION$euclid)
sd(VALIDATION$euclid)/length(VALIDATION$euclid)
IQR(VALIDATION$euclid)

####################
# Plot VALIDATION plots in UMAP space
library(ggplot2)



#####################################################
# PLot in UMAp space with colors

cs<-ggplot(VALIDATION) +
  geom_point(
    aes(x = UMAP1, y = UMAP2),
    colour = "black",
    alpha = 0.8,
    size = 2
  ) +
  geom_point(
    aes(x = UMAP1_pred, y = UMAP2_pred, colour = cosine_sim),
    alpha = 0.5,
    size = 2
  ) +scale_colour_gradientn(
    colours = viridisLite::plasma(100),
    values = scales::rescale(c(-1, 0, 0.5, 0.7, 0.85, 1))
  )+
  labs(
    x = "UMAP 1",
    y = "UMAP 2",
    colour = "Cosine similarity",
    title = "A"
  ) +
  theme_minimal()+
  theme(title=element_text(size=40),
        axis.title = element_text(size = 30),                  # axis titles
        axis.text = element_text(size = 30),                   # axis tick labels
        legend.text = element_text(size = 20),                 # legend labels
        legend.title = element_text(size = 20))


dis<-ggplot(VALIDATION) +
  geom_point(
    aes(x = UMAP1, y = UMAP2),
    colour = "black",
    alpha = 0.8,
    size = 2
  ) +
  geom_point(
    aes(x = UMAP1_pred, y = UMAP2_pred, colour = euclid),
    alpha = 0.5,
    size = 2
  ) +scale_colour_viridis_c(option = "viridis") +
  labs(
    x = "UMAP 1",
    y = "UMAP 2",
    colour = "Distance",
    title = "C"
  ) +
  theme_minimal()+
  theme(title=element_text(size=40),
        axis.title = element_text(size = 30),                  # axis titles
        axis.text = element_text(size = 30),                   # axis tick labels
        legend.text = element_text(size = 20),                 # legend labels
        legend.title = element_text(size = 20))


###############################################################
# Histograms
cs_hist<-ggplot(VALIDATION, aes(x = cosine_sim)) +
  geom_histogram(bins = 30, alpha = 0.8) +
  geom_vline(
    aes(xintercept = mean(cosine_sim, na.rm = TRUE)),
    colour = "black",
    linewidth = 1
  ) +
  geom_vline(
    aes(xintercept = median(cosine_sim, na.rm = TRUE)),
    colour = "black",
    linewidth = 1,
    linetype = "dashed"
  ) +
  labs(
    x = "Cosine similarity",
    y = "Count",
    fill = "Cosine similarity",
    title='B'
  ) +
  theme_minimal()+
  theme(title=element_text(size=40),
        axis.title = element_text(size = 30),                  # axis titles
        axis.text = element_text(size = 30),                   # axis tick labels
        legend.text = element_text(size = 20),                 # legend labels
        legend.title = element_text(size = 30))


dis_hist<-ggplot(VALIDATION, aes(x = euclid)) +
  geom_histogram(bins = 30, alpha = 0.8) +
  geom_vline(
    aes(xintercept = mean(euclid, na.rm = TRUE)),
    colour = "black",
    linewidth = 1
  ) +
  geom_vline(
    aes(xintercept = median(euclid, na.rm = TRUE)),
    colour = "black",
    linewidth = 1,
    linetype = "dashed"
  ) +
  labs(
    x = "Distance",
    y = "Count",
    title='D'
  ) +
  theme_minimal()+
  theme(title=element_text(size=40),
        axis.title = element_text(size = 30),                  # axis titles
        axis.text = element_text(size = 30),                   # axis tick labels
        legend.text = element_text(size = 20),                 # legend labels
        legend.title = element_text(size = 30))
dis_hist

##################################
# Make final figure
library(patchwork)

layout<-"AAABB
        CCCDD"
cs+cs_hist+dis+dis_hist+
  plot_layout(design = layout)

