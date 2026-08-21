###########################################
###########################################
# Figure S4 code
# This creates parity plots between observed and predicted UMAP dimensions
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
cols<-colnames(test)
cols<-cols[2:11]# remove first column system.index

# Filter columns
train<-train%>%select(all_of(cols))
test<-test%>%select(all_of(cols))

# Attribute to training or validation
train<-train%>%mutate(train_test='Train')
test<-test%>%mutate(train_test='Validation')

tt<-full_join(train,test)

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

# Split back into test and train

split<-split(data,data$train_test)

VALIDATION<-split$Validation
TRAIN<-split$Train

############################################################################
# Make figures
############################################################################
library(ggplot2)

plot_obs_pred <- function(df, target) {
  
  obs_col  <- target
  pred_col <- paste0(target, "_pred")
  
  # Compute metrics
  observed  <- df[[obs_col]]
  predicted <- df[[pred_col]]
  
  r2 <- cor(observed, predicted)^2
  rmse <- sqrt(mean((observed - predicted)^2))
  spearman <- cor(observed, predicted, method = "spearman")
  
  # Fit regression line
  fit <- lm(predicted ~ observed)
  slope <- coef(fit)[2]
  intercept <- coef(fit)[1]
  
  # Build plot
  ggplot(df, aes_string(x = obs_col, y = pred_col)) +
    geom_point(alpha = 0.5, color = "darkgray") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed",color='lightgrey') +  # 1:1 line
    geom_abline(intercept = intercept, slope = slope, color = "black", size = 1) +
    labs(
      title = paste0(
        target, "\n",
        "R² = ", round(r2, 2),
        ", RMSE = ", round(rmse, 2),
        ", Spearman = ", round(spearman, 2)
      ),
      x = "Observed",
      y = "Predicted"
    ) +
    theme_minimal(base_size = 14)
}

########################
# Test plot
plot_obs_pred(VALIDATION,'UMAP1')
plot_obs_pred(VALIDATION,'UMAP2')
plot_obs_pred(VALIDATION,'UMAP3')


##############################################################################
# With colors
##############################################################################
pal <- readRDS("my_palette.rds")

plot_obs_pred_cluster <- function(df, target, cluster_col, palette = NULL) {
  
  obs_col  <- target
  pred_col <- paste0(target, "_pred")
  
  # Compute metrics
  observed  <- df[[obs_col]]
  predicted <- df[[pred_col]]
  
  r2        <- cor(observed, predicted)^2
  rmse      <- sqrt(mean((observed - predicted)^2))
  spearman  <- cor(observed, predicted, method = "spearman")
  
  # Regression line
  fit       <- lm(predicted ~ observed)
  slope     <- coef(fit)[2]
  intercept <- coef(fit)[1]
  
  p <- ggplot(df, aes_string(x = obs_col, y = pred_col, color = cluster_col)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed",color='lightgrey') +
    geom_abline(intercept = intercept, slope = slope, color = "black", size = 1) +
    labs(
      title = target,
      subtitle= paste0(
        "R² = ", round(r2, 2),
        ", RMSE = ", round(rmse, 2),
        ", Spearman = ", round(spearman, 2)
      ),
      x = "Observed",
      y = "Predicted",
      color = 'Cluster'
    ) +
    theme_minimal(base_size = 14)
  
  # Apply palette if provided
  if (!is.null(palette)) {
    p <- p + scale_color_manual(values = palette)
  }
  
  p
}

UMAP1<-plot_obs_pred_cluster(VALIDATION,'UMAP1','cluster4',pal)
UMAP2<-plot_obs_pred_cluster(VALIDATION,'UMAP2','cluster4',pal)
UMAP3<-plot_obs_pred_cluster(VALIDATION,'UMAP3','cluster4',pal)


#########################################################################
# Patchwork
library(patchwork)

UMAP1/UMAP2/UMAP3+plot_annotation(tag_levels = 'A')+
  plot_layout(guides = 'collect')
