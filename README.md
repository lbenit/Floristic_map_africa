# Floristic\_map\_africa

Code for paper in revision to create a map of tree communities across southern Africa.



This repo uses a subset of the SEOSAW data that is publicly accessible, so it will not duplicate the results from the paper. I have commented where the code will not run because of differences between the subset and full dataset. All code has been generated using the subset, so it should run fine. The data intermediates are also in the Data folder, so you should be able to run the individual script without generating new files.



The main analysis was completed in colab with python and the google earth engine python API. R was used for some scripts and figures. Earth engine javascript was used to produce some geospatial layers that were saved as assets. These are commented on in the scripts.



**I recommend viewing the scripts by running them in Colab. Just click on the script in GitHub and there should be an 'Open in Colab' button at the top of the script.**











01\_Prep\_SEOSAW\_data

Clean the dataset and create a relative abundance matrix using basal area.





02\_UMAP

Use UMAP to reduce to three dimensions. The clustering is also completed in this script.





03\_Determine indicators

Uses indicspecies R package to identify indicator genera for the clusters.





04\_Extract\_predictors\_to\_plots

Extracts environmental variables to the plot locations for modelling.





05\_Combine\_UMAP\_plots\_w\_variables

This combines the data generated in 02 and 04 into a single dataset.





06\_Figure\_3

Generates figure 3





07\_Train\_test\_split

Splits the plot datasets into 70% training and 30% validation





08\_Cross\_validation\_4\_clusters

Cross validation using plots assigned to four clusters. Not in paper, but extra reference information.





09\_Random\_forest\_continuous

Random forest modelling with different variable combinations and assessment using linear regressions. Variable selection occurs here.





10\_Generate\_maps

Uses the earth engine python API to generate the maps in earth engine.





11\_Map\_visualization

This code can be used to interactively view the final maps reported in the paper.









Figures:

1. UMAP space with clusters- This was generated in R using the outputs from script 2. The script for this is in the Figures folder
2. Map of plots- This was generated in R using outputs from scripts 2 and 7.
3. Environmental predicters- Made using script 6
4. Final map- Made using QGIS. Interactive map is available by running script 11.



Supplemental figures

1. Optimal number of clusters- Generate in script 2
2. Map of plots divided into seven clusters- Generated using figure script 2
3. Categorical 4 cluster map- Generate in QGIS
4. Parity plots for UMAP dimensions- Generated in R using figure script S4
5. 3D validation using cosine similarity and distance- Generated in figure script S5

