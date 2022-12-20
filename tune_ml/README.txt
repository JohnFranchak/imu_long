TUNE ML README

1) Run compile_data.R to gather all data with full motion features from across the study. The full-day data are saved to compiled_data.RData, the video recorded portion is recorded to compiled_data_lite.RData. The latter is all that is needed for model training/testing.

2) There are 3 different files that do various versions of model training
  - classification_multiple_mlr3.R uses the mlr3 package to do model fitting. it's possible it doesn't even run on this dataset, it's an old version from a prior study.
  - classification_randforest_group.R creates a simple group model using the regular randforest package and saves it. This becomes the group model that is used for making predictions across the dataset. Saves 'group_model.RData'.
  - classification_tidymodels.R has nice features for resampling and LOOCV, but currently it doesn't make a 'production' model. This one is good for assessment. Saves an .RData file with model fits and metrics.
  
3) classification_randforest_featuresets.R will compare accuracy after reducing the feature set in different ways.