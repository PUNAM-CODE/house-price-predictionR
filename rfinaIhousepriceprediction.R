################### house price prediction###############
install.packages()
set.seed(100)  #Setting Seed to 100
rm(list = ls())  # Removing all current Object
x <- c('data.table','dplyr','mice','h2o') 
lapply(x,require,character.only=TRUE) # loading packages
install.packages(deparse(substitute(mice)), repos = "http://cran.us.r-project.org")
install.packages("h2o", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-jacobi/2/R", getOption("repos"))))
memory.size(max = TRUE) # Setting memory to max
options(scipen = 99,digits = 10) 
install.packages("MASS")
library(MASS)
# Initiallizing H2o with 2 threads and 10 gigs of memory

h2o.init(nthreads = 2,max_mem_size = "10g")
df_test <- read_csv("C:/Users/admin/Desktop/data science/project/house price predict advanse/test.csv")

df_train <- read_csv("C:/Users/admin/Desktop/data science/project/house price predict advanse/train.csv")

# Checking For Missing Patterns
md.pattern(df_train)
md.pattern(df_test)
install.packages('h2o',type='source')

# Dropping outliers based on Tableau
df_train <- df_train[-c(524,1299),]
View(df_train)
# Converting character columns to factors
df_train <- df_train %>% mutate_if(is.character,as.factor)

# Renaming those columns that start with NUMBER. If not renamed, MICE will give error
df_train <- df_train %>% rename('firstflrSF'='1stFlrSF','secondflrSF'='2ndFlrSF','third_SsnPorch'='3SsnPorch')
df_test <- df_test %>% rename('firstflrSF'='1stFlrSF','secondflrSF'='2ndFlrSF','third_SsnPorch'='3SsnPorch')

# MICE imputation on train using
parse(text="rRemodAdd+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrType+MasVnrArea+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinSF1+BsmtFinType2+BsmtFinSF2+BsmtUnf")
#parse(text=gsub('-','_',str))
mice_imputes = mice(df_train, m=1, maxit = 2,method='cart')
temp <- complete(mice_imputes,1)
df_train <- temp


# Feature engineering for TRAIN ONLY
df_train$SalePrice <- log(df_train$SalePrice)
df_train$house_age <- 2020-df_train$YearBuilt
df_train$lg_house_age <- log(df_train$house_age)
df_train$MSSubClass <- as.factor(df_train$MSSubClass)
df_train$`totFlrSF` <- df_train$`firstflrSF`+df_train$`secondflrSF`
df_train$totbath <- df_train$FullBath+df_train$HalfBath+df_train$BsmtHalfBath+df_train$BsmtFullBath
df_train$tot_ar <- df_train$GrLivArea + df_train$TotalBsmtSF+df_train$totFlrSF
df_train$tot_porch <- df_train$OpenPorchSF + df_train$EnclosedPorch + df_train$third_SsnPorch + df_train$ScreenPorch
df_train$`lg_totFlrSF` <- log(df_train$`firstflrSF`+df_train$`secondflrSF`)
df_train$lg_totbath <- log(df_train$FullBath+df_train$HalfBath+df_train$BsmtHalfBath+df_train$BsmtFullBath)
df_train$lg_tot_ar <- log(df_train$GrLivArea + df_train$TotalBsmtSF+df_train$totFlrSF)
df_train$lg_tot_porch <- log(df_train$OpenPorchSF + df_train$EnclosedPorch + df_train$third_SsnPorch + df_train$ScreenPorch)
df_train$Remod <- as.factor(ifelse(df_train$YearBuilt==df_train$YearRemodAdd, 0, 1))
df_train$Age <- as.numeric(df_train$YrSold)-df_train$YearRemodAdd
df_train$lg_Age <- log(df_train$Age)
df_train$lg_MiscVal <- log(df_train$MiscVal+1)
df_train$lg_MasVnrArea <- log(df_train$MasVnrArea+1)
df_train$lg_LotArea <- log(df_train$LotArea+1)
df_train$lg_LotFrontage <- log(df_train$LotFrontage+1)
df_train$lg_ltarea_front <- log((df_train$LotArea+1)*(df_train$LotFrontage+1))
df_train$lg_BsmtFinSF1 <- log(df_train$BsmtFinSF1+1)
df_train$lg_BsmtUnfSF <- log(df_train$BsmtUnfSF+1)
df_train$lg_TotRmsAbvGrd <- log(df_train$TotRmsAbvGrd+1)
df_train$lg_WoodDeckSF <- log(df_train$WoodDeckSF+1)
df_train$lg_OpenPorchSF <- log(df_train$OpenPorchSF+1)

# Feature engineering for TRAIN ONLY
df_test$house_age <- 2020-df_test$YearBuilt
df_test$MSSubClass <- as.factor(df_test$MSSubClass)
df_test$`totFlrSF` <- df_test$`firstflrSF`+df_test$`secondflrSF`
df_test$totbath <- df_test$FullBath+df_test$HalfBath+df_test$BsmtHalfBath+df_test$BsmtFullBath
df_test$tot_ar <- df_test$GrLivArea + df_test$TotalBsmtSF+df_test$`totFlrSF`
df_test$tot_porch <- df_test$OpenPorchSF + df_test$EnclosedPorch + df_test$third_SsnPorch + df_test$ScreenPorch
df_test$`lg_totFlrSF` <- log(df_test$`firstflrSF`+df_test$`secondflrSF`)
df_test$lg_totbath <- log(df_test$FullBath+df_test$HalfBath+df_test$BsmtHalfBath+df_test$BsmtFullBath)
df_test$lg_tot_ar <- log(df_test$GrLivArea + df_test$TotalBsmtSF+df_test$`totFlrSF`)
df_test$lg_tot_porch <- log(df_test$OpenPorchSF + df_test$EnclosedPorch + df_test$third_SsnPorch + df_test$ScreenPorch)
df_test$Remod <- as.factor(ifelse(df_test$YearBuilt==df_test$YearRemodAdd, 0, 1))
df_test$Age <- as.numeric(df_test$YrSold)-df_test$YearRemodAdd
df_test$lg_house_age <- log(df_test$house_age)
df_test$lg_Age <- log(df_test$Age)  
df_test$lg_MiscVal <- log(df_test$MiscVal+1)
df_test$lg_MasVnrArea <- log(df_test$MasVnrArea+1)
df_test$lg_LotArea <- log(df_test$LotArea+1)
df_test$lg_LotFrontage <- log(df_test$LotFrontage+1)
df_test$lg_ltarea_front <- log((df_test$LotArea+1)*(df_test$LotFrontage+1))
df_test$lg_BsmtFinSF1 <- log(df_test$BsmtFinSF1+1)
df_test$lg_BsmtUnfSF <- log(df_test$BsmtUnfSF+1)
df_test$lg_TotRmsAbvGrd <- log(df_test$TotRmsAbvGrd+1)
df_test$lg_WoodDeckSF <- log(df_test$WoodDeckSF+1)
df_test$lg_OpenPorchSF <- log(df_test$OpenPorchSF+1)

# Converting again character to factors again to be double sure
df_train <- df_train %>% mutate_if(is.character,as.factor)
df_test <- df_test %>% mutate_if(is.character,as.factor)

# Taking Back up
saveRDS(df_train,"final_df_train.rds")
saveRDS(df_test,"final_df_test.rds")

# Loading data frame to H2o frame
df_train_h20 <- as.h2o(as.data.frame(df_train))
df_test_h20 <- as.h2o(as.data.frame(df_test))

# Spliting the the dataset in 80 TRAIN - 20 VALIDATION
df.splits <- h2o.splitFrame(data =  df_train_h20, ratios = .8)
train <- df.splits[[1]]
valid <- df.splits[[2]]

# Making list of all the predictors
colnames(df_train_h20)
predictors <- colnames(df_train_h20)[2:80]
predictors
predictors <- c(predictors,"totbath","lg_totbath"
                ,"Remod","Age","lg_Age","lg_house_age",
                "lg_MiscVal","lg_MasVnrArea","lg_LotArea","lg_LotFrontage","lg_ltarea_front"
                ,"lg_BsmtFinSF1","lg_BsmtUnfSF","lg_TotRmsAbvGrd","lg_WoodDeckSF","lg_OpenPorchSF"
)
sort(predictors)

# Setting Y variable
response <- "SalePrice"

housing_glm <- h2o.glm(x = predictors, y = response, training_frame = train,
                       validation_frame = valid,alpha = 0.35,nfolds = 10,lambda = 0.0013,seed = 1
                       #,lambda_search = TRUE
                       # ,interaction_pairs =list(
                       # c("MSSubClass","MSZoning","SaleCondition","OverallCond") )
                       # ,interactions = interact_list
                       # ignore_const_cols = FALSE
)

# Checking Accuracy on the validation frame
print(h2o.mse(housing_glm, valid=TRUE))
print(h2o.mae(housing_glm, valid=TRUE))
print(h2o.rmse(housing_glm, valid=TRUE))

# Plotting the coefficients of variables
h2o.std_coef_plot(housing_glm,num_of_features=40)
h2o.varimp_plot(housing_glm)
################################################################################
# GRID SEARCH
################################################################################

# Hyperparameter LAMBDA and ALPHA
#lambda_seq <- seq(0.1,0.00001,by=-0.0001)
#hyper_params <- list( lambda = lambda_seq)
# hyper_params <- list( lambda = c(0.099,0.001,0.002,0.003,0.004,0.005,0.006,0.009,0.0001,0.00011))
hyper_params <- list( lambda = c(1, 0.5, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0) ,
                      alpha = c(0, .25, .5,.75,.89,.99,1))

# Grid search for selecting the best model
grid <- h2o.grid(x = predictors, y = response ,training_frame = train,nfolds=10,validation_frame = valid,
                 algorithm = "glm", grid_id = "allstate_grid", hyper_params = hyper_params,seed=1,
                 search_criteria = list(strategy = "Cartesian"))

# Sort the grid models by mse
sortedGrid <- h2o.getGrid("allstate_grid", sort_by = "rmse", decreasing = FALSE)
sortedGrid

# Making prediction using the new GLM model
pred <- exp(h2o.predict(object=housing_glm, newdata=df_test_h20))
pred

# Checking for MEAN of prediction and specific value if any
mean(pred)
mean(pred[1090])

# Exporting results to the csv
h2o.exportFile(
  pred,
  "pred_h20_39.csv" ,index=False)

#Shutting down the H2o cluster
#h2o.shutdown()
samplesubmission = read_csv('C:\Users\admin\Documents\pred_h20_39.csv')

output = pd.DataFrame({'Id': samplesubmission.Id, 'SalePrice': predstest})
output.to_csv('submission.csv', index=False)


