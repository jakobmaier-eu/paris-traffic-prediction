# What is this ?

This is the git repository related to the ML project of Guillaume Lambert et Jakob Maier.

If you are reading the report (see the report folder) and want to know more about what we have actually coded, you are invited to find the fitting R files here.

We give an overview here what the different folders contain:

### Preprocessing

This folder contains all the code we have used to aggregate data along graph edges, imputation (in the file missing_data.R), addition of new variables. Be aware that you may not be able to execute some code since we have not put the big raw data files into the git repository.

### Rapport

Here are all files related to the R-markdown project report, including images.

### Scores

This folder contains some RData files where we have stored vectors with performance scores of the different ML models.

### Scripts

These are small R files with little content, usually one visualisation, renaming of a variable in the dataframes, etc.

### Graphe

Here's information about our simplified graph about Paris' traffic. In particular, there are lists with all node and all edge names.

### Data

This folder contains a bunch of data we have used in the project, starting from information about the raw data, to some intermediately processed dataframe, up to almost clean prediction data. Note that the actual data we have used for predictions was too big for github, but you can find it in the package related to the project by using data("data_train") and data("data_test").

### 1_heure

Here we have stored some vectors of predicted data by different models on the train and test set (as well as some models themselves). All these models try to predict the next hours, whence the folder's name.

### 1_jour 

This is the remainder of a project we haven't been able to finish : Predicting the next day's traffic. There are some R files where we started training random forests for that case.


## No folder ?

Finally, there are some files outside of the folders. In these, we define our ML prediction models. If you are reading the report and want some more details, these are usually the files to look into. The names describe which models these R-files are about.