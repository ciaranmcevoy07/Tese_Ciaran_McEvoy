library(dplyr)
library(ggcorrplot)
library("FactoMineR")
library('corrr')
library(factoextra)

dataset <- read.csv("c:\\Users\\ciara\\Desktop\\Tese_Ciaran_McEvoy\\DatasetPro10.0.csv")

############ PCA Analysis for all Subjects #####################

# table <- dataset[, 1046:1080]

############ PCA Analysis for AD Subjects #####################

# filtered_dataset <- dataset[dataset$Group == 'AD', ]
# table <- filtered_dataset[, 1046:1080]

############ PCA Analysis for CN Subjects #####################

# filtered_dataset <- dataset[dataset$Group == 'CN', ]
# table <- filtered_dataset[, 1046:1080]

############ PCA Analysis for MCI Subjects #####################

values_to_filter <- c('MCI', 'LMCI', 'EMCI', 'SMC')
filtered_dataset <- dataset[dataset$Group %in% values_to_filter, ]
table <- filtered_dataset[, 1046:1080]

####################################################################
data.pca <- princomp(table)
summary(data.pca)
data.pca$loadings[, 1:2]
# print(fviz_eig(data.pca, addlabels = TRUE))
# print(fviz_pca_var(data.pca, col.var = "black"))
# print(fviz_cos2(data.pca, choice = "var", axes = 1:2))
print(fviz_pca_var(data.pca, col.var = "cos2",
            gradient.cols = c("black", "orange", "green"),
            repel = TRUE))