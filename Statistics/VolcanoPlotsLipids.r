library(tidyverse)
library(RColorBrewer)
library(ggrepel)

df_mci_ad_LS <- read.csv("Statistics/test/mci_ad_LS.csv")
df_cn_mci_LS <- read.csv("Statistics/test/cn_mci_LS.csv")
df_cn_ad_LS <- read.csv("Statistics/test/cn_ad_LS.csv")
df_mci_ad_LC <- read.csv("Statistics/test/mci_ad_LC.csv")
df_cn_mci_LC <- read.csv("Statistics/test/cn_mci_LC.csv")
df_cn_ad_LC <- read.csv("Statistics/test/cn_ad_LC.csv")


####### MCI vs AD for Lipid Species ################
df_mci_ad_LS$diffexpressed <- 'NO'
df_mci_ad_LS$diffexpressed[df_mci_ad_LS$log2FC > 0.05 & df_mci_ad_LS$p_value < 0.05] <- 'UP'
df_mci_ad_LS$diffexpressed[df_mci_ad_LS$log2FC < -0.05 & df_mci_ad_LS$p_value < 0.05] <- 'DOWN'

top20degs <- head(df_mci_ad_LS[order(df_mci_ad_LS$p_value), 'Feature'], 20)
df_mci_ad_LS$delabel <- ifelse(df_mci_ad_LS$Feature %in% top20degs, df_mci_ad_LS$Feature, NA)

p1 <- ggplot(data = df_mci_ad_LS, aes(x = log2FC, y = -log10(p_value), col = diffexpressed, label = delabel)) + 
    geom_vline(xintercept = c(-0.05, 0.05), col = 'gray', linetype = 'dashed') +
    geom_hline(yintercept = (0.05), col = 'gray', linetype = 'dashed') +
    geom_point(size = 2) +
    theme_classic() +
    scale_color_manual(
        values = c("#00AFBB", "grey", "#bb0c00"),
        labels = c("Downregulated", "Not significant", "Upregulated")) +
    labs(x = "log2FC",
         y = "-log10(p-value)",
         color = "Expression Status") +
    ggtitle('MCI vs AD Volcano Plot') +
    geom_text_repel(max.overlaps = Inf)

####### CN vs MCI for Lipid Species ################
df_cn_mci_LS$diffexpressed <- 'NO'
df_cn_mci_LS$diffexpressed[df_cn_mci_LS$log2FC > 0.05 & df_cn_mci_LS$p_value < 0.05] <- 'UP'
df_cn_mci_LS$diffexpressed[df_cn_mci_LS$log2FC < -0.05 & df_cn_mci_LS$p_value < 0.05] <- 'DOWN'

top20degs <- head(df_cn_mci_LS[order(df_cn_mci_LS$p_value), 'Feature'], 20)
df_cn_mci_LS$delabel <- ifelse(df_cn_mci_LS$Feature %in% top20degs, df_cn_mci_LS$Feature, NA)

p2 <- ggplot(data = df_cn_mci_LS, aes(x = log2FC, y = -log10(p_value), col = diffexpressed, label = delabel)) + 
    geom_vline(xintercept = c(-0.05, 0.05), col = 'gray', linetype = 'dashed') +
    geom_hline(yintercept = (0.05), col = 'gray', linetype = 'dashed') +
    geom_point(size = 2) +
    theme_classic() +
    scale_color_manual(
        values = c("#00AFBB", "grey", "#bb0c00"),
        labels = c("Downregulated", "Not significant", "Upregulated")) +
    labs(x = "log2FC",
         y = "-log10(p-value)",
         color = "Expression Status") +
    ggtitle('CN vs MCI Volcano Plot') +
    geom_text_repel(max.overlaps = Inf)

####### CN vs AD for Lipid Species ################

df_cn_ad_LS$diffexpressed <- 'NO'
df_cn_ad_LS$diffexpressed[df_cn_ad_LS$log2FC > 0.05 & df_cn_ad_LS$p_value < 0.05] <- 'UP'
df_cn_ad_LS$diffexpressed[df_cn_ad_LS$log2FC < -0.05 & df_cn_ad_LS$p_value < 0.05] <- 'DOWN'

top20degs <- head(df_cn_ad_LS[order(df_cn_ad_LS$p_value), 'Feature'], 20)
df_cn_ad_LS$delabel <- ifelse(df_cn_ad_LS$Feature %in% top20degs, df_cn_ad_LS$Feature, NA)

p3 <- ggplot(data = df_cn_ad_LS, aes(x = log2FC, y = -log10(p_value), col = diffexpressed, label = delabel)) + 
    geom_vline(xintercept = c(-0.05, 0.05), col = 'gray', linetype = 'dashed') +
    geom_hline(yintercept = (0.05), col = 'gray', linetype = 'dashed') +
    geom_point(size = 2) +
    theme_classic() +
    scale_color_manual(
        values = c("#00AFBB", "grey", "#bb0c00"),
        labels = c("Downregulated", "Not significant", "Upregulated")) +
    labs(x = "log2FC",
         y = "-log10(p-value)",
         color = "Expression Status") +
    ggtitle('CN vs AD Volcano Plot') +
    geom_text_repel(max.overlaps = Inf)


####### MCI vs AD for Lipid Classes ################
df_mci_ad_LC$diffexpressed <- 'NO'
df_mci_ad_LC$diffexpressed[df_mci_ad_LC$log2FC > 0.05 & df_mci_ad_LC$p_value < 0.05] <- 'UP'
df_mci_ad_LC$diffexpressed[df_mci_ad_LC$log2FC < -0.05 & df_mci_ad_LC$p_value < 0.05] <- 'DOWN'

top20degs <- head(df_mci_ad_LC[order(df_mci_ad_LC$p_value), 'Feature'], 20)
df_mci_ad_LC$delabel <- ifelse(df_mci_ad_LC$Feature %in% top20degs, df_mci_ad_LC$Feature, NA)

p4 <- ggplot(data = df_mci_ad_LC, aes(x = log2FC, y = -log10(p_value), col = diffexpressed, label = delabel)) + 
    geom_vline(xintercept = c(-0.05, 0.05), col = 'gray', linetype = 'dashed') +
    geom_hline(yintercept = (0.05), col = 'gray', linetype = 'dashed') +
    geom_point(size = 2) +
    theme_classic() +
    scale_color_manual(
        values = c("#00AFBB", "grey", "#bb0c00"),
        labels = c("Downregulated", "Not significant", "Upregulated")) +
    labs(x = "log2FC",
         y = "-log10(p-value)",
         color = "Expression Status") +
    ggtitle('MCI vs AD Volcano Plot') +
    geom_text_repel(max.overlaps = Inf)

####### CN vs MCI for Lipid Classes ################
df_cn_mci_LC$diffexpressed <- 'NO'
df_cn_mci_LC$diffexpressed[df_cn_mci_LC$log2FC > 0.05 & df_cn_mci_LC$p_value < 0.05] <- 'UP'
df_cn_mci_LC$diffexpressed[df_cn_mci_LC$log2FC < -0.05 & df_cn_mci_LC$p_value < 0.05] <- 'DOWN'

top20degs <- head(df_cn_mci_LC[order(df_cn_mci_LC$p_value), 'Feature'], 20)
df_cn_mci_LC$delabel <- ifelse(df_cn_mci_LC$Feature %in% top20degs, df_cn_mci_LC$Feature, NA)

p5 <- ggplot(data = df_cn_mci_LC, aes(x = log2FC, y = -log10(p_value), col = diffexpressed, label = delabel)) + 
    geom_vline(xintercept = c(-0.05, 0.05), col = 'gray', linetype = 'dashed') +
    geom_hline(yintercept = (0.05), col = 'gray', linetype = 'dashed') +
    geom_point(size = 2) +
    theme_classic() +
    scale_color_manual(
        values = c("#00AFBB", "grey", "#bb0c00"),
        labels = c("Downregulated", "Not significant", "Upregulated")) +
    labs(x = "log2FC",
         y = "-log10(p-value)",
         color = "Expression Status") +
    ggtitle('CN vs MCI Volcano Plot') +
    geom_text_repel(max.overlaps = Inf)

####### CN vs AD for Lipid Classes ################
df_cn_ad_LC$diffexpressed <- 'NO'
df_cn_ad_LC$diffexpressed[df_cn_ad_LC$log2FC > 0.05 & df_cn_ad_LC$p_value < 0.05] <- 'UP'
df_cn_ad_LC$diffexpressed[df_cn_ad_LC$log2FC < -0.05 & df_cn_ad_LC$p_value < 0.05] <- 'DOWN'

top20degs <- head(df_cn_ad_LC[order(df_cn_ad_LC$p_value), 'Feature'], 20)
df_cn_ad_LC$delabel <- ifelse(df_cn_ad_LC$Feature %in% top20degs, df_cn_ad_LC$Feature, NA)

p6 <- ggplot(data = df_cn_ad_LC, aes(x = log2FC, y = -log10(p_value), col = diffexpressed, label = delabel)) + 
    geom_vline(xintercept = c(-0.05, 0.05), col = 'gray', linetype = 'dashed') +
    geom_hline(yintercept = (0.05), col = 'gray', linetype = 'dashed') +
    geom_point(size = 2) +
    theme_classic() +
    scale_color_manual(
        values = c("#00AFBB", "grey", "#bb0c00"),
        labels = c("Downregulated", "Not significant", "Upregulated")) +
    labs(x = "log2FC",
         y = "-log10(p-value)",
         color = "Expression Status") +
    ggtitle('CN vs AD Volcano Plot') +
    geom_text_repel(max.overlaps = Inf)

#############################
ggsave("Pics/VolcanoPlot_LC_CNvsAD.pdf", plot = p6, width = 10, height = 15, limitsize = FALSE)
# print(p6)

