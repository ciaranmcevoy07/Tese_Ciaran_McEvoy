library(tidyverse)
library(RColorBrewer)
library(ggrepel)

df_cn_ad_VOL <- read.csv("Statistics/test/cn_ad_VOL.csv")
df_cn_mci_VOL <- read.csv("Statistics/test/cn_mci_VOL.csv")
df_mci_ad_VOL <- read.csv("Statistics/test/mci_ad_VOL.csv")

####################################################################################
df_cn_ad_VOL$diffexpressed <- 'NO'
df_cn_ad_VOL$diffexpressed[df_cn_ad_VOL$log2FC > 0.05 & df_cn_ad_VOL$p_value < 0.05] <- 'UP'
df_cn_ad_VOL$diffexpressed[df_cn_ad_VOL$log2FC < -0.05 & df_cn_ad_VOL$p_value < 0.05] <- 'DOWN'

top20degs <- head(df_cn_ad_VOL[order(df_cn_ad_VOL$p_value), 'Feature'], 20)
df_cn_ad_VOL$delabel <- ifelse(df_cn_ad_VOL$Feature %in% top20degs, df_cn_ad_VOL$Feature, NA)

p1 <- ggplot(data = df_cn_ad_VOL, aes(x = log2FC, y = -log10(p_value), col = diffexpressed, label = delabel)) + 
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
####################################################################################
df_cn_mci_VOL$diffexpressed <- 'NO'
df_cn_mci_VOL$diffexpressed[df_cn_mci_VOL$log2FC > 0.05 & df_cn_mci_VOL$p_value < 0.05] <- 'UP'
df_cn_mci_VOL$diffexpressed[df_cn_mci_VOL$log2FC < -0.05 & df_cn_mci_VOL$p_value < 0.05] <- 'DOWN'

top20degs <- head(df_cn_mci_VOL[order(df_cn_mci_VOL$p_value), 'Feature'], 20)
df_cn_mci_VOL$delabel <- ifelse(df_cn_mci_VOL$Feature %in% top20degs, df_cn_mci_VOL$Feature, NA)

p2 <- ggplot(data = df_cn_mci_VOL, aes(x = log2FC, y = -log10(p_value), col = diffexpressed, label = delabel)) + 
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
####################################################################################
df_mci_ad_VOL$diffexpressed <- 'NO'
df_mci_ad_VOL$diffexpressed[df_mci_ad_VOL$log2FC > 0.05 & df_mci_ad_VOL$p_value < 0.05] <- 'UP'
df_mci_ad_VOL$diffexpressed[df_mci_ad_VOL$log2FC < -0.05 & df_mci_ad_VOL$p_value < 0.05] <- 'DOWN'

top20degs <- head(df_mci_ad_VOL[order(df_mci_ad_VOL$p_value), 'Feature'], 20)
df_mci_ad_VOL$delabel <- ifelse(df_mci_ad_VOL$Feature %in% top20degs, df_mci_ad_VOL$Feature, NA)

p3 <- ggplot(data = df_mci_ad_VOL, aes(x = log2FC, y = -log10(p_value), col = diffexpressed, label = delabel)) + 
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
####################################################################################
ggsave("Pics/VolcanoPlot_Vol_CNvsMCI.pdf", plot = p2, width = 10, height = 15, limitsize = FALSE)