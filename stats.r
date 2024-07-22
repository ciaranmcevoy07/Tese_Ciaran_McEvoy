library(tidyverse)
library(RColorBrewer)
library(ggrepel)

df_mci_ad <- read.csv("test/mci_ad.csv")
df_cn_mci <- read.csv("test/cn_mci.csv")
df_cn_ad <- read.csv("test/cn_ad.csv")


#######
df_mci_ad$diffexpressed <- 'NO'
df_mci_ad$diffexpressed[df_mci_ad$log2FC > 0.05 & df_mci_ad$p_value < 0.05] <- 'UP'
df_mci_ad$diffexpressed[df_mci_ad$log2FC < -0.05 & df_mci_ad$p_value < 0.05] <- 'DOWN'

top20degs <- head(df_mci_ad[order(df_mci_ad$p_value), 'Feature'], 20)
df_mci_ad$delabel <- ifelse(df_mci_ad$Feature %in% top20degs, df_mci_ad$Feature, NA)

p1 <- ggplot(data = df_mci_ad, aes(x = log2FC, y = -log10(p_value), col = diffexpressed, label = delabel)) + 
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
#########
df_cn_mci$diffexpressed <- 'NO'
df_cn_mci$diffexpressed[df_cn_mci$log2FC > 0.05 & df_cn_mci$p_value < 0.05] <- 'UP'
df_cn_mci$diffexpressed[df_cn_mci$log2FC < -0.05 & df_cn_mci$p_value < 0.05] <- 'DOWN'

top20degs <- head(df_cn_mci[order(df_cn_mci$p_value), 'Feature'], 20)
df_cn_mci$delabel <- ifelse(df_cn_mci$Feature %in% top20degs, df_cn_mci$Feature, NA)

p2 <- ggplot(data = df_cn_mci, aes(x = log2FC, y = -log10(p_value), col = diffexpressed, label = delabel)) + 
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

########

df_cn_ad$diffexpressed <- 'NO'
df_cn_ad$diffexpressed[df_cn_ad$log2FC > 0.05 & df_cn_ad$p_value < 0.05] <- 'UP'
df_cn_ad$diffexpressed[df_cn_ad$log2FC < -0.05 & df_cn_ad$p_value < 0.05] <- 'DOWN'

top20degs <- head(df_cn_ad[order(df_cn_ad$p_value), 'Feature'], 20)
df_cn_ad$delabel <- ifelse(df_cn_ad$Feature %in% top20degs, df_cn_ad$Feature, NA)

p3 <- ggplot(data = df_cn_ad, aes(x = log2FC, y = -log10(p_value), col = diffexpressed, label = delabel)) + 
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

#########
print(p3)

