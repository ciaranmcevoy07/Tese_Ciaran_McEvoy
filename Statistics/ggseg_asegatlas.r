library(ggseg)
library(ggplot2)
library(dplyr)

FC_AD <- read.csv("Statistics/dataframes/LongFormat_LxV_AD.csv")
FC_CN <- read.csv("Statistics/dataframes/LongFormat_LxV_CN.csv")
FC_MCI <- read.csv("Statistics/dataframes/LongFormat_LxV_MCI.csv")

df_ce_ad <- FC_AD %>% filter(Lipids == "DEDE")
df_ce_ad <- df_ce_ad %>% select(-Lipids, -FoldChange)
df_ce_ad <- df_ce_ad %>% rename(label = Regions)
# df_ce_ad$label <- gsub("[-]", " ", df_ce_ad$label)
df_ce_ad$label <- gsub("_volume", "", df_ce_ad$label)

df_ce_cn <- FC_CN %>% filter(Lipids == "DEDE")
df_ce_cn <- df_ce_cn %>% select(-Lipids, -FoldChange)
df_ce_cn <- df_ce_cn %>% rename(label = Regions)
# df_ce_cn$label <- gsub("[-]", " ", df_ce_cn$label)
df_ce_cn$label <- gsub("_volume", "", df_ce_cn$label)

df_ce_cn$group <- "CN"
df_ce_ad$group <- "AD"

combined_df <- rbind(df_ce_cn, df_ce_ad)

aseg_labels <- brain_labels(aseg)
df_filtered <- combined_df %>%
  filter(label %in% aseg_labels)

someData = tibble(
  label = df_filtered$label, 
  r = df_filtered$r,
  group = df_filtered$group,
)
someData <- brain_join(someData, aseg, "label")

# # p <- ggseg(atlas="aseg", mapping=aes(fill=region))
p <- someData %>%
  group_by(group) %>%
  ggplot() +
  geom_brain(atlas = aseg, 
             aes(fill = r)) +
  scale_fill_gradient2(low = "blue", mid = "grey", high = "yellow", 
                       midpoint = 0, limits = c(-0.3, 0.3), 
                       oob = scales::squish) +
  facet_wrap(~group)

ggsave("Pics/DEDElipid_ADvsCN_asegatlas.pdf", plot = p, width = 10, height = 15, limitsize = FALSE)