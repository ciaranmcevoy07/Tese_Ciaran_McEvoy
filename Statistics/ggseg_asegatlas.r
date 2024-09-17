library(ggseg)
library(ggplot2)
library(dplyr)

FC_AD <- read.csv("Statistics/test/FC_AD.csv")
FC_CN <- read.csv("Statistics/test/FC_CN.csv")
FC_MCI <- read.csv("Statistics/test/FC_MCI.csv")

df_ce_ad <- FC_AD %>% filter(Lipids == "CE")
df_ce_ad <- df_ce_ad %>% select(-Lipids, -FoldChange)
df_ce_ad <- df_ce_ad %>% rename(label = Regions)
# df_ce_ad$label <- gsub("[-]", " ", df_ce_ad$label)
df_ce_ad$label <- gsub("_volume", "", df_ce_ad$label)

df_ce_cn <- FC_CN %>% filter(Lipids == "CE")
df_ce_cn <- df_ce_cn %>% select(-Lipids, -FoldChange)
df_ce_cn <- df_ce_cn %>% rename(label = Regions)
# df_ce_cn$label <- gsub("[-]", " ", df_ce_cn$label)
df_ce_cn$label <- gsub("_volume", "", df_ce_cn$label)

df_ce_cn$group <- "CN"
df_ce_ad$group <- "AD"

combined_df <- rbind(df_ce_cn, df_ce_ad)

aseg_labels <- brain_labels(aseg)
df_filtered <- combined_df %>%
  filter(label %in% dk_labels)

someData = tibble(
  label = df_filtered$label, 
  p = df_filtered$p_value,
  group = df_filtered$group,
)
someData <- brain_join(someData, aseg, "label")

# # p <- ggseg(atlas="aseg", mapping=aes(fill=region))
p <- someData %>%
  group_by(group) %>%
  ggplot() +
  geom_brain(atlas = aseg, 
             aes(fill = p)) +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  facet_wrap(~group)

ggsave("Pics/test.pdf", plot = p, width = 10, height = 15, limitsize = FALSE)