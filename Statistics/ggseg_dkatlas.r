library(ggseg)
library(ggplot2)
library(dplyr)

Dataset <- read.csv("DatasetPro13.0.csv")
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

# dk_atlas <- brain_atlas(dk)
dk_labels <- brain_labels(dk)
df_filtered <- combined_df %>%
  filter(label %in% dk_labels)

df_filtered$check <- 'NO'
df_filtered$check[df_filtered$p_value > 0.05] <- 'YES'

someData = tibble(
  label = df_filtered$label, 
  p = df_filtered$p_value,
  r = df_filtered$r,
  group = df_filtered$group
)
someData <- brain_join(someData, dk, "label")

# col_names <- colnames(Dataset)[72:100]

# # # first_row_values <- as.numeric(Dataset[1, 10:137]) 
# # first_row_values <- as.numeric(Dataset[1, 72:100]) 

# data("aseg")  
# merged_data <- merge(aseg, df_ce, by = "region", all.x = TRUE)
# # # print(df$region)

# # print(unique(df_ce$region))
# # print(unique(aseg$region))

# # # p1 <- ggseg(atlas = aseg, 
# # #             mapping = aes(fill = value), 
# # #             data = df) + 
# # #   scale_fill_viridis_c() + 
# # #   theme_void() + 
# # #   labs(title = "Cortical Brain Regions", fill = "Value")

# # p2 <- ggseg(.data=merged_data, mapping=aes(fill=p_value))
# print(dkt)

# p<- ggseg(someData, atlas = dk, 
#       colour = "black",
#       size = .1, 
#       position = "stacked",
#       mapping = aes(fill = p))



# p <- ggplot(someData) +
#   geom_brain(atlas = dk, 
#              position = position_brain(hemi ~ side),
#              aes(fill = p)) +
#   scale_fill_viridis_c(option = "cividis", direction = -1) +
#   theme_void() +
#   labs(title = "CE influence on Brain Regions in CN patients", 
#        subtitle = "Lateral Brain View")
# p <- ggplot() +
#   geom_brain(atlas = dk, position = position_brain(hemi ~ side))

p <- someData %>%
  group_by(group) %>%
  ggplot() +
  geom_brain(atlas = dk, aes(fill = r)) +
  scale_fill_gradient2(low = "blue", mid = "grey", high = "yellow", 
                       midpoint = 0, limits = c(-0.3, 0.3), 
                       oob = scales::squish) +
  facet_wrap(~group)

ggsave("Pics/Dhcerlipid_ADvsCN_dkatlas.pdf", plot = p, width = 10, height = 15, limitsize = FALSE)