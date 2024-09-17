library(ggplot2)
library(hrbrthemes)

dataset <- read.csv("DatasetPro12.0.csv")

dataset$Group <- ifelse(dataset$Group %in% c("SMC", "LMCI", "EMCI"), "MCI", dataset$Group)

# A basic scatterplot with color depending on Species
p <- ggplot(dataset, aes(x = CE, y = CC_Anterior_ICV)) + 
  geom_point(aes(color = Group, 
                 alpha = ifelse(Group == "AD", 1, 0.3))) + 
  theme_minimal() +  # Switch to a different theme
  ylab("CC_Anterior") +
  scale_color_manual(values = c("AD" = "red", "CN" = "blue", "MCI" = "green")) +
  scale_alpha_identity()

ggsave("Pics/SP_CC_Anterior_CE.pdf", plot = p, width = 10, height = 15, limitsize = FALSE)