library(tidyverse)


data <- read.csv("/scratch/ope4/Survival_david.csv")


data$Proportion_.Surviving <- data$Proportion_.Surviving * 100



p1 <- ggplot(data = data, aes(x = Survival_.Day, y = Proportion_.Surviving, 
                          linetype = Group)) +
  geom_step() +  
  scale_linetype_manual(values = c("Control" = "dashed", "PGG" = "solid")) +  # Define line types
  labs(title = "BAPN Aneurysm Rupture Survival Curve",  
       x = "Days since AngII pump Implantation",  
       y = "Proportion Surviving (%)")+
  #scale_color_manual(values = c("Control" = "blue", "PGG" = "red")) +  
  #xlim(0, 30) + 
  #facet_wrap(~Study_group, scales = "free_x") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + 
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 4)) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    #panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),  # Make x-axis text black
    axis.text.y = element_text(color = "black"),   # Make y-axis text black
    axis.line = element_line(colour = "black")
  ) +
  coord_fixed(ratio = 0.2) 

p1

# Save the plot
ggsave("/scratch/ope4/Survival_david.png", p1, width = 10, height = 8, dpi = 300, bg = "white")



####################################################################################################################
data_2 <- data |>
  filter(Group == "Control")

ggplot(data = data_2, aes(x = Survival_.Day, y = Proportion_.Surviving, 
                          linetype = Group)) +
  geom_step() +  
  scale_linetype_manual(values = c("Control" = "dashed", "PGG" = "solid")) +  # Define line types
  labs(title = "BAPN Aneurysm Ruputre Survival Curve (Control)",  
       x = "Days since AngII pump Implantation",  
       y = "Proportion Surviving (%)")+
  #scale_color_manual(values = c("Control" = "blue", "PGG" = "red")) +  
  #xlim(0, 30) + 
  #facet_wrap(~Study_group, scales = "free_x") +
  scale_y_continuous(limits = c(20, 100), breaks = seq(20, 100, by = 20)) + 
  scale_x_continuous(limits = c(0, 16), breaks = seq(0, 16, by = 4)) +  
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    #panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black")
  ) +
  coord_fixed(ratio = 0.1) 


