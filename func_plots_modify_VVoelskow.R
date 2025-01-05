################################################################################
###     MODIFY SAVED PLOTS                                                  ####
################################################################################

################################
#   SET FONT TO TIMES NEW ROMAN                                             
################################

windowsFonts(Times = windowsFont("Times New Roman"))


######################################
##   CREATE THEME FUNCTIONS FOR PLOTS
######################################

# Create theme function for DFS and OS outcomes
theme_PLOS_dfs_os <- function() {
  theme(axis.text = element_text(color = "gray22", family = "Times", size = 10),
        axis.title = element_text(color = "gray22", family = "Times", size = 10),
        axis.line = element_line(colour = "gray22"),
        legend.position = "right", 
        legend.key.spacing.y = unit(-0.2, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(color = "gray22", family = "Times", size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())
}


# Create theme function for all other outcomes (where a title is included)
theme_PLOS_other <- function() {
  theme(axis.text = element_text(color = "gray22", family = "Times", size = 10),
        axis.title = element_text(color = "gray22", family = "Times", size = 10),
        axis.line = element_line(colour = "gray22"),
        legend.position = "right", 
        legend.key.spacing.y = unit(-0.2, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(color = "gray22", family = "Times", size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = "Times", face = "bold", size = 12, 
                                  vjust = 0.2, color = "gray22"))
}