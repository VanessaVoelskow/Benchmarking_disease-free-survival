################################################################################
###   GGPLOT CODE FOR COMBINED PLOTS                                         ###
################################################################################

####################################################
#   COMBINE PLOTS OF CANCER-SPECIFIC DFS COMPONENTS                         
####################################################

gg.dfs.components <- (get(paste0("plot.ipw.recmet.", analysis)) | 
                           get(paste0("plot.ipw.recloc.", analysis))) /
                          (get(paste0("plot.ipw.contralat.", analysis)) | 
                           get(paste0("plot.ipw.secprim.", analysis)))

# Convert to ggplot object
gg.dfs.components <- ggplotify::as.ggplot(print(gg.dfs.components))

# Add legend to the upper right corner
gg.dfs.components <- gg.dfs.components + ggimage::geom_subview(x = 0.84, y = 0.96, subview = legend)



#################
###    SAVE PLOT                         
#################

ggsave(filename = output.path.fig.c.pdf,
       width = width.c, height = height.c, units = units.c, dpi = dpi.c,
       plot = gg.dfs.components)

ggsave(filename = output.path.fig.c.png,
       width = width.c, height = height.c, units = units.c, dpi = dpi.c,
       plot = gg.dfs.components)

ggsave(filename = output.path.fig.c.eds,
       width = width.c, height = height.c, units = units.c, dpi = dpi.c,
       plot = gg.dfs.components,
       device = cairo_ps)


