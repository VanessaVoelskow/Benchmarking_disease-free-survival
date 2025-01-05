################################################################################
###   GGPLOT CODE FOR INDIVIDUAL PLOTS                                       ###
################################################################################

####################################
###     LOAD RELEVANT RESULTS TABLE
####################################

tab.to.plot <- read.csv(file = input.path.tab)


#############################################
###     CONSTRUCT PARAMETRIC SURVIVAL CURVE                         
#############################################

plot.ipw <- ggplot(tab.to.plot, 
                   aes(x=t0, y=surv, col=surv, linetype=surv)) +
  geom_line(aes(y = surv0,
                color = "Chemotherapy + trastuzumab", linetype = "Chemotherapy + trastuzumab"),
            linewidth = 1, show.legend = TRUE) + 
  geom_line(aes(y = surv1,
                color = "Chemotherapy", linetype = "Chemotherapy"),
            linewidth = 1, show.legend = TRUE) +
  xlab("Time from baseline (years)") +
  scale_x_continuous(limits = c(0, 260),
                     breaks=c(0, 52, 104, 156, 208, 260),
                     labels=c("0", "1", "2", "3", "4", "5")) + 
  ylab(ylab.description) +
  scale_y_continuous(limits=c(0.45, 1.00),
                     breaks=c(0.45, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00),
                     labels=c("0", "50", "60",
                              "70", "80", "90", "100")) +
  theme_minimal() +
  theme_PLOS_dfs_os() +
  scale_color_manual(values=c("#1F449C", "#7CA1CC"),
                     breaks=c('Chemotherapy + trastuzumab',
                              'Chemotherapy')) +
  scale_linetype_manual(values=c("solid", "twodash"),
                        breaks=c('Chemotherapy + trastuzumab',
                                 'Chemotherapy')) +
  scale_fill_discrete(breaks=c('Chemotherapy + trastuzumab', 'Chemotherapy'))


  # Get legend
  legend = cowplot::get_plot_component(plot.ipw, 'guide-box-right', return_all = TRUE)


  # Break axis
  plot.ipw <- plot.ipw +
                scale_y_break(c(0.458, 0.49999), space = 0.2) +
                theme(legend.position = "none") +
  
  # Add RD and RR from individual results tables (formatted according journal convention)
  annotate("text", x = 10, y = 0.50,
           label = paste0("5-year risk difference (95% CI):\n",
                          get(paste0("results.", event, ".", analysis))$km.rd.ci),
           color = "gray22", family = "Times", size=8/.pt,
           hjust = 0)  + # Left align the text
  annotate("text", x = 10, y = 0.57,
           label = paste0("5-year risk ratio (95% CI):\n",
                          get(paste0("results.", event, ".", analysis))$rr.ci),
           color = "gray22", family = "Times", size=8/.pt,
           hjust = 0) + # Left align the text

  # Remove right axis
  theme(axis.line.y.right = element_blank(),   # Remove the right axis line
        axis.ticks.y.right = element_blank(),  # Remove right axis ticks
        axis.text.y.right = element_blank())   # Remove right axis text


  # Convert back to ggplot object
  plot.ipw.print <- ggplotify::as.ggplot(print(plot.ipw))


  # Add legend
  plot.ipw.image <- plot.ipw.print + ggimage::geom_subview(x = 0.57, y = 0.90, subview = legend)
  
  
  # Store in global environment (to combine them in the next step)
  assign(paste0("plot.ipw.", event, ".", analysis), plot.ipw.image, envir = .GlobalEnv)



#################
###    SAVE PLOT                         
#################

ggsave(filename = output.path.fig.i.pdf,
       width = width.i, height = height.i, units = units.i, dpi = dpi.i,
       plot = plot.ipw.image)

ggsave(filename = output.path.fig.i.png,
       width = width.i, height = height.i, units = units.i, dpi = dpi.i,
       plot = plot.ipw.image)

ggsave(filename = output.path.fig.i.eds,
       width = width.i, height = height.i, units = units.i, dpi = dpi.i,
       plot = plot.ipw.image,
       device = cairo_ps)



rm(plot.ipw, plot.ipw.print, plot.ipw.image, tab.to.plot)


