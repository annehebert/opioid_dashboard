
#contains 3 themes for the plots used in the dashboard: 
  #1 for maps, 2 for scatterplots and 3 for time course plots

map_theme <- theme(
  title = element_text(size = 14),
  legend.position = "bottom",
  legend.text.align = 0,
  axis.line = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank()
)


scatter_plot_theme <- theme(
  title = element_text(size = 14),
  legend.position = "bottom",
  legend.title = element_text(size = 12),
  panel.background = element_rect(fill = "white",
                                  color = "black",
                                  size = 0.5,
                                  linetype = "solid"),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12)
  )

time_plot_theme <- theme(
  legend.title = element_blank(),
  legend.text = element_text(size = 7),
  plot.title = element_text(size = 12),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.border = element_rect(color = 'grey', fill = NA),
  panel.background = element_blank(),
  axis.line = element_line(colour = "grey"))


  