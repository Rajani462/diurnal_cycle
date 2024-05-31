library(ggplot2)
library(gtable)
library(grid)

my_triangle_colourbar <- function(...) {
  guide <- guide_colourbar(...)
  class(guide) <- c("my_triangle_colourbar", class(guide))
  guide
}

guide_gengrob.my_triangle_colourbar <- function(...) {
  # First draw normal color bar
  guide <- NextMethod()
  # Extract bar / colors
  is_bar <- grep("^bar$", guide$layout$name)
  bar <- guide$grobs[[is_bar]]
  extremes <- c(bar$raster[1], bar$raster[length(bar$raster)])
  # Extract size
  width <- guide$widths[guide$layout$l[is_bar]]
  height <- guide$heights[guide$layout$t[is_bar]]
  short <- min(convertUnit(width, "cm", valueOnly = TRUE),
               convertUnit(height, "cm", valueOnly = TRUE))
  # Make space for triangles
  guide <- gtable_add_cols(guide, unit(short, "cm"),
                           guide$layout$l[is_bar] - 1)
  guide <- gtable_add_cols(guide, unit(short, "cm"),
                           guide$layout$l[is_bar])
  
  # Draw triangles
  left <- polygonGrob(
    x = unit(c(0, 1, 1), "npc"),
    y = unit(c(0, 0.5, 1), "npc"),
    gp = gpar(fill = extremes[1], col = NA)
  )
  right <- polygonGrob(
    x = unit(c(0, 1, 0), "npc"),
    y = unit(c(0, 0.5, 1), "npc"),
    gp = gpar(fill = extremes[2], col = NA)
  )
  # Add triangles to guide
  guide <- gtable_add_grob(
    guide, left,
    t = guide$layout$t[is_bar],
    l = guide$layout$l[is_bar] - 1
  )
  guide <- gtable_add_grob(
    guide, right,
    t = guide$layout$t[is_bar],
    l = guide$layout$l[is_bar] + 1
  )
  
  return(guide)
}



ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  # geom_text(data = lbl.Y.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2, hjust = 1.5) +
  # geom_text(data = lbl.X.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot, aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_wrap(~name, ncol = 3) + 
  # scale_fill_binned(type = "viridis", option = "B", direction = -1,
  #                   breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.5, 1, 1.5, 2), show.limits = TRUE) + 
  labs(x = NULL, y = NULL, fill = "Mean (mm/hr)") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
  theme_small +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"))+
  theme(strip.background = element_blank(), panel.border=element_blank(),
        legend.position = "bottom") + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) + 
  scale_fill_gradient2(
    high = "darkblue", mid = "lightyellow", low = "white", midpoint = 0.2,
    name = "MAP",
    limits = c(0.02, 0.5),
    oob = scales::oob_squish,
    guide = my_triangle_colourbar())

    
ggsave("./projects/main/results/trial_spat_mean.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)


ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  # geom_text(data = lbl.Y.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2, hjust = 1.5) +
  # geom_text(data = lbl.X.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot, aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_wrap(~name, ncol = 3) + 
  # scale_fill_binned(type = "viridis", option = "B", direction = -1,
  #                   breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.5, 1, 1.5, 2), show.limits = TRUE) + 
  labs(x = NULL, y = NULL, fill = "Mean (mm/hr)") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
  theme_small +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"))+
  theme(strip.background = element_blank(), panel.border=element_blank(),
        legend.position = "bottom") + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) +  
  scale_fill_gradientn(
    colors = c("gray", "lightblue", "orange", "red"),
    name = "Mean (mm/hr)",
    limits = c(0.02, 0.5),
    oob = scales::oob_squish,
    guide = my_triangle_colourbar()) + 
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
            legend.position = "bottom",
            legend.key.width = unit(2.8, "cm"),
            legend.key.height = unit(0.4, "cm"), 
            legend.spacing = unit(0.25,"cm"),
            legend.text = element_text(size = 12), 
            legend.title = element_text(hjust = 0.5, size = 12),
            legend.justification = "center")


ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  # geom_text(data = lbl.Y.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2, hjust = 1.5) +
  # geom_text(data = lbl.X.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot, aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_wrap(~name, ncol = 3) + 
  # scale_fill_binned(type = "viridis", option = "B", direction = -1,
  #                   breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.5, 1, 1.5, 2), show.limits = TRUE) + 
  labs(x = NULL, y = NULL, fill = "Mean (mm/hr)") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
  theme_small +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"))+
  theme(strip.background = element_blank(), panel.border=element_blank(),
        legend.position = "bottom") + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) +  
  scale_fill_stepsn(#high = "darkblue", mid = "white", low = "darkred", midpoint = 0.2,
    name = "mean (mm/hr)", colors = terrain.colors(12),
    breaks = seq(0, 2.5, 0.05),
    limits = c(0.02, 0.6), show.limits = TRUE,
    oob = scales::oob_squish,
    guide = my_triangle_colourbar()) +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
            legend.position = "bottom",
            legend.key.width = unit(2.8, "cm"),
            legend.key.height = unit(0.4, "cm"), 
            legend.spacing = unit(0.25,"cm"),
            legend.text = element_text(size = 12), 
            legend.title = element_text(hjust = 0.5, size = 12),
            legend.justification = "center")
