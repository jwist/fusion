# ggvolcano = volcano %>%
#   reshape2::melt() %>%
#   ggplot() +
#   geom_tile(aes(x=Var1,y=Var2,fill=value)) +
#   geom_contour(aes(x=Var1,y=Var2,z=value),color="black") +
#   scale_x_continuous("X",expand = c(0,0)) +
#   scale_y_continuous("Y",expand = c(0,0)) +
#   scale_fill_gradientn("Z",colours = terrain.colors(10)) +
#   coord_fixed()
# ggvolcano
#
# a_theme <- theme(legend.position = "none",
#                  panel.background = element_blank(),
#                  panel.grid.major = element_blank(),
#                  panel.grid.minor = element_blank(),
#                  axis.text.x = element_blank(),
#                  axis.text.y = element_blank(),
#                  axis.title = element_blank(),
#                  axis.ticks = element_blank())
#
# a = data.frame(x=rnorm(10000, 9, 2.5), y=rnorm(10000, 9, 1.9) )
# b = data.frame(x=rnorm(10000, 13, 2.1), y=rnorm(10000, 14, 2.0) )
# c = data.frame(x=rnorm(20000, 12, 3), y=rnorm(20000, 2, 3) )
# data = rbind(a,b,c)
#
# pp_nolines = ggplot(data, aes(x=x, y=y)) +
#   geom_hex(bins = 50, size = 0) +
#   scale_fill_viridis_c(option = "D") +
#   a_theme
# pp_nolines
#
# svglite::svglite(filename = "space1.svg", width = 10, height = 10)
# pp_nolines
# dev.off()
#
#
# a = data.frame(x=rnorm(10000, 9, 2.5), y=rnorm(10000, 9, 1.9) )
# b = data.frame(x=rnorm(10000, 11, 2.1), y=rnorm(10000, 13, 2.0) )
# c = data.frame(x=rnorm(20000, 5, 3), y=rnorm(20000, 5, 3) )
# data = rbind(a,b,c)
#
# pp_nolines = ggplot(data, aes(x=x, y=y)) +
#   geom_hex(bins = 50, size = 0) +
#   scale_fill_viridis_c(option = "H") +
#   a_theme
# pp_nolines
#
# svglite::svglite(filename = "space2.svg", width = 10, height = 10)
# pp_nolines
# dev.off()
#
#
# a = data.frame(x=rnorm(20000, 9, 4), y=rnorm(20000, 9, 4) )
# b = data.frame(x=rnorm(20000, 10, 3), y=rnorm(20000, 11, 3) )
# c = data.frame(x=rnorm(20000, 3, 3), y=rnorm(20000, 14, 3) )
# data = rbind(a,b,c)
#
# pp_nolines = ggplot(data, aes(x=x, y=y)) +
#   geom_hex(bins = 50, size = 0) +
#   scale_fill_viridis_c(option = "B") +
#   a_theme
# pp_nolines
#
# svglite::svglite(filename = "space3.svg", width = 10, height = 10)
# pp_nolines
# dev.off()
