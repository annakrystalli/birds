
x <-x[x$SEASONAL %in% 1:2,]

x@data$id <- x@data$SEASONAL

points <- fortify(x, region = "id")
DF <- merge(points, x@data, by = "id")


gg <- ggplot(data = DF, aes(x=long, y=lat, group = group,
                                              fill = id)) +
  geom_polygon()  +
  geom_path(color = "white") +
  scale_fill_hue(l = 40) +
  coord_equal() +
  theme(legend.position = "none", title = element_blank(),
        axis.text = element_blank())

print(gg)

washingtonWatershedsPlot