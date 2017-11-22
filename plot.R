z <- read.csv2("2017-11-21_210913.csv", sep = ",")
z <- tibble::as_tibble(z)
z <- subset(z, price < 400000 & price > 150000)

ll <- lapply(z$address, rtweet::lookup_coords)

foo <- function(x) {
  if (has_name_(x, "point")) {
    x <- x$point
  }
  x <- x[names(x) %in% c("lat", "lng", "lat1", "lng1")]
  names(x) <- c("lat", "lng")
  x
}

pts <- lapply(ll, foo)
pts <- tibble::as_tibble(do.call("rbind", pts))

pts$address <- unlist(lapply(ll, "[[", "place"))

z <- left_join(pts, z, by = "address")

lib(tidyverse)
z$sqft <- as.double(z$sqft)

ggplot(z, aes(x = lat, y = lng, color = price, size = sqft)) +
  geom_point() +
  theme_mwk()

install.packages("ggmap")
library(ggmap)

z$price_cat <- ifelse(
  z$price < 200000, "150-199k",
  ifelse(z$price < 250000, "200-249k",
         ifelse(z$price < 300000, "250-299k",
                ifelse(z$price < 350000, "300-349k", "350k+"))))

ggmap(get_map(location = 'Columbia, Missouri', zoom = 12, maptype = "toner-lite")) +
  geom_point(data = z, shape = 22, size = 3.25, alpha = .75,
             aes(x = lng, y = lat, fill = price_cat)) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        text = element_text(family = "Roboto"),
        axis.text = element_blank()) +
  labs(x = NULL, y = NULL)
##  scale_shape_manual(values = 21:25)
