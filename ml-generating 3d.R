library(tidyverse)

# data size
all <- 1000

# theta
theta <- runif(1000, min = 0, max = pi)

# class1, class2
c1 <- rnorm(n = all/2, mean = 40, sd = 1)
c1_df <- tibble(c1, "c1")
names(c1_df) <- c("value","class")

c2 <- rnorm(n = all/2, mean = 100, sd = 1)
c2_df <- tibble(c2, "c2")
names(c2_df) <- c("value","class")

c1c2_df <- bind_rows(c1_df,c2_df)
c1c2_df$class <- c1c2_df$class %>% as.factor()

# classify
colors <- c("#E69F00", "#56B4E9")
colors <- colors[as.numeric(c1c2_df$class)]

# phi, azimuth
azimuth <- runif(1000, min = 0, max = 2*pi)

# df
library(geometry)
df <- sph2cart(theta = theta, phi = azimuth, r = c1c2) %>% data.frame() # spherical coordinate to Cartesian coordinate

# 3d plot
library(scatterplot3d)
library(ggplot2)

scatterplot3d(df$x, df$y, df$z, color = colors)
