library(tidyverse)

# data size
all <- 1000

# theta
theta <- runif(1000, min = 0, max = pi)

# r1, r2
r1 <- rnorm(n = all/2, mean = 40, sd = 1)
r1_df <- tibble(r1, "r1")
names(r1_df) <- c("value","class")

r2 <- rnorm(n = all/2, mean = 100, sd = 1)
r2_df <- tibble(r2, "r2")
names(r2_df) <- c("value","class")

r1r2_df <- bind_rows(r1_df,r2_df)
r1r2_df$class <- r1r2_df$class %>% as.factor()

# classify
colors <- c("#E69F00", "#56B4E9")
colors <- colors[as.numeric(r1r2_df$class)]

# phi, azimuth
azimuth <- runif(1000, min = 0, max = 2*pi)

# df
library(geometry)
df <- sph2cart(theta = theta, phi = azimuth, r = r1r2) %>% data.frame() # spherical coordinate to Cartesian coordinate

# 3d plot
library(scatterplot3d)
library(ggplot2)

scatterplot3d(df$x, df$y, df$z, color = colors)
