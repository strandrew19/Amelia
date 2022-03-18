library(ggplot2)
library(ggpubr)
library(extrafont)
remotes::install_version("Rttf2pt1", version = "1.3.8")

wd <- dirname(rstudioapi::getActiveDocumentContext()$path)

font_import(paths = "C:/Users/s1ddschi/AppData/Local/Microsoft/Windows/Fonts/")
loadfonts(device = "win")

set.seed(43)

x <- rnorm(100, mean = 0, sd = 2)
norm_y <- 0.7 * x + rnorm(100, sd = 1)

quad_y <- x^2 + rnorm(100, sd = 1)

data <- data.frame(x, norm_y, quad_y = (-quad_y - min(-quad_y)))

data <- data[-c(100),]

ggplot(data = data, aes(x = x, y = norm_y)) + 
  geom_point() + 
  geom_smooth(color="red", method = "lm", se = F) + 
  theme_minimal() + 
  ggsci::scale_color_tron() + 
  theme(legend.title = element_blank(), 
        axis.title = element_blank(),
        text = element_text(family = "LM Roman 10"), 
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        axis.text = element_blank())

ggplot(data = data, aes(x = x, y = quad_y)) + 
  geom_point() + 
  geom_smooth(color = "red", method = "lm", se = F) + 
  theme_minimal() + 
  ggsci::scale_color_tron() + 
  theme(legend.title = element_blank(), 
        axis.title = element_blank(),
        text = element_text(family = "LM Roman 10"), 
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        axis.text = element_blank())
