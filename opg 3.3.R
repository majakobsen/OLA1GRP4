# Opgave 3.3
BarPlot2 <- function() {
  lav_terningeplot(
    EnMillion,
    undertitel = "1.000.000 gentagelser",
    farve = "#258C85"
  )
}

# Vis figurerne
BarPlot1()
BarPlot2()

# Gem begge figurer
r_mappe <- "C:/Users/Lasse/Desktop/R/OLA-1"

ggsave(
  filename = file.path(r_mappe, "BarPlot1.png"),
  plot = BarPlot1(),
  width = 10,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(r_mappe, "BarPlot2.png"),
  plot = BarPlot2(),
  width = 10,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

