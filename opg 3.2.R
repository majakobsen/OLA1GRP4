#Opgave 3.2

#Definer en funktion som kaster med seks terninger.

SeksTerninger <- function () {
  die <- 1:6
  dice <- sample(die, size = 6, replace = TRUE)
  sum(dice)
}

#Definer en funktion som kaster de 6 terninger 10000 gange, og giver et output i barplot

library(ggplot2)

# Simulér summer af seks terninger

TiTusind <- replicate(10000, SeksTerninger())
EnMillion <- replicate(1000000, SeksTerninger())

# Fælles design gør figurerne nemme at sammenligne
lav_terningeplot <- function(resultater, undertitel, farve) {
  ggplot(data.frame(Sum = resultater), aes(x = Sum)) +
    geom_histogram(
      aes(y = after_stat(count / sum(count))),
      binwidth = 1,
      boundary = 0.5,
      fill = farve,
      colour = "white",
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = 21,
      colour = "#253447",
      linetype = "dashed",
      linewidth = 0.7
    ) +
    annotate(
      "text",
      x = 21.5,
      y = 0.111,
      label = "Forventet sum: 21",
      hjust = 0,
      colour = "#253447",
      size = 3.8
    ) +
    scale_x_continuous(
      breaks = seq(6, 36, by = 3),
      limits = c(5.5, 36.5),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = seq(0, 0.12, by = 0.02),
      expand = expansion(mult = c(0, 0))
    ) +
    coord_cartesian(ylim = c(0, 0.12)) +
    labs(
      title = "Summer omkring 21 forekommer oftest",
      subtitle = undertitel,
      x = "Summen af seks terninger",
      y = "Andel af forsøg (%)",
      caption = "Kilde: egen simulation i R · Seks fair terninger pr. forsøg"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 19,
        colour = "#253447",
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        size = 12,
        colour = "#586779",
        margin = margin(b = 18)
      ),
      plot.title.position = "plot",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        colour = "#E4E9EF",
        linewidth = 0.4
      ),
      axis.title = element_text(colour = "#253447"),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.text = element_text(colour = "#586779"),
      plot.caption = element_text(
        hjust = 0,
        size = 9,
        colour = "#687789",
        margin = margin(t = 16)
      ),
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      plot.margin = margin(22, 25, 18, 18)
    )
}

# Opgave 3.2
BarPlot1 <- function() {
  lav_terningeplot(
    TiTusind,
    undertitel = "10.000 gentagelser",
    farve = "#4386B5"
  )
}

