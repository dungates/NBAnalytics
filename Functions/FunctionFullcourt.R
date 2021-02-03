# Adapted from Ed Küpfer
# https://gist.github.com/edkupfer
# https://gist.github.com/asteves/7266330

library(ggplot2)

court_themes = list(
  light = list(
    court = 'white',
    text = 'black'
  ),
  dark = list(
    court = 'gray20',
    text = 'white'
  )
)


fullcourt <- function(court_theme = court_themes$light) {
  palette(c(
    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"
  ))

  # Generate Data for the 3 point linea
  # Define the circle; add a point at the center if the 'pie slice' if the shape is to be filled
  circleFun <- function(center = c(0, 5.25), diameter = 20.9, npoints = 20000, start = 0, end = 1, filled = TRUE) {
    tt <- seq(start * pi, end * pi, length.out = npoints)
    df <- data.frame(
      y = center[1] + diameter / 2 * cos(tt),
      x = center[2] + diameter / 2 * sin(tt)
    )
    return(df)
  }

  halfCircle <- circleFun(c(0, 5.25), 20.9 * 2, start = 0, end = 1, filled = FALSE)
  ggplot(data = data.frame(y = 1, x = 1), aes(x, y)) +
    ### halfcourt line:
    geom_path(data = data.frame(x = c(47, 47), y = c(0, 50))) +
    ### outside box:
    geom_path(data = data.frame(y = c(0, 0, 50, 50, 0), x = c(0, 94, 94, 0, 0))) +
    ### solid FT semicircle above FT line:
    geom_path(data = data.frame(y = c((-6000:(-1) / 1000) + 25, (1:6000 / 1000) + 25), 
                                x = c(19 + sqrt(6^2 - c(-6000:(-1) / 1000, 1:6000 / 1000)^2))), aes(y = y, x = x)) +
    geom_path(data = data.frame(y = c((-6000:(-1) / 1000) + 25, (1:6000 / 1000) + 25), 
                                x = c(75 + sqrt(6^2 - c(-6000:(-1) / 1000, 1:6000 / 1000)^2))), aes(y = y, x = x)) +
    ### dashed FT semicircle below FT line:
    geom_path(data = data.frame(y = c((-6000:(-1) / 1000) + 25, (1:6000 / 1000) + 25), 
                                x = c(19 - sqrt(6^2 - c(-6000:(-1) / 1000, 1:6000 / 1000)^2))), 
              aes(y = y, x = x), linetype = "dashed") +
    geom_path(data = data.frame(y = c((-6000:(-1) / 1000) + 25, (1:6000 / 1000) + 25),
                                x = c(75 - sqrt(6^2 - c(-6000:(-1) / 1000, 1:6000 / 1000)^2))), 
              aes(y = y, x = x), linetype = "dashed") +
    ### key:
    geom_path(data = data.frame(y = c(17, 17, 33, 33, 17), x = c(0, 19, 19, 0, 0))) +
    geom_path(data = data.frame(y = c(17, 17, 33, 33, 17), x = c(94, 75, 75, 94, 94))) +
    ### box inside the key:
    geom_path(data = data.frame(y = c(19, 19, 31, 31, 19), x = c(0, 19, 19, 0, 0))) +
    geom_path(data = data.frame(y = c(19, 19, 31, 31, 19), x = c(94, 75, 75, 94, 94))) +
    ### restricted area semicircle:
    geom_path(data = data.frame(y = c((-4000:(-1) / 1000) + 25, (1:4000 / 1000) + 25), 
                                x = c(5.25 + sqrt(4^2 - c(-4000:(-1) / 1000, 1:4000 / 1000)^2))), aes(y = y, x = x)) +
    geom_path(data = data.frame(y = c((-4000:(-1) / 1000) + 25, (1:4000 / 1000) + 25), 
                                x = c(88.75 - sqrt(4^2 - c(-4000:(-1) / 1000, 1:4000 / 1000)^2))), aes(y = y, x = x)) +
    ### halfcourt semicircle:
    geom_path(data = data.frame(y = c((-6000:(-1) / 1000) + 25, (1:6000 / 1000) + 25), 
                                x = c(47 - sqrt(6^2 - c(-6000:(-1) / 1000, 1:6000 / 1000)^2))), aes(y = y, x = x)) +
    geom_path(data = data.frame(y = c((-6000:(-1) / 1000) + 25, (1:6000 / 1000) + 25), 
                                x = c(47 + sqrt(6^2 - c(-6000:(-1) / 1000, 1:6000 / 1000)^2))), aes(y = y, x = x)) +
    ### rim:
    geom_path(data = data.frame(y = c((-750:(-1) / 1000) + 25, (1:750 / 1000) + 25, (750:1 / 1000) + 25, (-1:-750 / 1000) + 25),
                                x = c(c(5.25 + sqrt(0.75^2 - c(-750:(-1) / 1000, 1:750 / 1000)^2)), 
                                      c(5.25 - sqrt(0.75^2 - c(750:1 / 1000, -1:-750 / 1000)^2)))), 
              aes(y = y, x = x), color = "#CF5300") +
    geom_path(data = data.frame(y = c((-750:(-1) / 1000) + 25, (1:750 / 1000) + 25, (750:1 / 1000) + 25, (-1:-750 / 1000) + 25),
                                x = c(c(88.75 + sqrt(0.75^2 - c(-750:(-1) / 1000, 1:750 / 1000)^2)), 
                                      c(88.75 - sqrt(0.75^2 - c(750:1 / 1000, -1:-750 / 1000)^2)))), 
              aes(y = y, x = x), color = "#CF5300") +
    ### backboard:
    geom_path(data = data.frame(y = c(22, 28), x = c(4, 4)), lineend = "butt", size = 2) +
    geom_path(data = data.frame(y = c(22, 28), x = c(90, 90)), lineend = "butt", size = 2) +
    ### three-point line:
    # geom_path(data=data.frame(y=c(-21,-21,-21000:(-1)/1000,1:21000/1000,21,21),x=c(0,169/12,5.25+sqrt(23.75^2-c(-21000:(-1)/1000,1:21000/1000)^2),169/12,0)),aes(y=y,x=x))+
    ### fiy aspect ratio to 1:1
    geom_path(data = halfCircle, aes(x = x, y = y + 25)) +
    ### Complete the three-point line
    geom_path(data = data.frame(y = c(4.1, 4.1, 45.9, 45.9), x = c(5.25, 0, 0, 5.25))) +
    geom_path(data = halfCircle, aes(x = 94 - x, y = y + 25)) +
    geom_path(data = data.frame(y = c(4.1, 4.1, 45.9, 45.9), x = c(88.75, 94, 94, 88.75))) +
    ### shot clock, all of these boxes are ordered right to left currently
    geom_path(data = data.frame(x = c(60+2, 65+2, 65+2, 60+2, 60+2), y = c(55, 55, 60, 60, 55)), size = 2) +
    ### game clock
    geom_path(data = data.frame(x = c(52.5+2, 60+2, 60+2, 52.5+2, 52.5+2), y = c(55, 55, 60, 60, 55)), size = 2) +
    ### quarter box
    geom_path(data = data.frame(x = c(45+2, 52.5+2, 52.5+2, 45+2, 45+2), y = c(55, 55, 60, 60, 55)), size = 2) +
    ### score and legend box
    geom_path(data = data.frame(x = c(28.5, 45+2, 45+2, 28.5, 28.5), y = c(55, 55, 60, 60, 55)), size = 2) +
    coord_fixed() +
    ### Clean up the Court
    theme_bw(base_family = "Roboto Condensed") +
    theme(
      text = element_text(family = "Roboto Condensed", color = court_theme$text),
      panel.grid = element_blank(),
      legend.title = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = c(0.416, 0.9),
      legend.direction = "horizontal",
      legend.text = element_text(face = "bold", color = court_theme$text),
      legend.background = element_rect(fill = NA),
      legend.key = element_blank(),
      plot.title = element_text(hjust = 0.391, vjust = -19, face = "bold", color = court_theme$text),
      plot.background = element_rect(fill = court_theme$court),
      panel.background = element_rect(fill = court_theme$court)
    )
}
