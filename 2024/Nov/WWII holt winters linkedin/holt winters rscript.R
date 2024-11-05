# Libraries --------------------------------------------------------------------
library(tidyverse)
library(zoo)
library(ggtext)
library(patchwork)
library(gganimate)
library(forecast)
library(showtext)
library(viridis)
library(DT)
library(knitr)
library(pander)
library(glue)


# Preparing all Captions ----------------------------------------------------
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = "../fontawesome/otfs/Font Awesome 6 Brands-Regular-400.otf")

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()

github_icon <- "&#xf09b"
linked_icon <- "&#xf08c"
github_username <- "Tonadeleon"

gitblue <- glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
  <span style='color: steelblue'>{github_username}</span>")

linblue <- glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{linked_icon};</span>
  <span style='color: steelblue'>{github_username}</span>")

gitblack <- glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
  <span style='color: black'>{github_username}</span>")

linblack <- glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{linked_icon};</span>
  <span style='color: black'>{github_username}</span>")

iconblack <- glue("**Graphic:**"," ", gitblack, " ", linblack) 

iconblue <- glue("**Graphic:**"," ", gitblue, " ", linblue)

caption <- glue("**Data**: International Air Transport Association Passenger Records")

gifcaption <- ("<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span> &nbsp; <span style='font-family:\"Font Awesome 6 Brands\";'>{linked_icon};</span> &nbsp; <span style='color: white'>**{github_username}**</span>")

gif2caption <- ("<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span> &nbsp; <span style='font-family:\"Font Awesome 6 Brands\";'>{linked_icon};</span> &nbsp; <span style='color: black'>**{github_username}**</span>")


# Reading and Procsessing Data -------------------------------------------------
url <- 'https://raw.githubusercontent.com/jbrownlee/Datasets/master/airline-passengers.csv'
data <- read_csv(url)
ts_data <- ts(data$Passengers, start = c(1949, 1), frequency = 12)


# Decomposing Fitting Holt Winters and Forecasting -----------------------------
decomposed <- decompose(ts_data, type = "multiplicative")
hw_model <- HoltWinters(ts_data, seasonal = "multiplicative")
forecast_hw <- forecast(hw_model, h = 24)


# Original Series --------------------------------------------------------------
title <- glue("**Airline Passengers**")
subtitle <- "When a time series shows both a **trend** and **seasonality**, and you want to **forecast** future behavior, the **Holt-Winters** method is a classic approach. As you can see in the series below, there is an upward trend along with a quasi-predictable, growing seasonality. Let's dive deeper and decompose the series to confirm the presence of these components."
subtitle <- paste0(subtitle, "<br>", caption, "<br>", iconblue)

data$Date <- as.Date(as.yearmon(time(ts_data)))
ggplot(data, aes(x = Date, y = ts_data)) +
  geom_line(linewidth = 1.5) +
  theme_minimal() +
  labs(title = title,
       subtitle = subtitle,
       x = "Year",
       y = "Passengers") +
  theme_minimal(base_size = 26, base_family = "roboto") +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(colour = "black", hjust = 0, halign = 0, margin = margin(b = 5, t = 5),lineheight = 0.5, size = rel(1.4), family = "roboto"
    ),
    plot.subtitle = element_textbox_simple(colour = "grey20", hjust = 0, halign = 0, margin = margin(b = 10, t = 10),lineheight = 1, family = "roboto", size = rel(.8)
    ),
    plot.caption = element_textbox_simple(colour = "grey20", hjust = 1, halign = 0, family = "roboto", size = rel(0.65), margin = margin(t = 10, b = 5), lineheight = 1
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(
      lineheight = 0.4, margin = margin(t = -5, b = -10), size = rel(.6),
    ),
    axis.title.y = element_text(
      lineheight = .5, angle = 0, face = "bold", vjust = .99, margin = margin(r = -71), size = rel(.65),
    ),
    axis.title.x = element_text(lineheight = .5, angle = 0, face = "bold", vjust = -1, size = rel(.65),
    ),
    legend.position = "none"
  )

# Decomposed Series ------------------------------------------------------------
title <- glue("**Decomposed Series**")
subtitle <- glue("Since the data exhibits exponential growth, a multiplicative Holt-Winters model will better capture its structure. After applying this decomposition, we observe a clear <span style=\"color: #FF8C00;\">**growing trend**</span> in the time series. Additionally, there is a <span style=\"color: #8FBC8F;\">**consistent seasonal pattern**</span>, which correlates strongly with varying travel seasons throughout the years. Given the **small error term** in the model, we can anticipate a good fit, indicating that this approach will likely provide reliable forecasts for future observations.")
subtitle <- paste0(subtitle, "<br>", caption, "<br>", iconblack)

decomposed_df <- data.frame(
  Date = as.Date(as.yearmon(time(decomposed$seasonal))),
  Seasonal = decomposed$seasonal,
  Trend = decomposed$trend,
  Remainder = decomposed$random
)

trend <- ggplot(decomposed_df, aes(x = Date)) +
  geom_line(aes(y = Trend), size = 1, color = "orange") +
  labs(
    title = "Trend") +
  theme_minimal() + 
  theme(
    axis.title = element_blank(),
    plot.title = element_text(family = "robotp"))


season <- ggplot(decomposed_df, aes(x = Date)) +
  geom_line(aes(y = Seasonal), size = 1, color = "darkseagreen") +
  labs(
    title = "Seasonal Component") +
  theme_minimal() + 
  theme(
    axis.title = element_blank(),
    plot.title = element_text(family = "robotp"))


err <- ggplot(decomposed_df, aes(x = Date)) +
  geom_line(aes(y = Remainder), size = 1, color = "black") +
  labs(
    title = "Error Term") +
  theme_minimal() + 
  theme(
    axis.title = element_blank(),
    plot.title = element_text(family = "robotp"))

all <- (trend / season / err) 

all + plot_annotation(
    title = title,
    subtitle = subtitle,
    theme = theme_minimal(base_family = "roboto", base_size = 26) +
      theme(
        plot.margin = margin(5, 5, 5, 5),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.title.position = "plot",
        plot.title = element_textbox_simple(
          colour = "black",
          hjust = 0,
          halign = 0,
          margin = margin(b = 10, t = 5),
          lineheight = 0.5,
          size = rel(1.4),
          family = "robotoslab",
          face="bold"),
        plot.subtitle = element_textbox_simple(
          colour = "grey20",
          hjust = 0,
          halign = 0,
          margin = margin(b = 20, t = 10),
          lineheight = 1,
          family = "roboto",
          size = rel(1)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(lineheight = 0.4, margin = margin(t = -5, b = -10), size = rel(.8)),
        axis.title.y = element_text(lineheight = 0.5, angle = 0, face = "bold", vjust = 1.05, margin = margin(r = -17, t = 20), size = rel(.9)),
        axis.title.x = element_text(lineheight = 0.5, angle = 0, face = "bold", vjust = -1, size = rel(.9)),
        legend.position = "none")
  )


# Forecast Plot ----------------------------------------------------------------
n_hist <- length(ts_data)
n_forecast <- length(forecast_hw$mean)
slowdown_factor <- 3.5

historical_df <- data.frame(
  Date = as.Date(as.yearmon(time(ts_data))),
  Passengers = as.numeric(ts_data),
  Lower80 = NA,
  Upper80 = NA,
  Lower95 = NA,
  Upper95 = NA,
  ForecastLabel = NA,
  Time = 1:n_hist,
  color = 1
)

forecast_df <- data.frame(
  Date = as.Date(as.yearmon(time(forecast_hw$mean))),
  Passengers = as.numeric(forecast_hw$mean),
  Lower80 = as.numeric(forecast_hw$lower[,1]),
  Upper80 = as.numeric(forecast_hw$upper[,1]),
  Lower95 = as.numeric(forecast_hw$lower[,2]),
  Upper95 = as.numeric(forecast_hw$upper[,2]),
  ForecastLabel = "Forecast",
  color=0,
  Time = max(1:n_hist) + cumsum(rep(slowdown_factor, n_forecast))
)

combined_df <- rbind(historical_df, forecast_df) |> 
  mutate(
    date = as.numeric(Date),
    colors = scale(Passengers - date)[, 1]
  )

title <- glue("**Holt-Winters Forecasting**")
subtitle <- glue("Compare <span style=\"color: #4fa7a5;\">**the observed series**</span> (1949-1960) to its <span style=\"color: black;\">**two-year forecast**</span> (1961-1962).")
subtitle <- paste0(subtitle, "<br>",caption, "<br>", iconblack)

ggplot(combined_df, aes(x = Date, y = Passengers)) +
  geom_ribbon(
    aes(ymin = Lower95, ymax = Upper95), 
    fill = "grey80") +
  geom_line(
    aes(color = color), size = 1.5) +
  scale_color_gradient(
    low = "black",
    high = "#4fa7a5") +
  labs(title = title,
       subtitle = subtitle,
       x = "Year",
       y = "Passengers") +
  theme_minimal(base_size = 30, base_family = "roboto") +
  theme(
    plot.margin = margin(b=10, t=5, l=2, r=2),
    plot.background = element_rect(
      fill = "white", 
      color = "white"),
    panel.background = element_rect(
      fill = "white", 
      color = "white"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      colour = "black",
      hjust = 0, 
      halign = 0, 
      margin = 
        margin(b = 10, t = 5),
      lineheight = 0.5, 
      size = rel(1.5), 
      family = "roboto"
    ),
    plot.subtitle = element_textbox_simple(
      colour = "grey20", 
      hjust = 0, 
      halign = 0, 
      margin = margin(b = 50, t = 10),
      lineheight = .65, 
      family = "roboto", 
      size = rel(1)
    ),
    plot.caption = element_textbox_simple(
      colour = "grey20", 
      hjust = 1, 
      halign = 0, 
      family = "roboto", 
      size = rel(1), 
      margin = margin(t = 10, b = 5), 
      lineheight = .65
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linewidth = .65),
    axis.text = element_text(
      lineheight = 0.4, 
      margin = margin(t = -5, b = -10), 
      size = rel(.75)
    ),
    axis.title.y = element_text(
      lineheight = .5, 
      angle = 0, 
      face = "bold", 
      vjust = 1.1, 
      margin = margin(r = -50), 
      size = rel(.85),
    ),
    axis.title.x = element_text(
      lineheight = .5, 
      angle = 0, 
      face = "bold", 
      vjust = -1, 
      size = rel(.85),
    ),
    legend.position = "none"
  )


# Gif 1 ------------------------------------------------------------------------
historical_df <- data.frame(
  Date = as.Date(as.yearmon(time(ts_data))),
  Passengers = as.numeric(ts_data),
  Lower80 = NA,
  Upper80 = NA,
  Lower95 = NA,
  Upper95 = NA,
  ForecastLabel = NA,
  color = 0
)

forecast_df <- data.frame(
  Date = as.Date(as.yearmon(time(forecast_hw$mean))),
  Passengers = as.numeric(forecast_hw$mean),
  Lower80 = as.numeric(forecast_hw$lower[,1]),
  Upper80 = as.numeric(forecast_hw$upper[,1]),
  Lower95 = as.numeric(forecast_hw$lower[,2]),
  Upper95 = as.numeric(forecast_hw$upper[,2]),
  ForecastLabel = "Forecast",
  color = 1
)

combined_df <- rbind(historical_df, forecast_df) |>
  mutate(
    date = as.numeric(Date),
    colors = scale(Passengers - date)[, 1]
  )

combined_df <- rbind(combined_df, pause_df)
forecast_df <- combined_df |> filter(color == 1)
historical_df <- combined_df |> filter(color == 0)

combined_df <- rbind(historical_df, forecast_df) |>
  mutate(
    date = as.numeric(Date),
    colors = scale(Passengers - date)[, 1]
  )

loess_fit <- loess(Passengers ~ date, data = combined_df)
combined_df$LOESS <- predict(loess_fit, newdata = combined_df$date)

animated_plot <- ggplot(combined_df, aes(x = Date, y = Passengers)) +
  geom_line(data = combined_df, aes(y = LOESS), color = "grey20", size = 1) +
  geom_ribbon(aes(ymin = Lower95, ymax = Upper95), fill = "#e6f2e8", alpha = 0.5) +
  geom_line(data = forecast_df, color = "white", size = 1.5) +
  geom_line(data = historical_df, aes(color = colors), size = 1.2) +
  scale_color_viridis_c(option = "mako", begin = .8, end = .5) +
  labs(
    subtitle = "Forecasting 2 Years with the Holt-Winters Method",
    title = "Airplane Passengers Over Time (1949-1960)",
    caption = gifcaption,
    x = "Year",
    y = "Passengers"
  ) +
  theme_void(base_size = 26, base_family = "roboto") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.title = element_text(colour = "white", face = "bold",
                              hjust = 0, margin = margin(b = 5, t = 5),
                              lineheight = 0.5, size = rel(1.2),
                              family = "robotoslab"
    ),
    plot.subtitle = element_text(colour = "grey80",
                                 hjust = 0, margin = margin(b = 10, t = 10),
                                 lineheight = 1, family = "roboto",
                                 size = rel(.8)
    ),
    plot.caption = element_textbox_simple(
      hjust = 0, colour = "white", size = rel(.6),
      margin = margin(t = 10),
      family = "roboto"
    ),
    #axis.line.y = element_line(color = "grey20"),
    #axis.line.x = element_line(color = "grey20"),
    axis.title.y = element_text(angle = 90, colour = "grey80", face = "bold",
                                hjust = .5, margin = margin(l = 10, r = 10),
                                lineheight = 1, family = "roboto",
                                size = rel(.6)),
    axis.title.x = element_text(colour = "grey80", face = "bold", hjust = .5,
                                margin = margin(b = 10, t = 10),
                                lineheight = 1, family = "roboto",
                                size = rel(.6)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", color = "black")
  ) +
  transition_reveal(Time) +
  view_follow(fixed_y = T) +
  geom_label(aes(label = ForecastLabel), vjust = -1.5, hjust = 0.5,
             color = "#358d93", fontface="bold", size = 5, na.rm = TRUE)

animated_gif <- animate(animated_plot, width = 1050, height = 650, fps = 60, duration = 11, renderer = gifski_renderer())
anim_save("animated_plot.gif", animation = animated_gif)


# Gif 2 ------------------------------------------------------------------------
set.seed(123)

historical_df <- data.frame(
  Date = as.Date(as.yearmon(time(ts_data))),
  Passengers = as.numeric(ts_data)
)

n_frames <- 100
alpha_end <- 0.2755
beta_end <- 0.03269
gamma_end <- 0.8707

alpha_seq <- c(
  seq(.99, .6, length.out = n_frames / 4),
  seq(.6, .3, length.out = n_frames / 4),
  seq(.3, .2, length.out = n_frames / 4),
  seq(.2, alpha_end, length.out = n_frames / 4)
)

beta_seq <- c(
  seq(.99, .6, length.out = n_frames / 4),
  seq(.6, .2, length.out = n_frames / 4),
  seq(.2, 0.01, length.out = n_frames / 4),
  seq(0.01, beta_end, length.out = n_frames / 4)
)

gamma_seq <- c(
  seq(.01, .5, length.out = n_frames / 4),
  seq(.5, .6, length.out = n_frames / 4),
  seq(.6, 0.9, length.out = n_frames / 4),
  seq(0.9, gamma_end, length.out = n_frames / 4)
)


fitted_list <- lapply(1:n_frames, function(i){
  hw <- HoltWinters(ts_data, alpha = alpha_seq[i], beta = beta_seq[i], gamma = gamma_seq[i], seasonal = "multiplicative")
  fitted <- as.numeric(hw$fitted[, "xhat"])
  data.frame(
    Frame = i,
    Date = as.Date(as.yearmon(time(ts_data))),
    Fitted = c(rep(NA, length(ts_data) - length(fitted)), fitted),
    Alpha = alpha_seq[i],
    Beta = beta_seq[i],
    Gamma = gamma_seq[i]
  )
})

fitted_df <- bind_rows(fitted_list)

animated_df <- fitted_df %>%
  left_join(historical_df, by = "Date")

p <- ggplot(animated_df, aes(x = Date)) +
  geom_line(
    aes(y = Passengers, color = "Observed"),
    size = 1.5
  ) +
  geom_line(
    aes(y = Fitted, color = "Fitted"),
    size = 1.3
  ) +
  geom_text(
    aes(x = min(Date) + years(4) + months(6),
        y = max(Passengers, na.rm = TRUE),
        label = "Observed"),
    hjust = 0,
    vjust = 1.2,
    size = 6,
    color = "#4fa7a5"
  ) +
  geom_text(
    aes(x = min(Date) + years(6),
        y = max(Passengers, na.rm = TRUE),
        label = "Vs"),
    hjust = 0,
    vjust = 1.2,
    size = 6,
    color = "grey30"
  ) +
  geom_text(
    aes(x = min(Date) + years(6) + months(6),
        y = max(Passengers, na.rm = TRUE),
        label = "Fitted"),
    hjust = 0,
    vjust = 1.2,
    size = 6,
    color = "black"
  ) +
  geom_text(
    aes(x = min(Date),
        y = max(Passengers, na.rm = TRUE),
        label = "Parameters:"),
    hjust = 0,
    vjust = 1.2,
    size = 6,
    color = "black"
  ) +
  geom_text(
    aes(x = min(Date),
        y = max(Passengers, na.rm = TRUE) - 35,
        label = paste0("Alpha: ", round(Alpha, 2))),
    hjust = 0,
    vjust = 1.2,
    size = 6,
    color = "steelblue4"
  ) +
  geom_text(
    aes(x = min(Date),
        y = max(Passengers, na.rm = TRUE) - 70,
        label = paste0("Beta: ", round(Beta, 2))),
    hjust = 0,
    vjust = 1.2,
    size = 6,
    color = "darkseagreen4"
  ) +
  geom_text(
    aes(
      x = min(Date),
      y = max(Passengers, na.rm = TRUE) - 105,
      label = paste0("Gamma: ", round(Gamma, 2))
    ),
    hjust = 0,
    vjust = 1.2,
    size = 6,
    color = "orange2"
  ) +
  scale_color_manual(
    name = "Series",
    values = c("Observed" = "#4fa7a5",
               "Fitted" = "black")) +
  labs(
    subtitle = "Take a look at how alpha, beta, and gamma play a role in Holt-Winters modeling.",
    title = "Demonstrating Performance of Holt-Winters Parameters",
    caption = gif2caption,
    x = "Year",
    y = "Passengers"
  ) +
  theme_minimal(
    base_size = 25,
    base_family = "roboto"
  ) +
  theme(
    axis.ticks = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.title = element_text(
      colour = "black",
      face = "bold",
      hjust = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      size = rel(1),
      family = "robotoslab"
    ),
    plot.subtitle = element_text(
      colour = "grey20",
      hjust = 0, margin = margin(b = 10, t = 10),
      lineheight = 1,
      family = "roboto",
      size = rel(.75)
    ),
    plot.caption = element_textbox_simple(
      hjust = 0, colour = "grey20", size = rel(.6),
      margin = margin(t = 10),
      family = "roboto"
    ),
    axis.title.y = element_text(
      angle = 90,
      colour = "grey20",
      face = "bold",
      hjust = .5,
      margin = margin(l = 10, r = 10),
      lineheight = 1, family = "roboto",
      size = rel(.6)),
    axis.title.x = element_text(
      colour = "grey20",
      face = "bold", hjust = .5,
      margin = margin(b = 10, t = 10),
      lineheight = 1, family = "roboto",
      size = rel(.6)),
    axis.text.x = element_text(
      colour = "grey20",
      lineheight = 1, family = "roboto",
      size = rel(.6)),
    axis.text.y = element_text(
      colour = "grey20",
      lineheight = 1, family = "roboto",
      size = rel(.6)),
    legend.position = "none",
  ) +
  transition_manual(Frame)

animation <- animate(p, nframes = n_frames, fps = 180, width = 1050, height = 650, end_pause = 25, duration = 12)
anim_save("holtwinters200.gif", animation = animation)

#-------------------------------------------------------------------------------
