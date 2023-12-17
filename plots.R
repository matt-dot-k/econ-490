require(ggplot2)
require(ggthemes)

source("./code/analysis.R")

plot_theme <- theme(
    plot.subtitle = element_text(
        size = 10, hjust = 0),
    plot.caption = element_text(
        size = 10, hjust = 0),
    strip.text = element_text(
        size = 10, face = "bold"),
    axis.title.x = element_text(
        size = 10, face = "bold", angle = 0,
        margin = margin(t = 0.25, unit = "cm")),
    axis.title.y = element_text(
        size = 10, face = "bold", angle = 90,
        margin = margin(r = 0.25, unit = "cm")),
    axis.text = element_text(
        size = 10),
    plot.background = element_rect(
        fill = "#FFFFFF"),
    panel.background = element_rect(
        fill = "#FFFFFF"),
    panel.border = element_rect(
        color = "#000000", fill = NA, linewidth = 1),
    legend.background = element_rect(
        fill = "#FFFFFF"),
    legend.key = element_rect(
        fill = "#FFFFFF"),
    legend.title = element_text(
        size = 10, face = "bold"),
    legend.margin = margin(t = 0, unit = "cm"),
    legend.text = element_text(
        size = 10),
    legend.position = "bottom"
)

attack_colours <- c("Insurgency" = "coral1", "Terror" = "#3EB489")

attacks_chart <- ggplot(
    data = inherent_resolve,
    aes(x = month)
) +
    geom_line(
        aes(y = terror_attacks, color = "Terror"),
        linewidth = 1.2,
) +
    geom_line(
        aes(y = insurgency_actions, color = "Insurgency"),
        linewidth = 1.2
) +
    labs(
        x = "Month",
        y = "Attacks",
        color = "Attack Type",
        caption = "Source: Global Terrorism Database"
) +
    scale_color_manual(
        values = attack_colours
) +
    facet_wrap(
        ~ country
) +
    theme_fivethirtyeight(
) +
    plot_theme

country_colours <- c("Iraq" = "#3EB489", "Syria" = "coral1")

treatments <- c("Control" = "coral1", "Intervention" = "#3EB489")

facilities_chart <- ggplot(
    data = facilities_study,
    aes(x = time)
) +
    geom_vline(
        xintercept = 0,
        linewidth = 1.0,
        linetype = "dashed"
) +
    geom_point(
        aes(y = fit, color = group),
        size = 2.0,
        shape = 17,
) +
    geom_ribbon(
        aes(ymin = ci_low, ymax = ci_high, fill = group),
        alpha = 0.3
) +
    geom_line(
        aes(y = fit, color = group),
        linewidth = 1.2,
) +
    labs(
        x = "Time to Strike",
        y = "Attacks",
        color = "Time Period",
        caption = "Source: CJTF-OIR; Global Terrorism Database" # nolint
) +
    scale_color_manual(
        values = treatments
) +
    scale_fill_manual(
        guide = "none",
        values = treatments
) +
    facet_wrap(
        ~ target
) +
    theme_fivethirtyeight() +
    plot_theme

logistics_chart <- ggplot(
    data = logistics_study,
    aes(x = time)
) +
    geom_vline(
        xintercept = 0,
        linewidth = 1.0,
        linetype = "dashed"
) +
    geom_point(
        aes(y = fit, color = group),
        size = 2.0,
        shape = 17,
) +
    geom_ribbon(
        aes(ymin = ci_low, ymax = ci_high, fill = group),
        alpha = 0.3
) +
    geom_line(
        aes(y = fit, color = group),
        linewidth = 1.2,
) +
    labs(
        x = "Time to Strike",
        y = "IED/VBIED Attacks",
        color = "Time Period",
        caption = "Source: CJTF-OIR; Global Terrorism Database" # nolint
) +
    scale_color_manual(
        values = treatments
) +
    scale_fill_manual(
        guide = "none",
        values = treatments
) +
    facet_wrap(
        ~ target
) +
    theme_fivethirtyeight() +
    plot_theme
