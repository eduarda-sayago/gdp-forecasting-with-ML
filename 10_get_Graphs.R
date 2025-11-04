# ================================================
# --------------------Graphs----------------------
# ================================================

library(ggplot2)
library(dplyr)
library(ggthemes)
library(tidyr)
library(stringr)
library(showtext)
library(sysfonts)

sysfonts::font_add_google("EB Garamond")
showtext::showtext_auto()

# ================================================
# ----------------------GDP-----------------------
# ================================================
# 2 graphs with both gdp series, one on top of the other
dfpib <- data.frame(date = raw_gdp$date, pib_rs = raw_gdp$pib_rs)
dfibc <- data.frame(date = rawm_ibc$date, ibc_rs = rawm_ibc$ibc_rs)

my_colors <- c("GDP (RS)" = "#0C4C8A", "IBC (RS)" = "#E69F00")

# === GDP (top) ===
p_gdp <- ggplot(dfpib, aes(x = date, y = pib_rs)) +
  geom_line(color = my_colors["GDP (RS)"], size = 0.9, na.rm = TRUE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "GDP") +
  theme_bw(base_size = 14, base_family = "EB Garamond") +
  theme(
    plot.margin = margin(4, 8, 2, 8),
    axis.title.y = element_text(size = 50, face = "plain"),
    axis.text.y  = element_text(size = 50),
    axis.text.x  = element_text(size = 40, angle = -90, vjust = 0.5, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )
print(p_gdp)

# === IBC (bottom) ===
p_ibc <- ggplot(dfibc, aes(x = date, y = ibc_rs)) +
  geom_line(color = my_colors["IBC (RS)"], size = 0.9, na.rm = TRUE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "IBCR-RS") +
  theme_bw(base_size = 14, base_family = "EB Garamond") +
  theme(
    plot.margin = margin(4, 8, 2, 8),
    axis.title.y = element_text(size = 50, face = "plain"),
    axis.text.y  = element_text(size = 50),
    axis.text.x  = element_text(size = 40, angle = -90, vjust = 0.5, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )
print(p_ibc)

ggsave("Plots/GDP_plot.png", p_gdp, width = 8, height = 2, dpi = 300)
ggsave("Plots/IBC_plot.png", p_ibc, width = 8, height = 2, dpi = 300)

rm(dfpib, dfibc, p_gdp, p_ibc)

# ================================================
# ----------------CSFE - Monthly------------------
# ================================================

CSFE_m <- csfe_m %>%
  pivot_longer(
    cols = -date,
    names_to = c("model", "horizon"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = horizon,
    values_from = value,
    names_prefix = "csfe_"
  )

# -monthly--------------------h1
csfe_mh1 <- CSFE_m %>%
  ggplot(aes(x=date, y= csfe_h1, color = model)) +
  geom_line(linewidth=0.8) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Horizon 1",
       subtitle =  "Data from Sep/2018 to May/2025",
       x = "Date",
       y = "CSFE",
       color = "Model:") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_discrete(labels = c(
    "lasso" = "Lasso",
    "enet" = "Elastic Net",
    "rf" = "Random Forest"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw(base_size = 16, base_family = "EB Garamond") +
  theme(
    plot.margin = margin(4, 8, 2, 8),
    axis.title.y = element_text(size = 60, face = "plain"),
    axis.text.y  = element_text(size = 60),
    axis.text.x  = element_text(size = 60, angle = -90, vjust = 0.5, hjust = 1),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 70, face = "plain"),
    plot.subtitle = element_text(size = 55, face = "plain"),
    legend.title = element_text(size = 60, face = "plain"),
    legend.text  = element_text(size = 55),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.key.width = unit(1, "cm"),   # ⬅️ Increases the width of each legend key
    legend.key.height = unit(1, "cm"),  # ⬅️ Increases vertical space of keys
    legend.box.spacing = unit(1, "cm") 
  )
print(csfe_mh1)

# -monthly--------------------h12
csfe_mh12 <- CSFE_m %>%
  ggplot(aes(x=date, y= csfe_h12, color = model)) +
  geom_line(linewidth=0.8) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Horizon 12",
       subtitle =  "Data from Sep/2018 to May/2025",
       x = "Date",
       y = "CSFE",
       color = "Model:") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_discrete(labels = c(
    "lasso" = "Lasso",
    "enet" = "Elastic Net",
    "rf" = "Random Forest"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw(base_size = 16, base_family = "EB Garamond") +
  theme(
    plot.margin = margin(4, 8, 2, 8),
    axis.title.y = element_text(size = 60, face = "plain"),
    axis.text.y  = element_text(size = 60),
    axis.text.x  = element_text(size = 60, angle = -90, vjust = 0.5, hjust = 1),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 70, face = "plain"),
    plot.subtitle = element_text(size = 55, face = "plain"),
    legend.title = element_text(size = 60, face = "plain"),
    legend.text  = element_text(size = 55),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.key.width = unit(1, "cm"),   # ⬅️ Increases the width of each legend key
    legend.key.height = unit(1, "cm"),  # ⬅️ Increases vertical space of keys
    legend.box.spacing = unit(1, "cm") 
  )
print(csfe_mh12)

# -monthly--------------------save

ggsave("Plots/Csfe - Mh1.png", csfe_mh1, width = 8, height = 6, dpi = 225)
ggsave("Plots/Csfe - Mh12.png", csfe_mh12, width = 8, height = 6, dpi = 225)

rm(CSFE_m, csfe_mh1, csfe_mh12)

# ================================================
# ---------------CSFE - Quarterly-----------------
# ================================================

CSFE_q <- csfe_q %>%
  pivot_longer(
    cols = -date,
    names_to = c("model", "horizon"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = horizon,
    values_from = value,
    names_prefix = "csfe_"
  )

# -quarterly--------------------h1
csfe_qh1 <- CSFE_q %>%
  ggplot(aes(x=date, y= csfe_h1, color = model)) +
  geom_line(linewidth=0.8) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Horizon 1",
       subtitle =  "Data from 2Q2018 to 1Q2025",
       x = "Date",
       y = "CSFE",
       color = "Model:") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_discrete(labels = c(
    "lasso" = "Lasso",
    "enet" = "Elastic Net",
    "rf" = "Random Forest"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw(base_size = 16, base_family = "EB Garamond") +
  theme(
    plot.margin = margin(4, 8, 2, 8),
    axis.title.y = element_text(size = 60, face = "plain"),
    axis.text.y  = element_text(size = 60),
    axis.text.x  = element_text(size = 60, angle = -90, vjust = 0.5, hjust = 1),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 70, face = "plain"),
    plot.subtitle = element_text(size = 55, face = "plain"),
    legend.title = element_text(size = 60, face = "plain"),
    legend.text  = element_text(size = 55),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.key.width = unit(1, "cm"),   # ⬅️ Increases the width of each legend key
    legend.key.height = unit(1, "cm"),  # ⬅️ Increases vertical space of keys
    legend.box.spacing = unit(1, "cm") 
  )

print(csfe_qh1)

# -quarterly--------------------h4
csfe_qh4 <- CSFE_q %>%
  ggplot(aes(x=date, y= csfe_h4, color = model)) +
  geom_line(linewidth=0.8) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Horizon 4",
       subtitle =  "Data from 2Q2018 to 1Q2025",
       x = "Date",
       y = "CSFE",
       color = "Model:") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_discrete(labels = c(
    "lasso" = "Lasso",
    "enet" = "Elastic Net",
    "rf" = "Random Forest"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw(base_size = 16, base_family = "EB Garamond") +
  theme(
    plot.margin = margin(4, 8, 2, 8),
    axis.title.y = element_text(size = 60, face = "plain"),
    axis.text.y  = element_text(size = 60),
    axis.text.x  = element_text(size = 60, angle = -90, vjust = 0.5, hjust = 1),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 70, face = "plain"),
    plot.subtitle = element_text(size = 55, face = "plain"),
    legend.title = element_text(size = 60, face = "plain"),
    legend.text  = element_text(size = 55),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.key.width = unit(1, "cm"),   # ⬅️ Increases the width of each legend key
    legend.key.height = unit(1, "cm"),  # ⬅️ Increases vertical space of keys
    legend.box.spacing = unit(1, "cm") 
  )

print(csfe_qh4)


# -quarterly--------------------save

ggsave("Plots/Csfe - Qh1.png", csfe_qh1, width = 8, height = 6, dpi = 225)
ggsave("Plots/Csfe - Qh4.png", csfe_qh4, width = 8, height = 6, dpi = 225)

rm(CSFE_q, csfe_qh1, csfe_qh4)