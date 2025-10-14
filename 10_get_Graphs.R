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
library(patchwork)

sysfonts::font_add_google("EB Garamond")
showtext::showtext_auto()

# ================================================
# ----------------------CSFE----------------------
# ================================================

CSFE_long <- CSFE_df %>%
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
csfe_mh1 <- CSFE_long %>%
  ggplot(aes(x=date, y= csfe_h1, color = model)) +
  geom_line(linewidth=0.8) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Horizon 1",
       subtitle =  "Data from Jan/2019 to May/2025",
       x = "Date",
       y = "CSFE",
       color = "Model") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_discrete(labels = c(
    "lasso" = "Lasso",
    "enet" = "Elastic Net",
    "rf" = "Random Forest"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw(base_size = 16, base_family = "EB Garamond")

# -monthly--------------------h12
csfe_mh12 <-CSFE_long %>%
  ggplot(aes(x=date, y= csfe_h12, color = model)) +
  geom_line(linewidth=0.8) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Horizon 12",
       subtitle =  "Data from Jan/2019 to May/2025",
       x = "Date",
       y = "CSFE",
       color = "Model") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_discrete(labels = c(
    "lasso" = "Lasso",
    "enet" = "Elastic Net",
    "rf" = "Random Forest"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw(base_size = 16, base_family = "EB Garamond")


# -monthly--------------------combine

month_CSFE <- csfe_mh1 + csfe_mh12 + plot_layout(ncol = 2) +
  plot_annotation(title = "Cumulative Squared Forecast Error (CSFE)")

# Save as vector (recommended for papers)
ggsave("plot_month_CSFE.pdf", combined, width = 10, height = 4)
