# install.packages("ggbump")
devtools::install_github("davidsjoberg/ggbump")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbump)

estados_br <- c(
  "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", 
  "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão", 
  "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará", 
  "Paraíba", "Paraná", "Pernambuco", "Piauí", "Rio de Janeiro", 
  "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima", 
  "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"
)

dados <- tibble(
  nome_uf = estados_br,
  nota_2023 = round(rnorm(27, mean = 6.2, sd = 1.1), 1),
  nota_2024 = round(nota_2023 + rnorm(27, mean = 0.2, sd = 0.6), 1)
) |> 
  mutate(
    nota_2023 = pmin(pmax(nota_2023, 0), 10),
    nota_2024 = pmin(pmax(nota_2024, 0), 10)
  ) |> 
  mutate(
    ranking_2023 = dense_rank(desc(nota_2023)),
    ranking_2024 = dense_rank(desc(nota_2024))
  ) |> 
  mutate(
    status = case_when(
      ranking_2024 < ranking_2023 ~ "SUBIU",
      ranking_2024 > ranking_2023 ~ "CAIU",
      TRUE                        ~ "SE MANTEVE"
    ),
    sinal = case_when(
      status == "SUBIU"      ~ "▲",
      status == "CAIU"       ~ "▼",
      status == "SE MANTEVE" ~ "—"
    )
  )

df_2023 <- dados |> 
  arrange(ranking_2023, desc(nota_2023), nome_uf) |> 
  mutate(
    y_2023 = row_number(),
    label_rank_2023 = paste0(ranking_2023, "º")
  )

df_2024 <- dados |> 
  arrange(ranking_2024, desc(nota_2024), nome_uf) |> 
  mutate(
    y_2024 = row_number(),
    label_rank_2024 = paste0(ranking_2024, "º")
  )

dados_grafico <- df_2023 |> 
  select(nome_uf, status, sinal, label_rank_2023, y_2023) |> 
  left_join(
    df_2024 |> select(nome_uf, label_rank_2024, y_2024),
    by = "nome_uf"
  )

 paleta <- c(
  "SUBIU"      = "#0284a5",
  "CAIU"       = "#9e1d47",
  "SE MANTEVE" = "#e0a32e"
)

ggplot(dados_grafico) +
  geom_sigmoid(
    aes(x = 1.3, xend = 8, y = y_2023, yend = y_2024, colour = status),
    linewidth = 1,
    alpha = 0.95,
    show.legend = FALSE,
    arrow = arrow(
      type = "open",
      length = unit(0.12, "inches")
    )
  ) +
  geom_text(
    aes(x = 0.2, y = y_2023, label = nome_uf),
    family = "Avenir Next Medium",
    size = 4.6,
    colour = "#1e293b",
    hjust = 1
  ) +
  geom_text(
    aes(x = 0.95, y = y_2023, label = label_rank_2023),
    size = 4.2,
    colour = "#64748b",
    hjust = 1
  ) +
  geom_text(
    aes(x = 9.10, y = y_2024, label = nome_uf),
    family = "Avenir Next Medium",
    size = 4.6,
    colour = "#1e293b",
    hjust = 0
  ) +
  geom_text(
    aes(x = 8.35, y = y_2024, label = label_rank_2024),
    size = 4.2,
    colour = "#64748b",
    hjust = 0
  ) +
  geom_text(
    aes(x = 12.7, y = y_2024, label = sinal, colour = status),
    show.legend = FALSE,
    fontface = "bold",
    hjust = 0.5
  ) + 
  coord_cartesian(clip = "off") +
  scale_y_reverse(breaks = NULL) +
  scale_x_continuous(
    breaks = c(1.3, 8),
    labels = c("2023", "2024"),
    limits = c(-2.5, 13.5),
    position = "top"
  ) +
  labs(
    title = "PIB\nEvolução do ranking",
    subtitle = "Unidades da Federação • 2023 → 2024\n",
    color = NULL
  ) + 
  theme_void(base_size = 14) +
  geom_segment(
    aes(x = 1.3, xend = 8, y = y_2023, yend = y_2024, colour = status),
    alpha = 0
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 1.1,
        alpha = 1
      )
    )
  ) +
  scale_color_manual(values = paleta) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 12, face = "bold", colour = "#1e293b"),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    plot.margin = margin(t = 30, r = 40, b = 30, l= 40),
    plot.title = element_text(size = 28, face = "bold", color = "#0f172a", 
                              lineheight = 1.1),
    plot.subtitle = element_text(size = 14, color = "#64748b"),
    axis.text.x.top = element_text(size = 20, face = "bold", 
                                   colour = "#1e293b", margin = margin(t = 20, b = 5))
    
  )
  
ggsave(last_plot(), file = "ranking.png", width = 10.5, height = 15, dpi = 300, 
       units = "in")
  














