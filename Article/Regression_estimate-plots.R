# Time Plots 
library(broom.mixed)
# Sweden 

ESSSE_list <- list()

for (i in 1:8) {
  j <- i*7
  ESSSE_list[[i]] <- ESSSE |> filter(Bandwith <= j)
  names(ESSSE_list)[i] <- paste0("ESSSE_", i, "w")
}

list2env(ESSSE_list , .GlobalEnv)


# Trust 

models_list <- list()
for (i in 1:8) {
  model <-
    lm(Trust ~ Treatment, data = get(paste0("ESSSE_", i, "w")))
  models_list <- append(models_list, list(model))
}

models_list_i <- rev(models_list)

a <-
  plot_summs(
    models_list_i,
    scale = TRUE,
    colors = "Rainbow",
    point.shape = FALSE,
    ci_level = .95
  ) +
  xlim(-1, 2.5) +
  scale_color_grey() + theme_minimal() + ggplot2::coord_flip() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank()) +
  labs(title = "Trust in Police")

a


models_list <- list()
for (i in 1:8) {
  model <-
    lm(Obligation ~ Treatment, data = get(paste0("ESSSE_", i, "w")))
  models_list <- append(models_list, list(model))
}

models_list_i <- rev(models_list)


b <-
  plot_summs(
    models_list_i,
    scale = TRUE,
    colors = "Rainbow",
    point.shape = FALSE,
    ci_level = .95
  ) +
  xlim(-1, 2.5) +
  scale_color_grey() + theme_minimal() + ggplot2::coord_flip() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank()) +
  labs(title = "Obligation")

b


models_list <- list()
for (i in 1:8) {
  model <-
    lm(Effectiveness ~ Treatment, data = get(paste0("ESSSE_", i, "w")))
  models_list <- append(models_list, list(model))
}

models_list_i <- rev(models_list)


f <-
  plot_summs(
    models_list_i,
    scale = TRUE,
    colors = "Rainbow",
    point.shape = FALSE,
    ci_level = .95
  ) +
  xlim(-1, 2.5) +
  scale_color_grey() + theme_minimal() + ggplot2::coord_flip() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank()) +
  labs(title = "Police Effectiveness")

f

models_list <- list()
for (i in 1:8) {
  model <-
    lm(MoralAlignment ~ Treatment, data = get(paste0("ESSSE_", i, "w")))
  models_list <- append(models_list, list(model))
}

models_list_i <- rev(models_list)

c <-
  plot_summs(
    models_list_i,
    scale = TRUE,
    colors = "Rainbow",
    point.shape = FALSE,
    ci_level = .95
  ) +
  xlim(-1, 2.5) +
  scale_color_grey() + theme_minimal() + ggplot2::coord_flip() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank()) +
  labs(title = "Moral Alignment")

c

d <-
  plot_summs(
    models_list_i,
    scale = TRUE,
    colors = "Rainbow",
    point.shape = FALSE,
    ci_level = .95
  ) +
  xlim(-1, 2.5) +
  scale_color_grey() + theme_minimal()  + ggplot2::coord_flip() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank()) +
  labs(title = "Lawfulness")

d

models_list <- list()
for (i in 1:8) {
  model <-
    lm(ProceduralFairness ~ Treatment, data = get(paste0("ESSSE_", i, "w")))
  models_list <- append(models_list, list(model))
}

models_list_i <- rev(models_list)

e <-
  plot_summs(
    models_list_i,
    scale = TRUE,
    colors = "Rainbow",
    point.shape = FALSE,
    ci_level = .95
  ) +
  xlim(-1, 2.5) +
  scale_color_grey() + theme_minimal()  + ggplot2::coord_flip() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank()) +
  labs(title = "Procedural Fairness")

e

p <- plot_grid(a,
               b,
               c,
               d,
               e,
               f,
               nrow = 2,
               ncol = 3,
               align = "h")
ate_Swden <- p

ggsave("../Figures/ate_Swden.png")










# Time Plots 

# Russia 

ESSRU_list <- list()

for (i in 1:8) {
  j <- i*7
  ESSRU_list[[i]] <- ESSRU |> filter(Bandwith <= j)
  names(ESSRU_list)[i] <- paste0("ESSRU_", i, "w")
}

list2env(ESSRU_list , .GlobalEnv)


# Trust 

models_list <- list()
for (i in 1:8) {
  model <-
    lm(Trust ~ Treatment, data = get(paste0("ESSRU_", i, "w")))
  models_list <- append(models_list, list(model))
}

models_list_i <- rev(models_list)

a <-
  plot_summs(
    models_list_i,
    scale = TRUE,
    colors = "Rainbow",
    point.shape = FALSE,
    ci_level = .95
  ) +
  xlim(-2, 1) +
  scale_color_grey() + theme_minimal() + ggplot2::coord_flip() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank()) +
  labs(title = "Trust in Police")

a 


models_list <- list()
for (i in 1:8) {
  model <-
    lm(Obligation ~ Treatment, data = get(paste0("ESSRU_", i, "w")))
  models_list <- append(models_list, list(model))
}

models_list_i <- rev(models_list)


b <-
  plot_summs(
    models_list_i,
    scale = TRUE,
    colors = "Rainbow",
    point.shape = FALSE,
    ci_level = .95
  ) +
  xlim(-2, 1) +
  scale_color_grey() + theme_minimal() + ggplot2::coord_flip() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank()) +
  labs(title = "Obligation")

b


models_list <- list()
for (i in 1:8) {
  model <-
    lm(Effectiveness ~ Treatment, data = get(paste0("ESSRU_", i, "w")))
  models_list <- append(models_list, list(model))
}

models_list_i <- rev(models_list)

models_list_i <- rev(models_list)

f <-
  plot_summs(
    models_list_i,
    scale = TRUE,
    colors = "Rainbow",
    point.shape = FALSE,
    ci_level = .95
  ) +
  xlim(-2, 1) +
  scale_color_grey() + theme_minimal() + ggplot2::coord_flip() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank()) +
  labs(title = "Police Effectiveness")

f


models_list <- list()
for (i in 1:8) {
  model <-
    lm(MoralAlignment ~ Treatment, data = get(paste0("ESSRU_", i, "w")))
  models_list <- append(models_list, list(model))
}

models_list_i <- rev(models_list)

c <-
  plot_summs(
    models_list_i,
    scale = TRUE,
    colors = "Rainbow",
    point.shape = FALSE,
    ci_level = .95
  ) +
  xlim(-2, 1) +
  scale_color_grey() + theme_minimal() + ggplot2::coord_flip() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank()) +
  labs(title = "Moral Alignment")

c

models_list <- list()
for (i in 1:8) {
  model <-
    lm(Lawfulness ~ Treatment, data = get(paste0("ESSRU_", i, "w")))
  models_list <- append(models_list, list(model))
}

models_list_i <- rev(models_list)

d <-
  plot_summs(
    models_list_i,
    scale = TRUE,
    colors = "Rainbow",
    point.shape = FALSE,
    ci_level = .95
  ) +
  xlim(-2, 1) +
  scale_color_grey() + theme_minimal() + ggplot2::coord_flip() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank()) +
  labs(title = "Lawfulness")

d

models_list <- list()
for (i in 1:8) {
  model <-
    lm(ProceduralFairness ~ Treatment, data = get(paste0("ESSRU_", i, "w")))
  models_list <- append(models_list, list(model))
}

models_list_i <- rev(models_list)

e <-
  plot_summs(
    models_list_i,
    scale = TRUE,
    colors = "Rainbow",
    point.shape = FALSE,
    ci_level = .95
  ) +
  xlim(-2, 1) +
  scale_color_grey() + theme_minimal()  + ggplot2::coord_flip() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank()) +
  labs(title = "Procedural Fairness")

e

p <- plot_grid(a,
               b,
               c,
               d,
               e,
               f,
               nrow = 2,
               ncol = 3,
               align = "h")
ate_Russia <- p

ggsave("../Figures/ate_Russia.png")
