ubiplot <- function(scores, loadings) {

  X <- scores %>%
    as.data.frame() %>%
    mutate(observation = rownames(scores))

  ggfortify::ggbiplot(
    plot.data = X,
    loadings.data = loadings %>% as.data.frame(),
    loadings = TRUE,
    loadings.colour = "#9CC3D5FF",
    loadings.label = TRUE,
    label.label = "",
    loadings.label.size = 3,
    colour = "observation",
    loadings.label.colour = "#0063B2FF",
    loadings.label.label = rownames(loadings), alpha = .7,
  ) +
    geom_vline(xintercept = 0, alpha = 0.3) + geom_hline(yintercept = 0, alpha = 0.3)
}
