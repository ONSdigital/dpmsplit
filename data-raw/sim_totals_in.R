library(dplyr)
library(purrr)
library(dpmaccpf)

n_iter <- 10
set.seed(0)

counts_true <- readRDS("counts_true.rds")

perturb <- function(x) {
  ans <- rnorm(n = n_iter, mean = x, sd = 2 * sqrt(x + 2))
  ans <- pmax(ans, 0)
  ans <- as.integer(ans)
  ans
}

sim_totals_in <- counts_true %>%
  slice(rep(1:n(), each = 2)) %>%
  data.frame(region = rep(c("a", "b"))) %>%
  mutate(
    count = external + internal,
    count = map(count, perturb)
  ) %>%
  select(-external, -internal)

save(sim_totals_in,
  file = "../data/sim_totals_in.rda",
  compress = "bzip2"
)
