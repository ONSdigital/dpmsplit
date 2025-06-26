library(dplyr)
library(dpmaccpf)

set.seed(1)

counts_true <- readRDS("counts_true.rds")

sim_reported_im <- counts_true %>%
  slice(rep(1:n(), each = 2)) %>%
  data.frame(region_orig = rep("x")) %>%
  data.frame(region_dest = rep(c("a", "b"))) %>%
  mutate(
    count = rbinom(n = n(), size = internal, prob = 0.7),
  ) %>%
  select(-external, -internal)

save(sim_reported_im,
     file = "../data/sim_reported_im.rda",
     compress = "bzip2"
)
