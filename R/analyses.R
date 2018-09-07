hydroMetrics %>% 
  ggplot(aes(x = cumQ, y = totalLoad)) +
  geom_point() +
  facet_wrap(~ analyte, scales = "free")

hydroMetrics %>% 
  inner_join(
    hysteresisIndices,
    by = c("stormMark", "analyte")
  ) %>% 
  ggplot(aes(x = FI, y = HImean)) + 
  lims(x = c(-1, 1), y = c(-1, 1)) +
  geom_vline(xintercept = 0, colour = "gray") +
  geom_hline(yintercept = 0, colour = "gray") +
  geom_point(aes(colour = monsoon)) +
  facet_wrap(~ analyte) +
  theme_minimal()

hydroMetrics %>% 
  inner_join(
    hysteresisIndices,
    by = c("stormMark", "analyte")
  ) %>% 
  inner_join(
    contributingGauges,
    by = c("stormMark")
  ) %>% 
  ggplot(aes(x = FI, y = HImean)) + 
  lims(x = c(-1, 1), y = c(-1, 1)) +
  geom_vline(xintercept = 0, colour = "gray") +
  geom_hline(yintercept = 0, colour = "gray") +
  geom_point(aes(colour = reachLength)) +
  facet_wrap(~ analyte) +
  theme_minimal()


hydroMetrics %>% 
  inner_join(
    hysteresisIndices,
    by = c("stormMark", "analyte")
  ) %>% 
  inner_join(
    contributingGauges,
    by = c("stormMark")
  ) %>% 
  select(totalLoad:fromInterceptor) %>% 
  pairs()

hydroMetrics %>% 
  inner_join(
    hysteresisIndices,
    by = c("stormMark", "analyte")
  ) %>% 
  inner_join(
    contributingGauges,
    by = c("stormMark")
  ) %>% 
  ggplot(aes(x = reachLength, y = totalLoad)) +
  geom_boxplot() +
  facet_wrap(~ analyte, scales = "free")