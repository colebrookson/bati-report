## knight 1 models ===========================================================
knight1_lice <- glmmTMB::glmmTMB(
  all_lice ~ route * year + season + (1 | site_code),
  data = knight1_df,
  family = "nbinom2"
)
saveRDS(knight1_lice, here::here("./outputs/knight1-all-lice-model.rds"))

knight1_leps <- glmmTMB::glmmTMB(
  all_leps ~ route * year + season + (1 | site_code),
  data = knight1_df,
  family = "nbinom2"
)
saveRDS(knight1_leps, here::here("./outputs/knight1-all-leps-model.rds"))

knight1_lep_adults <- glmmTMB::glmmTMB(
  adult_leps ~ route * year + season + (1 | site_code),
  data = knight1_df,
  family = "nbinom2"
)
saveRDS(knight1_lep_adults, 
        here::here("./outputs/knight1-lep-adults-model.rds"))

knight1_lep_copes <- glmmTMB::glmmTMB(
  lep_cope ~ route * year + season + (1 | site_code),
  data = knight1_df,
  family = "nbinom2"
)
saveRDS(knight1_lep_copes, 
        here::here("./outputs/knight1-lep-copes-model.rds"))

## knight 2 models ===========================================================
knight2_leps <- glmmTMB::glmmTMB(
  all_leps ~ route * year + season + (1 | site_code),
  data = knight2_df,
  family = "nbinom2"
)
saveRDS(knight2_leps, here::here("./outputs/knight2-all-leps-model.rds"))

knight2_lep_adults <- glmmTMB::glmmTMB(
  adult_leps ~ route * year + season + (1 | site_code),
  data = knight2_df,
  family = "nbinom2"
)
saveRDS(knight2_lep_adults, 
        here::here("./outputs/knight2-lep-adults-model.rds"))

knight2_lep_copes <- glmmTMB::glmmTMB(
  lep_cope ~ route * year + season + (1 | site_code),
  data = knight2_df,
  family = "nbinom2"
)
saveRDS(knight2_lep_copes, 
        here::here("./outputs/knight2-lep-copes-model.rds"))

## wakeman models ============================================================
wakeman_leps <- glmmTMB::glmmTMB(
  all_leps ~ route * year + season + (1 | site_code),
  data = wakeman_df,
  family = "nbinom2"
)
saveRDS(wakeman_leps, here::here("./outputs/wakeman-all-leps-model.rds"))

wakeman_lep_adults <- glmmTMB::glmmTMB(
  adult_leps ~ route * year + season + (1 | site_code),
  data = wakeman_df,
  family = "nbinom2"
)
saveRDS(wakeman_lep_adults, 
        here::here("./outputs/wakeman-lep-adults-model.rds"))

wakeman_lep_copes <- glmmTMB::glmmTMB(
  lep_cope ~ route * year + season + (1 | site_code),
  data = wakeman_df,
  family = "nbinom2"
)
saveRDS(wakeman_lep_copes, 
        here::here("./outputs/wakeman-lep-copes-model.rds"))

