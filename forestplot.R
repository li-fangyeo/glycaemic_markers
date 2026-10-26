library(dplyr)
library(forestploter)
library(readxl)

#Table for common species among 3 glycaemic traits
daa_taxa26052026 <- read_excel("C:/Users/lifyeo/OneDrive - O365 Turun yliopisto/Manuscripts/Diabetes/daa_taxa26052026.xlsx")

sig <- daa_taxa26052026 %>%
  filter(q < 0.05)

g0   <- sig %>% filter(predictor == "Glucose (0 min)")
g2   <- sig %>% filter(predictor == "Glucose (120 min)")
homa <- sig %>% filter(predictor == "HOMA-IR")
hba1c <- sig %>% filter(predictor == "HbA1c")

common_species <- Reduce(
  intersect,
  list(g0$species, g2$species, homa$species)
)

common_rows <- sig %>%
  filter(species %in% common_species)

common_rows <- arrange(common_rows, by = species)

df <- common_rows %>% select(predictor, species, est, lwr_95, upr_95, q)
species_list <- df$species |> unique() |> as.character()

# 3. Header rows
header_rows <- tibble(
  predictor = species_list,
  species   = NA_character_,
  est       = NA_real_,
  lwr_95    = NA_real_,
  upr_95    = NA_real_,
  q         = NA_real_
)

# 4. Build final table
df_final <- species_list %>%
  lapply(function(sp) {
    bind_rows(
      header_rows %>% filter(predictor == sp),
      df %>% filter(species == sp)
    )
  }) %>%
  bind_rows()

# 5. Remove species column (numeric values remain intact)
df_final <- df_final %>%
  select(-species)

df_final

df_final <- df_final %>%
  mutate(
    predictor = recode(predictor,
                       "Glucose (0 min)" = "FG",
                       "Glucose (120 min)" = "PPG",
                       "HOMA-IR" = "IR")
  ) %>%
  rename(
    FDR = q)

df_final <- df_final %>%
  mutate(
    est     = round(est, 2),
    lwr_95  = round(lwr_95, 2),
    upr_95  = round(upr_95, 2),
    FDR       = round(FDR, 3)
  )
df_final <- df_final %>%
  mutate(
    FDR = ifelse(is.na(FDR), NA,
                 ifelse(FDR == 0, "<0.001", sprintf("%.3f", FDR)))
  )
#indent if there is a number at est
df_final$predictor <- ifelse(is.na(df_final$est), 
                             df_final$predictor,
                      paste0("   ", df_final$predictor))

df_final$` ` <- paste(rep(" ", 25), collapse = " ")

# Create a confidence interval column to display
df_final$`β (95% CI)` <- ifelse(is.na(df_final$est), "",
  sprintf("%.2f (%.2f to %.2f)",
          df_final$est, df_final$lwr_95, df_final$upr_95)
)


df_final$FDR <- ifelse(is.na(df_final$FDR), "", df_final$FDR)

df_final

#make the words center
tm <- forest_theme(base_size = 18,
                   rowhead=list(fg_params=list(hjust=0, x=0)),
                   colhead=list(fg_params=list(hjust=0.5, x=0.5)),
                   #core=list(fg_params=list(hjust=0.5, x=0.5)),
                   ci_Theight = 0.3,
                   ci_lwd = 2)

alpha_ft <- forest(df_final[,c(1, 7,6, 5)],
                   est = df_final$est,
                   lower = df_final$lwr_95, 
                   upper = df_final$upr_95,
                   #sizes = alpha$se,
                   ci_column = 3,
                   ref_line = 0,
                   xlim = c(-0.2, 0.2),
                   #x_trans = c("log10"),
                   xlab = "Beta",
                   theme = tm,
                   footnote = "FG, fasting glucose; PPG, postprandial glucose; IR, insulin resistance")

# Print plot
plot(alpha_ft)

# Save plots to PDF, specifying dimensions
pdf("commonspp.pdf", width = 15, height = 12)
print(alpha_ft)
dev.off()
