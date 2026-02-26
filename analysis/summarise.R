# summarise.R
# Reads the generated dataset and produces two output tables:
#   output/condition_counts.csv  — prevalence of each condition
#   output/sex_summary.csv       — age and diabetes rate by sex

data <- read.csv("output/dataset.csv", stringsAsFactors = FALSE)

# --- Age (approximate, from date of birth) ---
data$dob <- as.Date(data$date_of_birth)
data$age <- as.integer(as.numeric(Sys.Date() - data$dob) / 365.25)

n <- nrow(data)

# --- Condition prevalence ---
condition_counts <- data.frame(
  condition = c("Diabetes", "Hypertension", "Asthma"),
  n         = c(sum(data$has_diabetes,     na.rm = TRUE),
                sum(data$has_hypertension, na.rm = TRUE),
                sum(data$has_asthma,       na.rm = TRUE))
)
condition_counts$percent <- round(condition_counts$n / n * 100, 1)

# --- Breakdown by sex ---
sex_summary <- do.call(rbind, lapply(c("female", "male"), function(s) {
  sub <- data[data$sex == s, ]
  data.frame(
    sex          = s,
    n            = nrow(sub),
    mean_age     = round(mean(sub$age, na.rm = TRUE), 1),
    pct_diabetes = round(mean(sub$has_diabetes, na.rm = TRUE) * 100, 1)
  )
}))

# --- Write outputs ---
write.csv(condition_counts, "output/condition_counts.csv", row.names = FALSE)
write.csv(sex_summary,      "output/sex_summary.csv",      row.names = FALSE)

cat("=== Condition prevalence (n =", n, "patients) ===\n")
print(condition_counts)
cat("\n=== Sex breakdown ===\n")
print(sex_summary)
