# attainment_analysis.R
#
# Example analysis for the mini-OpenSAFELY school data platform.
#
# Produces two outputs:
#   output/attainment_summary.csv    — mean scores by school, subject, and PP status
#   output/school_caterpillar.png    — ranked school means with 95% CIs (caterpillar plot)
#
# Data read from dummy_data/ (supplied by the server, not the repo).

# ── Load data ──────────────────────────────────────────────────────────────────

pupils      <- read.csv("dummy_data/pupils.csv",      stringsAsFactors = FALSE)
assessments <- read.csv("dummy_data/assessments.csv", stringsAsFactors = FALSE)
schools     <- read.csv("dummy_data/schools.csv",     stringsAsFactors = FALSE)
mats        <- read.csv("dummy_data/mats.csv",        stringsAsFactors = FALSE)

cat("Loaded:", nrow(pupils), "pupils,", nrow(assessments), "assessments,",
    nrow(schools), "schools\n")

# ── Restrict to primary schools, Summer term, English assessments ──────────────

primary_ids <- schools$school_id[schools$school_type == "primary"]

dat <- assessments[
  assessments$school_id %in% primary_ids &
  assessments$term      == "Summer"      &
  assessments$subject   == "English",
]

# Convert raw score (out of 40) to a percentage
dat$score_pct <- dat$raw_score / 40 * 100

# Attach pupil characteristics
dat <- merge(dat,
             pupils[, c("pupil_id", "fsm", "sen_status", "pupil_premium")],
             by = "pupil_id", all.x = TRUE)

# Attach MAT name
dat <- merge(dat, schools[, c("school_id", "school_name")],
             by = "school_id", all.x = TRUE)
dat <- merge(dat, mats[, c("mat_id", "mat_name")], by = "mat_id", all.x = TRUE)

# ── Summary table: mean score by school and pupil premium status ───────────────

summary_list <- lapply(unique(dat$school_id), function(sid) {
  sub <- dat[dat$school_id == sid, ]
  pp_mean   <- mean(sub$score_pct[sub$pupil_premium == 1], na.rm = TRUE)
  npp_mean  <- mean(sub$score_pct[sub$pupil_premium == 0], na.rm = TRUE)
  data.frame(
    school_id    = sid,
    school_name  = sub$school_name[1],
    mat_name     = sub$mat_name[1],
    n_assessments = nrow(sub),
    mean_score_pp    = round(pp_mean,  1),
    mean_score_nonpp = round(npp_mean, 1),
    gap              = round(npp_mean - pp_mean, 1)
  )
})
summary_df <- do.call(rbind, summary_list)

dir.create("output", showWarnings = FALSE)
write.csv(summary_df, "output/attainment_summary.csv", row.names = FALSE)
cat("Written: output/attainment_summary.csv\n")

# ── Caterpillar plot: school-level mean scores ranked, with 95% CIs ────────────

# Compute mean and 95% CI per school (all pupils combined)
school_stats <- lapply(unique(dat$school_id), function(sid) {
  scores <- dat$score_pct[dat$school_id == sid]
  scores <- scores[!is.na(scores)]
  n      <- length(scores)
  if (n < 2) return(NULL)
  m  <- mean(scores)
  se <- sd(scores) / sqrt(n)
  data.frame(
    school_id  = sid,
    mat_name   = dat$mat_name[dat$school_id == sid][1],
    n          = n,
    mean       = m,
    lo95       = m - 1.96 * se,
    hi95       = m + 1.96 * se
  )
})
school_stats <- do.call(rbind, Filter(Negate(is.null), school_stats))

# Rank schools by mean score
school_stats <- school_stats[order(school_stats$mean), ]
school_stats$rank <- seq_len(nrow(school_stats))

# Colours by MAT
mat_names  <- unique(school_stats$mat_name)
mat_cols   <- c("#0066cc", "#cc4400", "#007744")
names(mat_cols) <- mat_names
cols <- mat_cols[school_stats$mat_name]

# Plot
png("output/school_caterpillar.png", width = 1400, height = 700, res = 130)

par(mar = c(4, 4.5, 3, 8), xpd = FALSE)

plot(
  school_stats$rank, school_stats$mean,
  ylim  = range(c(school_stats$lo95, school_stats$hi95), na.rm = TRUE),
  xlim  = c(0, nrow(school_stats) + 1),
  xlab  = "Schools ranked by mean score",
  ylab  = "Mean score (% of marks available)",
  main  = "Primary school attainment in English\n(Summer assessments 2023\u201324, with 95% confidence intervals)",
  pch   = 19, cex = 0.7,
  col   = cols,
  bty   = "l", las = 1
)

# Error bars
segments(
  school_stats$rank, school_stats$lo95,
  school_stats$rank, school_stats$hi95,
  col = cols, lwd = 1.2
)

# Grand mean line
abline(h = mean(school_stats$mean), lty = 2, col = "grey50")

# Legend (outside plot area)
par(xpd = TRUE)
legend(
  x      = nrow(school_stats) * 1.02,
  y      = max(school_stats$hi95, na.rm = TRUE),
  legend = mat_names,
  col    = mat_cols,
  pch    = 19, lwd = 1.5,
  bty    = "n", cex = 0.85,
  title  = "MAT"
)

dev.off()
cat("Written: output/school_caterpillar.png\n")

# ── Print summary to log ───────────────────────────────────────────────────────

cat("\n=== Attainment gap (non-PP minus PP, percentage points) ===\n")
cat(sprintf("  Mean gap across schools: %.1f pp\n", mean(summary_df$gap, na.rm = TRUE)))
cat(sprintf("  Range: %.1f to %.1f pp\n",
            min(summary_df$gap, na.rm = TRUE),
            max(summary_df$gap, na.rm = TRUE)))
cat("\n=== School score range ===\n")
cat(sprintf("  Lowest school mean:  %.1f%%\n", min(school_stats$mean)))
cat(sprintf("  Highest school mean: %.1f%%\n", max(school_stats$mean)))
cat(sprintf("  Grand mean:          %.1f%%\n", mean(school_stats$mean)))
