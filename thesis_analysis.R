###############################################################################
# DBI Master's Thesis R Analysis
# "Do Music Recommender Systems Shape Preferences?"
# Anja Urbanc | VU Amsterdam | DBI
###############################################################################

## ----------------------------- 0. SETUP ----------------------------------- ##
CSV_PATH <- "DBI Master Thesis - Music Streaming Platforms and Music Evaluation_June 29, 2026_02.22.csv"
OUT_DIR  <- "analysis_output"

needed <- c("tidyverse", "lme4", "lmerTest", "emmeans", "psych", "broom.mixed")
to_install <- needed[!(needed %in% rownames(installed.packages()))]
if (length(to_install) > 0) install.packages(to_install, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(tidyverse); library(lme4); library(lmerTest)
  library(emmeans);   library(psych); library(broom.mixed)
})
dir.create(OUT_DIR, showWarnings = FALSE)
sink_lines <- c()  # collects text for numbers_to_paste.txt
add <- function(...) sink_lines <<- c(sink_lines, sprintf(...))

## ------------------------ 1. READ RAW QUALTRICS DATASET ---------------------------- ##
# Qualtrics export: row 1 = column codes, row 2 = question text, row 3 = JSON.
# Read with the codes as names, then drop the two descriptive rows.
raw <- readr::read_csv("~/Downloads/DBI Master Thesis - Music Streaming Platforms and Music Evaluation_June 29, 2026_02.22.csv")
raw <- raw[-c(1, 2), ]            # drop question-text + import-id rows
add("Raw responses read: %d", nrow(raw))

## ------------------------ 2. LIKERT RECODING ------------------------------- ##
# Handles both "Agree" style and "7 = Strongly agree" style answers.
likert7 <- c(
  "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3,
  "Neither agree nor disagree" = 4,
  "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7
)
recode_likert <- function(x) {
  x <- as.character(x)
  # strip a leading "N = " prefix if present
  x <- str_trim(str_replace(x, "^[0-9]\\s*=\\s*", ""))
  out <- likert7[x]
  as.numeric(out)
}

## ------------------------ 3. EXCLUSIONS ------------------------------------ ##
# Pre-registered rules applied as a SEQUENTIAL waterfall, in the order reported
# in the thesis: consent -> age >=18 -> audio check -> completion -> recorded
# interface condition. Each count is the number of respondents *additionally*
# dropped at that step (i.e. among those who survived all earlier steps), so the
# counts are non-overlapping and sum exactly to the total dropped. Reporting them
# independently would double-count respondents who fail more than one check.
# Column codes: QID108 = consent, QID4 = age>=18, QID153 = audio check,
# Finished / Progress = completion, interface_condition = assigned player.
n0 <- nrow(raw)
dat <- raw %>%
  mutate(
    consent  = str_detect(coalesce(QID108, ""), regex("agree", ignore_case = TRUE)),
    age_ok   = str_detect(coalesce(QID4, ""),   regex("yes",   ignore_case = TRUE)),
    audio_ok = !is.na(QID153) & QID153 != "",
    finished = str_detect(coalesce(as.character(Finished), ""), regex("true", ignore_case = TRUE)) |
               coalesce(as.numeric(Progress), 0) >= 100,
    iface_ok = interface_condition %in% c("Spotify-style", "AppleMusic-style", "Neutral")
  )

# Sequential drops: evaluate each rule only on respondents still remaining.
step <- dat
n_consent <- sum(!step$consent);                       step <- step %>% filter(consent)
n_age     <- sum(!step$age_ok);                         step <- step %>% filter(age_ok)
n_audio   <- sum(!step$audio_ok);                       step <- step %>% filter(audio_ok)
n_incomp  <- sum(!step$finished);                       step <- step %>% filter(finished)
n_iface   <- sum(!step$iface_ok);                       step <- step %>% filter(iface_ok)
dat <- step

add("Excluded (sequential): consent=%d age=%d audio=%d incomplete=%d no_interface=%d",
    n_consent, n_age, n_audio, n_incomp, n_iface)
add("Total excluded: %d  (of %d raw responses)", n0 - nrow(dat), n0)
add("Final analysis sample (participants): %d", nrow(dat))

## ------------------------ 4. RESHAPE TO LONG FORMAT ------------------------------- ##
# Evaluation-block -> clip-number map, per interface condition.
# (Eval block N corresponds to clip N; the label for clip N is in
#  {SP|AM|CTRL}_clipN_label_shown; a blank label = the "None"/no-label clip.)
eval_blocks <- list(
  "Spotify-style"    = c("QID146","QID112","QID113","QID114","QID149","QID119"),
  "AppleMusic-style" = c("QID117","QID116","QID118","QID150","QID120","QID122"),
  "Neutral"          = c("QID125","QID130","QID129","QID128","QID126","QID145")
)
label_prefix <- c("Spotify-style" = "SP", "AppleMusic-style" = "AM", "Neutral" = "CTRL")
eval_item_names <- c("liked","taste_match","relevance","replay","save")

long_list <- list()
for (i in seq_len(nrow(dat))) {
  row  <- dat[i, ]
  cond <- row$interface_condition
  if (is.na(cond) || !(cond %in% names(eval_blocks))) next
  blocks <- eval_blocks[[cond]]
  pref   <- label_prefix[[cond]]
  for (clip in 1:6) {
    blk <- blocks[clip]
    items <- recode_likert(c(row[[paste0(blk,"_1")]], row[[paste0(blk,"_2")]],
                             row[[paste0(blk,"_3")]], row[[paste0(blk,"_4")]],
                             row[[paste0(blk,"_5")]]))
    lbl_raw <- row[[paste0(pref,"_clip",clip,"_label_shown")]]
    label   <- ifelse(is.na(lbl_raw) || lbl_raw == "" || tolower(lbl_raw) == "none",
                      "None", as.character(lbl_raw))
    pos     <- suppressWarnings(as.numeric(row[[paste0(pref,"_clip",clip,"_position")]]))
    listened<- suppressWarnings(as.numeric(row[[paste0(pref,"_clip",clip,"_total_listened_ms")]]))
    long_list[[length(long_list)+1]] <- tibble(
      pid = row$ResponseId, interface = cond, clip = paste0("clip", clip),
      label = label, position = pos, listened_ms = listened,
      liked = items[1], taste_match = items[2], relevance = items[3],
      replay = items[4], save = items[5]
    )
  }
}
long <- bind_rows(long_list) %>%
  mutate(
    label = factor(label, levels = c("None","New","Recommended")),  # None = reference
    interface = factor(interface, levels = c("Neutral","Spotify-style","AppleMusic-style")),
    clip = factor(clip), pid = factor(pid)
  )
add("Clip-level observations (long rows): %d", nrow(long))

## ------------------------ 5. SCORE SCALES ---------------------------------- ##
# 5 music-evaluation questions (per clip)
long <- long %>%
  rowwise() %>%
  mutate(eval_composite = mean(c(liked, taste_match, relevance, replay, save), na.rm = TRUE)) %>%
  ungroup()
alpha_eval <- tryCatch(
  psych::alpha(long[, eval_item_names], check.keys = FALSE)$total$raw_alpha,
  error = function(e) NA_real_)
add("Evaluation composite Cronbach's alpha: %.3f", alpha_eval)

# TIPI Big Five (QID31_1..10). Standard Gosling (2003) scoring.
# Pairs (item, reverse-item): E=1,6R  A=7,2R  C=3,8R  ES(=low N)=9,4R  O=5,10R
tipi <- dat %>%
  transmute(
    pid = ResponseId,
    i1 = recode_likert(QID31_1), i2 = recode_likert(QID31_2),
    i3 = recode_likert(QID31_3), i4 = recode_likert(QID31_4),
    i5 = recode_likert(QID31_5), i6 = recode_likert(QID31_6),
    i7 = recode_likert(QID31_7), i8 = recode_likert(QID31_8),
    i9 = recode_likert(QID31_9), i10 = recode_likert(QID31_10)
  ) %>%
  mutate(
    rev = 8,  # reverse formula: 8 - x for a 7-point scale
    extraversion      = rowMeans(cbind(i1, rev - i6), na.rm = TRUE),
    agreeableness     = rowMeans(cbind(i7, rev - i2), na.rm = TRUE),
    conscientiousness = rowMeans(cbind(i3, rev - i8), na.rm = TRUE),
    emot_stability    = rowMeans(cbind(i9, rev - i4), na.rm = TRUE),  # high = low neuroticism
    openness          = rowMeans(cbind(i5, rev - i10), na.rm = TRUE)
  ) %>%
  select(pid, extraversion, agreeableness, conscientiousness, emot_stability, openness)

# Recommendation attitudes (QID110_1..6). Item 5 is reverse-worded
# ("I prefer choosing music myself..."). Build a trust/reliance composite from 1-4.
att <- dat %>%
  transmute(
    pid = ResponseId,
    a1 = recode_likert(QID110_1), a2 = recode_likert(QID110_2),
    a3 = recode_likert(QID110_3), a4 = recode_likert(QID110_4),
    a5 = recode_likert(QID110_5), a6 = recode_likert(QID110_6)
  ) %>%
  mutate(rec_trust_reliance = rowMeans(cbind(a1, a2, a3, a4), na.rm = TRUE))
alpha_att <- tryCatch(
  psych::alpha(att[, c("a1","a2","a3","a4")], check.keys = FALSE)$total$raw_alpha,
  error = function(e) NA_real_)
add("Recommendation trust/reliance alpha: %.3f", alpha_att)

# Merge participant-level moderators (mean-centred for interactions)
mods <- tipi %>%
  left_join(att %>% select(pid, rec_trust_reliance), by = "pid") %>%
  mutate(across(c(openness, agreeableness, conscientiousness, emot_stability,
                  extraversion, rec_trust_reliance),
                ~ as.numeric(scale(., scale = FALSE)), .names = "{.col}_c"))
long <- long %>% left_join(mods, by = "pid")

## ------------------ 5b. PARTICIPANT-LEVEL FRAME + DEMOGRAPHICS ------------- ##
# One row per participant, joining moderators to the raw demographic / habit
# columns. Real Qualtrics column codes from this export:
# QID141 = age (open), QID142 = gender, QID143 = education, QID144 = country,
# QID152 = device, QID16 = streaming frequency, QID5 = most-used platform,
# QID151 = "heard any clips before?".
pdat <- dat %>%
  transmute(
    pid       = ResponseId,
    interface = factor(interface_condition,
                       levels = c("Neutral", "Spotify-style", "AppleMusic-style")),
    age       = suppressWarnings(as.numeric(QID141)),
    gender    = as.character(QID142),
    education = as.character(QID143),
    country   = as.character(QID144),
    device    = as.character(QID152),
    stream_freq = as.character(QID16),
    platform  = as.character(QID5),
    familiar  = as.character(QID151)   # "heard any clips before?" item
  ) %>%
  left_join(mods, by = "pid")

# --- Age ---
add("\n================ SAMPLE DESCRIPTIVES (Section 4.1) ================")
add("Age: M=%.1f, SD=%.1f, range %d-%d",
    mean(pdat$age, na.rm = TRUE), sd(pdat$age, na.rm = TRUE),
    suppressWarnings(min(pdat$age, na.rm = TRUE)),
    suppressWarnings(max(pdat$age, na.rm = TRUE)))

# --- Categorical demographics: counts + percentages ---
report_pct <- function(x, label) {
  tb <- sort(table(x[!is.na(x) & x != ""]), decreasing = TRUE)
  n  <- sum(tb)
  add("%s (n=%d):", label, n)
  for (lv in names(tb))
    add("  %-28s %d (%.1f%%)", lv, tb[[lv]], 100 * tb[[lv]] / n)
}
report_pct(pdat$gender,      "Gender")
report_pct(pdat$education,   "Education")
report_pct(pdat$device,      "Device")
report_pct(pdat$stream_freq, "Streaming frequency")
report_pct(pdat$platform,    "Most-used platform")
report_pct(pdat$familiar,    "Heard any clip before")
write_csv(pdat, file.path(OUT_DIR, "participant_level_data.csv"))

## ------------------ 5c. RANDOMIZATION CHECKS (Section 4.4) ---------------- ##
# One-way ANOVAs of each trait on interface group (between-participant factor),
# plus the group means reported in Table 6, and a chi-square test of streaming
# frequency by interface group.
add("\n================ RANDOMIZATION CHECKS (Section 4.4) ================")
balance_rows <- list()
trait_vars <- c(Openness = "openness", Agreeableness = "agreeableness",
                Extraversion = "extraversion", Conscientiousness = "conscientiousness",
                `Emotional stability` = "emot_stability")
for (nm in names(trait_vars)) {
  v <- trait_vars[[nm]]
  aov_fit <- aov(reformulate("interface", v), data = pdat)
  s <- summary(aov_fit)[[1]]
  Fval <- s$`F value`[1]; pval <- s$`Pr(>F)`[1]
  df1 <- s$Df[1]; df2 <- s$Df[2]
  gm <- pdat %>%
    group_by(interface) %>% summarise(M = mean(.data[[v]], na.rm = TRUE), .groups = "drop")
  getm <- function(g) round(gm$M[gm$interface == g], 2)
  add("  %-20s Neutral=%.2f Spotify=%.2f Apple=%.2f  F(%d,%d)=%.2f, p=%.3f",
      nm, getm("Neutral"), getm("Spotify-style"), getm("AppleMusic-style"),
      df1, df2, Fval, pval)
  balance_rows[[nm]] <- tibble(
    variable = nm, neutral_M = getm("Neutral"), spotify_M = getm("Spotify-style"),
    apple_M = getm("AppleMusic-style"), F = round(Fval, 2), df1 = df1, df2 = df2,
    p = round(pval, 3))
}
write_csv(bind_rows(balance_rows), file.path(OUT_DIR, "table6_balance_checks.csv"))

# Streaming frequency x interface (chi-square)
sf_tab <- table(pdat$stream_freq, pdat$interface)
sf_chi <- suppressWarnings(chisq.test(sf_tab))
add("  Streaming frequency x interface: chi2(%d)=%.2f, p=%.3f",
    sf_chi$parameter, sf_chi$statistic, sf_chi$p.value)

## ------------------ 5d. MANIPULATION CHECKS (Section 4.5) ---------------- ##
# Percentages of participants who noticed a label, were unsure, or did not
# notice; and the most common perceived label location. Real column codes:
# QID41 = "did you notice any label?", QID42 = "where did you notice the labels?".
add("\n================ MANIPULATION CHECKS (Section 4.5) ================")
mc <- dat %>% transmute(
  noticed  = as.character(QID41),
  location = as.character(QID42))
report_pct(mc$noticed,  "Label noticed")
report_pct(mc$location, "Perceived label location")

## ------------------------ 6. DESCRIPTIVES & CHECKS ------------------------- ##
desc <- long %>% group_by(label) %>%
  summarise(n = n(), M = mean(eval_composite, na.rm = TRUE),
            SD = sd(eval_composite, na.rm = TRUE), .groups = "drop")
write_csv(desc, file.path(OUT_DIR, "table4_descriptives_by_label.csv"))
add("\nEvaluation composite by label:")
for (k in seq_len(nrow(desc)))
  add("  %-12s n=%d  M=%.2f  SD=%.2f", desc$label[k], desc$n[k], desc$M[k], desc$SD[k])

# Within-participant balance check: each pid should have 2 of each label
bal <- long %>% count(pid, label) %>% pivot_wider(names_from = label, values_from = n)
write_csv(bal, file.path(OUT_DIR, "within_subject_label_balance.csv"))

## ------------------------ 7. MODELS ---------------------------------------- ##
report_contrast <- function(emm, name) {
  s <- summary(emm, infer = c(TRUE, TRUE))
  for (r in seq_len(nrow(s))) {
    add("  [%s] %s: b=%.3f, SE=%.3f, 95%% CI [%.3f, %.3f], p=%.4f",
        name, as.character(s$contrast[r]), s$estimate[r], s$SE[r],
        s$lower.CL[r], s$upper.CL[r], s$p.value[r])
  }
  as.data.frame(s)
}

add("\n================ PRIMARY MODEL (H1, H2) ================")
m_main <- tryCatch(
  lmer(eval_composite ~ label + position + (1 | pid) + (1 | clip), data = long,
       REML = TRUE, control = lmerControl(optimizer = "bobyqa")),
  error = function(e) { add("Primary model failed: %s", conditionMessage(e)); NULL })

if (!is.null(m_main)) {
  write_csv(broom.mixed::tidy(m_main), file.path(OUT_DIR, "table5_primary_model.csv"))
  emm <- emmeans(m_main, ~ label)
  # H1: Recommended vs None ; H2: Recommended vs New
  ct <- contrast(emm, method = list(
    "Recommended - None" = c(-1, 0, 1),
    "Recommended - New"  = c(0, -1, 1)))
  h12 <- report_contrast(ct, "H1/H2")
  write_csv(h12, file.path(OUT_DIR, "table5_H1_H2_contrasts.csv"))

  # Standardized effect size (Cohen's d) for each contrast: b / SD(eval_composite),
  # matching the d values reported in Table 8 (H1 d=0.05, H2 d=0.04).
  sd_eval <- sd(long$eval_composite, na.rm = TRUE)
  for (r in seq_len(nrow(h12)))
    add("  [d] %s: d = %.3f", as.character(h12$contrast[r]), h12$estimate[r] / sd_eval)

  # Estimated marginal means figure
  emm_df <- as.data.frame(emm)
  p <- ggplot(emm_df, aes(label, emmean)) +
    geom_col(width = .6, fill = "#0077B3") +
    geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = .15) +
    labs(x = "Recommendation label", y = "Estimated mean evaluation (1-7)",
         title = "Estimated marginal evaluation by label condition") +
    theme_minimal(base_size = 13)
  ggsave(file.path(OUT_DIR, "figure_eval_by_label.png"), p, width = 6, height = 4, dpi = 150)
}

# Per-item models
add("\n---- Per-item label effects (Recommended - None) ----")
peritem <- map_dfr(eval_item_names, function(it) {
  f <- as.formula(paste0(it, " ~ label + position + (1|pid) + (1|clip)"))
  mm <- tryCatch(lmer(f, data = long, control = lmerControl(optimizer = "bobyqa")),
                 error = function(e) NULL)
  if (is.null(mm)) return(tibble(item = it, estimate = NA))
  e <- emmeans(mm, ~ label)
  c2 <- as.data.frame(contrast(e, method = list("Rec-None" = c(-1,0,1))))
  tibble(item = it, estimate = c2$estimate, SE = c2$SE, p = c2$p.value)
})
write_csv(peritem, file.path(OUT_DIR, "table6_per_item.csv"))

add("\n================ MODERATION (H3a-H3c) ================")
mod_specs <- list(
  H3a_openness        = "openness_c",
  H3b_agreeableness   = "agreeableness_c",
  H3c_trust_reliance  = "rec_trust_reliance_c"
)
mod_rows <- list()
for (nm in names(mod_specs)) {
  v <- mod_specs[[nm]]
  f <- as.formula(paste0("eval_composite ~ label * ", v,
                         " + position + (1|pid) + (1|clip)"))
  mm <- tryCatch(lmer(f, data = long, control = lmerControl(optimizer = "bobyqa")),
                 error = function(e) { add("%s failed: %s", nm, conditionMessage(e)); NULL })
  if (!is.null(mm)) {
    td <- broom.mixed::tidy(mm) %>% filter(str_detect(term, ":"))
    for (r in seq_len(nrow(td)))
      add("  [%s] %s: b=%.3f, SE=%.3f, p=%.4f", nm, td$term[r], td$estimate[r], td$std.error[r],
          ifelse("p.value" %in% names(td), td$p.value[r], NA))
    mod_rows[[nm]] <- broom.mixed::tidy(mm) %>% mutate(model = nm)
  }
}
if (length(mod_rows)) write_csv(bind_rows(mod_rows), file.path(OUT_DIR, "table_moderation.csv"))

# --- Figure 4: agreeableness median-split cell means (Section 6.2) ---
# Participants at or above the median are "high" agreeableness; below are "low".
# (Median split uses >= to match the cell means reported in the thesis:
#  high Rec=4.08/None=3.72 ; low Rec=3.63/None=3.86.)
agr_med <- median(long$agreeableness, na.rm = TRUE)
long <- long %>% mutate(
  agr_group = if_else(agreeableness >= agr_med, "High agreeableness", "Low agreeableness"))
agr_cells <- long %>%
  group_by(agr_group, label) %>%
  summarise(M = mean(eval_composite, na.rm = TRUE),
            SE = sd(eval_composite, na.rm = TRUE) / sqrt(n()),
            n = n(), .groups = "drop")
write_csv(agr_cells, file.path(OUT_DIR, "figure4_agreeableness_cells.csv"))
add("\n---- Figure 4: agreeableness median-split cell means (median = %.2f) ----", agr_med)
for (r in seq_len(nrow(agr_cells)))
  add("  %-20s %-12s M=%.2f SE=%.2f (n=%d)",
      agr_cells$agr_group[r], as.character(agr_cells$label[r]),
      agr_cells$M[r], agr_cells$SE[r], agr_cells$n[r])

p_agr <- ggplot(agr_cells, aes(label, M, fill = agr_group)) +
  geom_col(width = .65, position = position_dodge(.7)) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),
                width = .15, position = position_dodge(.7)) +
  labs(x = "Recommendation label", y = "Mean evaluation (1-7)",
       fill = NULL, title = "Label x agreeableness (median split)") +
  theme_minimal(base_size = 13)
ggsave(file.path(OUT_DIR, "figure_agreeableness_interaction.png"),
       p_agr, width = 6.5, height = 4, dpi = 150)

add("\n================ EXPLORATORY: INTERFACE (RQ1) ================")
m_int <- tryCatch(
  lmer(eval_composite ~ label * interface + position + (1|pid) + (1|clip),
       data = long, control = lmerControl(optimizer = "bobyqa")),
  error = function(e) { add("Interface model failed: %s", conditionMessage(e)); NULL })
if (!is.null(m_int)) {
  write_csv(broom.mixed::tidy(m_int), file.path(OUT_DIR, "table_interface_RQ1.csv"))
  add("  Type-III test of label:interface interaction reported in table_interface_RQ1.csv")

  # Likelihood-ratio test of the label x interface interaction (RQ1):
  # compare full model (with interaction) vs reduced model (without), refit with
  # ML (REML = FALSE) as required for LRTs on fixed effects. Reproduces the
  # thesis value chi2(4) = 0.07, p > .99.
  m_int_ml  <- update(m_int, REML = FALSE)
  m_red_ml  <- lmer(eval_composite ~ label + interface + position + (1|pid) + (1|clip),
                    data = long, REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
  lrt <- anova(m_red_ml, m_int_ml)
  chisq <- lrt$Chisq[2]; chidf <- lrt$Df[2]; chip <- lrt$`Pr(>Chisq)`[2]
  add("  LRT label x interface: chi2(%d) = %.2f, p = %.3f", chidf, chisq, chip)
}

## ------------------------ 8. SECONDARY: LISTENING -------------------------- ##
# Descriptive mean listening time per clip (Section 6.4: M approx 29.7 s).
add("\n================ SECONDARY: LISTENING (Section 6.4) ================")
add("  Mean listening time per clip: M = %.1f s (SD = %.1f s)",
    mean(long$listened_ms, na.rm = TRUE) / 1000,
    sd(long$listened_ms,   na.rm = TRUE) / 1000)
m_listen <- tryCatch(
  lmer(listened_ms ~ label + position + (1|pid) + (1|clip), data = long,
       control = lmerControl(optimizer = "bobyqa")),
  error = function(e) NULL)
if (!is.null(m_listen))
  write_csv(broom.mixed::tidy(m_listen), file.path(OUT_DIR, "table_secondary_listening.csv"))

## ------------------ 8b. SENSITIVITY / POWER (Section 7.3) ----------------- ##
# Design-based sensitivity analysis reported in Section 7.3. This is a design
# calculation (not a re-estimate from the data): for the within-participant
# Recommended-vs-None contrast, the realized design gives ~80% power to detect a
# standardized effect of about d = 0.20, i.e. a minimum detectable effect (MDE)
# of d * SD(eval_composite) scale points. Reproduces "~80% power for d ~ 0.20,
# MDE ~ 0.31 scale points".
sd_eval_sens <- sd(long$eval_composite, na.rm = TRUE)
d_target <- 0.20
add("\n================ SENSITIVITY / POWER (Section 7.3) ================")
add("  Target standardized effect: d = %.2f (~80%% power, within-participant contrast)", d_target)
add("  Minimum detectable effect: %.2f scale points (= d x SD, SD = %.2f)",
    d_target * sd_eval_sens, sd_eval_sens)
# If the 'pwr' package is available, also report an analytic paired-design power
# check as a cross-reference (optional; safe to skip if not installed).
if (requireNamespace("pwr", quietly = TRUE)) {
  n_part <- dplyr::n_distinct(long$pid)
  pw <- pwr::pwr.t.test(n = n_part, d = d_target, sig.level = 0.05, type = "paired")
  add("  pwr cross-check (paired, n=%d, d=%.2f): power = %.2f", n_part, d_target, pw$power)
}

## ------------------------ 9. WRITE NUMBERS FILE ---------------------------- ##
writeLines(sink_lines, file.path(OUT_DIR, "numbers_to_paste.txt"))
write_csv(long, file.path(OUT_DIR, "analysis_long_format.csv"))
cat(paste(sink_lines, collapse = "\n"), "\n")
cat("\nAll outputs written to:", normalizePath(OUT_DIR), "\n")
