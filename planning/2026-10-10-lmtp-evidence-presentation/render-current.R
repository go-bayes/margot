set.seed(42)
library(margot)
n <- 3000; tau <- 6
waves <- paste("Time", 10:15, "to Time", 11:16)
w <- rexp(n); w <- w / mean(w)
p <- matrix(plogis(rnorm(n * tau, 2, 0.8)), n, tau)
obs <- matrix(rbinom(n * tau, 1, p), n, tau)
for (j in 2:tau) obs[, j] <- obs[, j] * obs[, j - 1]
reached <- cbind(TRUE, obs[, -tau] == 1)
p[!reached] <- NA
fac <- ifelse(reached, obs / p, 0)
cens <- margot_lmtp_censoring_report(obs, baseline_weights = w, fitted_probabilities = p,
  censoring_factors = fac, joint_ratios = fac * matrix(exp(rnorm(n * tau, 0, .4)), n, tau), wave_labels = waves)
src <- data.frame(age = sample(c("18-29","30-49","50+"), n, TRUE, c(.15,.35,.5)),
  gender = sample(c("Female","Male"), n, TRUE, c(.6,.4)), income = rnorm(n, 0.2))
tgt <- data.frame(age = sample(c("18-29","30-49","50+"), 5000, TRUE, c(.22,.34,.44)),
  gender = sample(c("Female","Male"), 5000, TRUE), income = rnorm(5000))
proj <- margot_target_projection_report(src, tgt, c("age","gender","income"), projection_weights = w, strata = c("age","gender"))
jr <- fac * matrix(exp(rnorm(n * tau, 0, .4)), n, tau)
aw <- lapply(c(intervention = "intervention", natural_course = "natural_course"), function(a)
  margot_lmtp_analysis_weight_report(w, jr, regularised_joint_ratios = pmin(jr, quantile(jr, .99)), wave_labels = waves, policy_id = a))
ev <- margot_lmtp_evidence_report(cens, proj, aw)
sink(file.path(Sys.getenv("OUT"), "print.txt")); print(ev); print(cens); print(proj); print(aw[[1]]); sink()
ggplot2::ggsave(file.path(Sys.getenv("OUT"), "censoring.png"), plot(cens), width = 12, height = 4, dpi = 110, bg = "white")
ggplot2::ggsave(file.path(Sys.getenv("OUT"), "projection.png"), plot(proj), width = 11, height = 5, dpi = 110, bg = "white")
ggplot2::ggsave(file.path(Sys.getenv("OUT"), "weights.png"), plot(aw[[1]]), width = 10, height = 4, dpi = 110, bg = "white")
ggplot2::ggsave(file.path(Sys.getenv("OUT"), "evidence.png"), plot(ev), width = 12, height = 16, dpi = 90, bg = "white")
