# Loading in results and generating plots

library(data.table)
library(tidyverse)
library(git2r)
library(tictoc)
library(ggplot2)
library(patchwork)
library(cowplot)
library(latex2exp)
library(furrr)
library(sn)
library(ggrepel)
library(testthat)
library(svglite)
library(lemon)


#########################
# Figures S1, 1, 2 and 3

rm(list = ls())
devtools::load_all()
no.samples <- 5000
cap_cases <- 2000
max_days <- 300

# Load in pre-saved results
sweep_results <- readRDS("data-raw/res_Jan_complete_fix.rds")

# A colour-blind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sweep_results <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = cap_cases, week_range = 40:42)) %>%
  dplyr::ungroup(scenario)

sweep_results <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(mean_Re = calc_R(sims[[1]])) %>%
  dplyr::ungroup(scenario)

sweep_results <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(sd_Re = calc_R_sd(sims[[1]])) %>%
  dplyr::ungroup(scenario)

sweep_results <- sweep_results %>%
  dplyr::group_by(index_R0, sensitivity, test_asym, test_delay, iso_adhere) %>%
  dplyr::mutate(rel_Re = (mean_Re[which(control_effectiveness==0)]-mean_Re)/mean_Re[which(control_effectiveness==0)]) %>%
  dplyr::ungroup()

# Figure S1: Parameter distributions (incubation, generation interval etc.)
ringbp::make_figure_S1()

# Figs 1 and 2
res <- sweep_results

lower <- rep(NA,nrow(res))
upper <- rep(NA,nrow(res))

for(i in 1:nrow(res)){
  out <- prop.test(res$pext[i]*no.samples,no.samples,correct=TRUE)
  CIs <- as.numeric(unlist(out[6]))
  lower[i] <- CIs[1]
  upper[i] <- CIs[2]
}

res <- res %>% mutate(lower = lower,
                      upper = upper)
lower <- c()
upper <- c()

saveRDS(res,'data-raw/Fig1.rds')
res <- readRDS('data-raw/Fig1.rds')

Fig1 <- res %>%
  filter(test_delay != 0.5) %>%
  filter(test_asym == FALSE) %>%
  filter(sensitivity == 0.95) %>%
  mutate(test_delay = factor(test_delay, labels = c('instant tracing','2 day delay'))) %>%
  mutate(iso_adhere = factor(iso_adhere, labels = c('poor reporting & adherence', 'good reporting & adherence','high adherence'))) %>%
  mutate(index_R0 = factor(index_R0, labels = c("Rs = 1.3","1.5"))) %>%
  ggplot(aes(control_effectiveness, 1 - pext, colour = test_delay, linetype = test_delay)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2)],name="TTI delay:") +
  ggplot2::scale_linetype_manual(values = c(2,1),name="TTI delay:") +
  geom_line(lwd=1.1) +
  geom_point() +
  geom_linerange(aes(control_effectiveness,ymax=1-lower,ymin=1-upper),show.legend=FALSE) +
  facet_rep_grid(iso_adhere~index_R0,scales='free', repeat.tick.labels = 'all') +
  coord_capped_cart(bottom='both', left='both') +
  theme_cowplot(font_size = 16) +
  background_grid() +
  theme(panel.spacing = unit(2, "lines")) +
  theme(strip.background =element_rect(fill="white"), axis.line=element_line()) +
  ggplot2::theme(legend.position = c(0.24,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,3,0.1), 'lines'),legend.key.size = grid::unit(2.5, "lines")) +
  #ggplot2::theme(legend.position = c(0.24,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,2.5,0.1), 'lines')) +
  labs(x='Contact tracing coverage',y="Prob. large outbreak") +
  ylim(c(0,0.6))

Fig2 <- res %>%
  filter(test_delay != 0.5) %>%
  filter(test_asym == FALSE) %>%
  filter(sensitivity == 0.95) %>%
  mutate(test_delay = factor(test_delay, labels = c('instant tracing','2 day delay'))) %>%
  mutate(iso_adhere = factor(iso_adhere, labels = c('poor reporting & adherence', 'good reporting & adherence','high adherence'))) %>%
  mutate(index_R0 = factor(index_R0, labels = c("Rs = 1.3","1.5"))) %>%
  ggplot(aes(control_effectiveness, 100*rel_Re, colour = test_delay, fill = test_delay)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2)],name="TTI delay:") +
  ggplot2::scale_fill_manual(values = cbPalette[c(4,2)],name="TTI delay:") +
  geom_col(aes(y=100*rel_Re),position=position_dodge()) +
  facet_rep_grid(iso_adhere ~ index_R0,scales='free', repeat.tick.labels = 'all') +
  coord_capped_cart(bottom='both', left='both') +
  theme_cowplot(font_size = 16) +
  background_grid() +
  theme(panel.spacing = unit(2, "lines")) +
  theme(strip.background =element_rect(fill="white"), axis.line=element_line()) +
  ggplot2::theme(legend.position = c(0.25,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,3,0.1), 'lines'),legend.key.size = grid::unit(1.5, "lines")) +
  #ggplot2::theme(legend.position = c(0.24,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,2.5,0.1), 'lines')) +
  labs(x='Contact tracing coverage',y="% reduction in R") +
  ylim(c(-2,15)) +
  scale_x_continuous(breaks=c(-0.2,0,0.2,0.4,0.6,0.8,1),labels=c("",0,0.2,0.4,0.6,0.8,""), limits = c(-0.2,1))

Fig3 <- res %>%
  filter(test_asym == FALSE) %>%
  filter(iso_adhere == 0.65) %>%
  mutate(test_delay = factor(test_delay, labels = c('instant tracing','1 day delay', '2 day delay'))) %>%
  mutate(index_R0 = factor(index_R0, labels = c("Rs = 1.3","1.5"))) %>%
  mutate(sensitivity = factor(sensitivity)) %>%
  mutate(iso_adhere = factor(iso_adhere,labels=c(""))) %>%
  ggplot(aes(control_effectiveness, 1 - pext, colour = test_delay, linetype = sensitivity)) +
  ggplot2::scale_colour_manual(values = cbPalette[c(4,2,7)],name="TTI delay:") +
  ggplot2::scale_linetype_manual(values = c(2,1),name="Test sensitivity:") +
  geom_line(lwd=1.1) +
  geom_point() +
  geom_linerange(aes(control_effectiveness,ymax=1-lower,ymin=1-upper),show.legend=FALSE) +
  facet_rep_grid(iso_adhere ~ index_R0,scales='free', repeat.tick.labels = 'all') +
  coord_capped_cart(bottom='both', left='both') +
  theme_cowplot(font_size = 16) +
  background_grid() +
  theme(panel.spacing = unit(2, "lines")) +
  theme(strip.background =element_rect(fill="white"), axis.line=element_line()) +
  ggplot2::theme(legend.position = c(0.05,-0.17),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,3,0.1), 'lines'),legend.key.size = grid::unit(2.5, "lines")) +
  #ggplot2::theme(legend.position = c(0.24,-0.1),legend.title=element_text(size=14),legend.direction = "horizontal",legend.box = "horizontal",plot.margin= grid::unit(c(0.1,0.1,2.5,0.1), 'lines')) +
  labs(x='Contact tracing coverage',y="Prob. large outbreak") +
  ylim(c(0,0.4))

################################
# Manipulate data for Fig 4

res2 <- list()
week_range <- 40:42

res <- sweep_results

for(i in seq_len(nrow(res))){
  #print(i)
  tmp <- res$sims[i][[1]]
  tmp <-
    tmp %>%
    dplyr::group_by(sim) %>% # group by simulation run
    mutate(max_weekly = max(weekly_cases),
           time_to_size = which(cumulative>=500)[1], #time to reach 500 cases (weeks)
           total = max(cumulative)) %>%
    dplyr::filter(week %in% week_range) %>%
    dplyr::summarise(extinct =
                       ifelse(all(weekly_cases == 0 &
                                    cumulative < cap_cases),
                              1, 0),
                     max_weekly = max(max_weekly),
                     time_to_size = min(time_to_size),
                     total = max(total)) %>%
    dplyr::ungroup()
  tmp <-
    tmp %>%
    mutate(index_R0 = res$index_R0[i],
           control_effectiveness = res$control_effectiveness[i],
           max_quar_delay = res$max_quar_delay[i],
           iso_adhere = res$iso_adhere[i],
           sensitivity = res$sensitivity[i])

  res2[[i]] <- tmp
}
res2 <- do.call(rbind, res2)

# we want:
# total outbreaks / n
total_cumulative_distr <-
  res2 %>%
  mutate(total = ifelse(total > 2000, 2000, total)) %>%
  group_by(index_R0, control_effectiveness, max_quar_delay, iso_adhere, sensitivity) %>%
  do(res = tibble(cumdistr = nrow(.) * ecdf(.$total)(4:2000),
                  total = 4:2000,
                  outbreaks = nrow(.) - sum(.$extinct),
                  runs = nrow(.),
                  max_quar_delay = .$max_quar_delay[1],
                  index_R0 = .$index_R0[1],
                  iso_adhere = .$iso_adhere[1],
                  sensitivity = .$sensitivity[1],
                  control_effectiveness = .$control_effectiveness[1],
                  poutbreak = pmin(1,(outbreaks) / (runs - cumdistr))))


total_cumulative_distr <- do.call(rbind, total_cumulative_distr$res) %>%
  mutate(index_R0 = factor(index_R0, labels = c('Rs = 1.3','1.5'))) %>%
  mutate(iso_adhere = factor(iso_adhere)) %>%
  mutate(sensitivity = factor(sensitivity, labels = c('65% sensitive','95%'))) %>%
  mutate(max_quar_delay = factor(max_quar_delay, labels = c('instant tracing', '1 day delay', '2 day delay'))) %>%
  filter(outbreaks != 0)

# Fig 4
T1 <- total_cumulative_distr

lower <- rep(NA,nrow(T1))
upper <- rep(NA,nrow(T1))

for(i in 1:nrow(T1)){

  out <- prop.test(T1$outbreaks[i],max(T1$runs[i]-T1$cumdistr[i],T1$outbreaks[i]),correct=FALSE)
  CIs <- as.numeric(unlist(out[6]))
  lower[i] <- CIs[1]
  upper[i] <- CIs[2]
}

T1 <- T1 %>% mutate(lower = lower,
                    upper = upper)
lower <- c()
upper <- c()

saveRDS(T1,'data-raw/Fig4.rds')
T1 <- readRDS('data-raw/Fig4.rds')

T1 <- T1 %>% mutate(control_effectiveness=factor(control_effectiveness,labels=c("no tracing","20%","40%","60%","80%")))
T1 <- T1 %>% mutate(iso_adhere = factor(iso_adhere, labels = c('poor reporting & adherence', 'average reporting & adherence','average reporting & high adherence')))


Fig4 <- T1 %>%
  filter(sensitivity=="95%") %>%
  filter(max_quar_delay != "1 day delay") %>%
  filter(index_R0=="Rs = 1.3") %>%
  ggplot(aes(total, poutbreak, colour = control_effectiveness)) +
  geom_line(size=1.1,aes(linetype=control_effectiveness)) +
  geom_linerange(alpha=0.1,aes(total,ymax=upper,ymin=lower),show.legend=FALSE) +
  facet_grid(max_quar_delay ~ iso_adhere) +
  scale_colour_manual(values = cbPalette[c(3,8,7,2,4)],name="TTI coverage:") +
  scale_linetype_manual(values = c(1,5,2,4,3),name="TTI coverage:") +
  ylab('Prob. large outbreak') +
  guides(linetype=guide_legend(title="TTI coverage:")) +
  theme_cowplot(font_size = 16) +
  theme(strip.background =element_rect(fill="white")) +
  background_grid() +
  ggplot2::theme(legend.position = c(0.08,0.2), legend.title=element_text(size=14), legend.key.width = unit(2,"cm")) +
  labs(x='Total cases so far',y="Prob. large outbreak") +
  xlim(c(0,1000)) +
  ylim(c(0,1))

save(file="data-raw/outbreakSize_plot.Rdata",Fig4)
