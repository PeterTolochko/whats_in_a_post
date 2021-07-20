require(gridExtra)
require(rstan)
require(brms)
require(ggthemes)
require(bayesplot)

color_scheme_set(scheme = "gray")

load('bivariate_fit_main.rda')
post_1 <- posterior_samples(bivariate_fit_main)
bivariate_fit <- bivariate_fit_main


labels = c(
  "Intercept",
  "Issue Salience",
  "Sentiment",
  "Length (log)",
  "Event (vs. Text Only)",
  "Link (vs. Text Only)",
  "Note (vs. Text Only)",
  "Photo (vs. Text Only)",
  "Video (vs. Text Only)",
  "Leader (vs. Candidate)",
  "Organization (vs. Candidate)",
  "Government",
  "Ideology Left/Right",
  "Ideology Extremism",
  "Sentiment x Issue Salience"
)

breaks_angry = c(
  "b_numangryint_Intercept",
  "b_numangryint_MIPP",
  "b_numangryint_sentiment",
  "b_numangryint_log_length",
  "b_numangryint_statustypeevent",
  "b_numangryint_statustypelink",
  "b_numangryint_statustypenote",
  "b_numangryint_statustypephoto",
  "b_numangryint_statustypevideo",
  "b_numangryint_else_leader_orgleader",
  "b_numangryint_else_leader_orgorg",
  "b_numangryint_government",
  "b_numangryint_IDEO",
  "b_numangryint_IDEOsq",
  "b_numangryint_MIPP:sentiment"
)

breaks_loves = c(
  "b_numlovesint_Intercept",
  "b_numlovesint_MIPP",
  "b_numlovesint_sentiment",
  "b_numlovesint_log_length",
  "b_numlovesint_statustypeevent",
  "b_numlovesint_statustypelink",
  "b_numlovesint_statustypenote",
  "b_numlovesint_statustypephoto",
  "b_numlovesint_statustypevideo",
  "b_numlovesint_else_leader_orgleader",
  "b_numlovesint_else_leader_orgorg",
  "b_numlovesint_government",
  "b_numlovesint_IDEO",
  "b_numlovesint_IDEOsq",
  "b_numlovesint_MIPP2:sentiment"
)

# Angry model plot --------------------------------------------------------

pars_angry <- names(post_1)[1:16]
pars_angry <- pars_angry[-2]

pars_angry <- c(
  'b_numangryint_statustypeevent',
  'b_numangryint_statustypelink',
  'b_numangryint_statustypenote',
  'b_numangryint_statustypephoto',
  'b_numangryint_statustypevideo',
  'b_numangryint_log_length',
  'b_numangryint_IDEOsq',
  'b_numangryint_IDEO',
  'b_numangryint_government',
  'b_numangryint_else_leader_orgorg',
  'b_numangryint_else_leader_orgleader',
  'b_numangryint_MIPP:sentiment',
  'b_numangryint_MIPP',
  'b_numangryint_sentiment',
  'b_numangryint_Intercept'
  
)

p1 <- stanplot(bivariate_fit, type = "intervals", prob_outer = 0.95,
         pars = pars_angry)
p1 <- p1 + scale_y_discrete(labels = labels, breaks = breaks_angry) +
  theme_tufte() +
  theme(axis.text.y = element_text(hjust = 1, size = 22),
        plot.title = element_text(hjust = 0.5, size = 22),
        axis.text.x = element_text(size = 22),
        axis.ticks = element_blank()) +
  theme(text = element_text(family = 'Arial')) +
  xlim(-10, 10)

# Love model plot ---------------------------------------------------------

pars_loves <- names(post_1)[c(2,17:30)]

pars_loves <- c(
  'b_numlovesint_statustypeevent',
  'b_numlovesint_statustypelink',
  'b_numlovesint_statustypenote',
  'b_numlovesint_statustypephoto',
  'b_numlovesint_statustypevideo',
  'b_numlovesint_log_length',
  'b_numlovesint_IDEOsq',
  'b_numlovesint_IDEO',
  'b_numlovesint_government',
  'b_numlovesint_else_leader_orgorg',
  'b_numlovesint_else_leader_orgleader',
  'b_numlovesint_MIPP:sentiment',
  'b_numlovesint_MIPP',
  'b_numlovesint_sentiment',
  'b_numlovesint_Intercept'
  
)

p2 <- stanplot(bivariate_fit, type = "intervals", prob_outer = 0.95,
               pars = pars_loves)
p2 <- p2 +
  scale_y_discrete(labels = labels, breaks = breaks_loves) +
  theme_tufte() +
  theme(axis.text.y = element_text(hjust = 1, size = 22),
        plot.title = element_text(hjust = 0.5, size = 22),
        axis.text.x = element_text(size = 22),
        axis.ticks = element_blank()) +
  theme(text = element_text(family = 'Arial')) +
  xlim(-10, 10)

grid.arrange(p2, p1,
             ncol = 2)

# Angry Traceplots --------------------------------------------------------

post_angry <- posterior_samples(bivariate_fit,
                              pars = breaks_angry,
                              as.array = T)

dimnames(post_angry)[[3]] <- labels

trace_angry <- mcmc_trace(post_angry, pars = labels, 
           facet_args = list(ncol = 4, strip.position = "left")) +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

# Love Traceplots ---------------------------------------------------------

post_love <- posterior_samples(bivariate_fit,
                                pars = breaks_loves,
                                as.array = T)

dimnames(post_love)[[3]] <- labels

trace_love <- mcmc_trace(post_love, pars = labels, 
                          facet_args = list(ncol = 4, strip.position = "left")) +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

dens_angry <- mcmc_dens_overlay(post_angry, pars = labels) +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5, size = 15))
dens_love <- mcmc_dens_overlay(post_love, pars = labels) +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

grid.arrange(trace_angry, trace_love,
             dens_angry, dens_love,
             ncol = 2)
