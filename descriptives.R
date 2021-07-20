require(tidyverse)
require(brms)
require(kableExtra)
require(ggthemes)
require(reshape2)
require(extrafont)
require(rstan)
require(brms)
require(ggthemes)
require(bayesplot)


data <- read_csv("data.csv")

# re-level issues for plots

data <- data %>%
  mutate(
    issue = factor(issue,
                       levels = c("10000",
                                  "13000",
                                  "20000",
                                  "17000",
                                  "16000",
                                  "22000",
                                  "18000",
                                  "12000",
                                  "14000",
                                  "19000",
                                  "11000"))
  )

# Descriptives by Party ---------------------------------------------------

party_table <- data %>%
  group_by(party_affil) %>%
  summarise(
    all_angry  = sum(numangry_int),
    all_loves  = sum(numloves_int),
    all_wows   = sum(numwow_int),
    all_haha   = sum(numhaha_int),
    all_sad    = sum(numsad_int),
    all_reactions = sum(numsad_int , numwow_int , numhaha_int , numangry_int , numloves_int)
  ) %>%
  mutate(
    rel_angry = all_angry / all_reactions,
    rel_loves = all_loves / all_reactions,
    rel_wows  = all_wows / all_reactions,
    rel_haha  = all_haha / all_reactions,
    rel_sad   = all_sad / all_reactions
  ) %>%
  mutate(
    party_affil = factor(party_affil, levels = c("GRÜNE", "PILZ", "SPÖ", "NEOS", "ÖVP", "FPÖ"))
  )


# Descriptives by Issue ---------------------------------------------------

issue_table <- data %>%
  group_by(issue) %>%
  summarise(
    all_angry  = sum(numangry_int),
    all_loves  = sum(numloves_int),
    all_wows   = sum(numwow_int),
    all_haha   = sum(numhaha_int),
    all_sad    = sum(numsad_int),
    all_reactions = sum(numsad_int , numwow_int , numhaha_int , numangry_int , numloves_int)
  ) %>%
  mutate(
    rel_angry = all_angry / all_reactions,
    rel_loves = all_loves / all_reactions,
    rel_wows  = all_wows / all_reactions,
    rel_haha  = all_haha / all_reactions,
    rel_sad   = all_sad / all_reactions
  ) %>% 
  mutate(
    issue_new = factor(issue,
                       levels = c("10000",
                                  "13000",
                                  "20000",
                                  "17000",
                                  "16000",
                                  "22000",
                                  "18000",
                                  "12000",
                                  "14000",
                                  "19000",
                                  "11000"))
  )


# Sentiment by Issue ------------------------------------------------------

sentiment_table <- data %>%
  group_by(issue) %>%
  summarise(
    mean_sent = mean(sentiment),
    sd = sd(sentiment)
  ) %>% 
  mutate(
    issue_new = factor(issue,
                       levels = c("10000",
                                  "13000",
                                  "20000",
                                  "17000",
                                  "16000",
                                  "22000",
                                  "18000",
                                  "12000",
                                  "14000",
                                  "19000",
                                  "11000"))
  )





# Standard Errors for sentiment -------------------------------------------

sent_sd <- data %>%
  group_by(issue) %>%
  summarise(
    mean_sent = mean(sentiment),
    sd = sd(sentiment),
    n = n()
  ) %>% 
  mutate(
    issue_new = factor(issue,
                       levels = c("10000",
                                  "13000",
                                  "20000",
                                  "17000",
                                  "16000",
                                  "22000",
                                  "18000",
                                  "12000",
                                  "14000",
                                  "19000",
                                  "11000"))
  )



# new_data_sent <- new_sub %>%
#   group_by(issue_new)
# 


# Sentiment by Party ------------------------------------------------------


sentiment_party <- data %>%
  group_by(party_affil) %>%
  summarise(
    mean_sent = mean(sentiment),
    sd = sd(sentiment)
  ) %>%
  mutate(
    party_affil = factor(party_affil, levels = c("GRÜNE", "PILZ", "SPÖ", "NEOS", "ÖVP", "FPÖ"))
  )



# Breaks for plots --------------------------------------------------------


breaks = c("10000",
           "13000",
           "20000",
           "17000",
           "16000",
           "22000",
           "18000",
           "12000",
           "14000",
           "19000",
           "11000")

labels = c(
  "Economy & Budget",
  "Education & Culture",
  "Environment",
  "Europe",
  "Foreign Policy",
  "Immigration",
  "Infrastructure",
  "Labour Market",
  "Security & Army",
  "Society",
  "Welfare State"
  
)


party_breaks = c("PILZ",
                 "GRÜNE",
                 "SPÖ",
                 "NEOS",
                 "ÖVP",
                 "FPÖ")

party_labels = c("PILZ",
                 "GREENS",
                 "SPÖ",
                 "NEOS",
                 "ÖVP",
                 "FPÖ")




# Plots -------------------------------------------------------------------


# Figure 1 ----------------------------------------------------------------

party_table %>%
  select(-starts_with('all_')) %>%
  melt() %>%
  mutate(
    variable = factor(variable, levels = rev(c("rel_angry", "rel_loves", "rel_wows", "rel_haha", "rel_sad")))
  ) %>%
  ggplot() +
  geom_bar(aes(party_affil, value, fill = variable), stat = 'identity') +
  scale_fill_discrete(
    breaks = party_breaks,
    labels = party_labels
  ) + 
  scale_x_discrete(
    breaks = party_breaks,
    labels = party_labels
  ) +
  theme_tufte() +
  theme(axis.text.x = element_text(size = 27),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 27),
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 23)) +
  xlab('Party') +
  ylab('Share of Reactions') +
  scale_fill_manual(name = "Reaction",
                  breaks = c("rel_angry", "rel_loves", "rel_wows", "rel_haha", "rel_sad"),
                  labels = c("Angry", "Love", "Wow",  "Haha", "Sad"),
                  values = rev(c("gray18", "gray32", "gray45", "gray55", "gray75"))) +
  scale_y_reverse() +
  theme(text = element_text(family = 'Arial'))






# Figure 2 ----------------------------------------------------------------


issue_table %>%
  select(-starts_with('all_')) %>%
  melt() %>%
  mutate(
    variable = factor(variable, levels = rev(c("rel_angry", "rel_loves", "rel_wows", "rel_haha", "rel_sad")))
  ) %>%
  ggplot() +
  geom_bar(aes(issue_new, value, fill = variable), stat = 'identity') +
  scale_fill_discrete(
    breaks = breaks,
    labels = labels
  ) + 
  scale_x_discrete(
    breaks = breaks,
    labels = labels
  ) + 
  theme_tufte() +
  theme(axis.text.x = element_text(size = 25, angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 27),
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 23)) +
  xlab('Policy Field') +
  ylab('Share of Reactions') +
  scale_fill_manual(name = "Reaction",
                  breaks = c("rel_angry", "rel_loves", "rel_wows", "rel_haha", "rel_sad"),
                  labels = c("Angry", "Love", "Wow",  "Haha", "Sad"),
                  values = rev(c("gray18", "gray32", "gray45", "gray55", "gray75"))) +
  theme(text = element_text(family = 'Arial'))




# Figure 3 ----------------------------------------------------------------


sentiment_party %>%
  ggplot() +
  geom_errorbar(aes(x = factor(party_affil),
                    ymin = mean_sent - sd,
                    ymax = mean_sent + sd), width = 0.3, size = 1, color = "lightgrey") +
  geom_point(aes(factor(party_affil), y = mean_sent),
             size = 4,
             shape = 21,
             fill = 'darkgrey') +
  scale_fill_discrete(
    breaks = party_breaks,
    labels = party_labels
  ) + 
  scale_x_discrete(
    breaks = party_breaks,
    labels = party_labels
  ) +
  theme_tufte() +
  theme(axis.text.x = element_text(size = 27),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 27),
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 23)) +
  xlab('Party') +
  ylab('Sentiment Score') +
  geom_hline(yintercept = 0, lty = 3) +
  theme(text = element_text(family = 'Arial'))


# Figure 4 ----------------------------------------------------------------


sent_sd %>%
  ggplot() +
  geom_errorbar(aes(x = factor(issue_new),
                    ymin = mean_sent - sd,
                    ymax = mean_sent + sd), width = 0.3, size = 1, color = "lightgrey") +
  geom_point(aes(factor(issue_new), y = mean_sent),
             size = 4,
             shape = 21,
             fill = 'darkgrey') + 
  scale_x_discrete(
    breaks = breaks,
    labels = labels
  ) +
  theme_tufte() +
  theme(axis.text.x = element_text(size = 25, angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 27),
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 23)) +
  xlab('Policy Field') +
  ylab('Sentiment Score') +
  geom_hline(yintercept = 0, lty = 3)  +
  theme(text = element_text(family = 'Arial'))




# Descriptive Tables ------------------------------------------------------

# Issue posts by party

data %>%
  group_by(issue) %>%
  summarise(
    n()
  ) %>%
  mutate(
    issue = labels
  ) %>%
  stargazer(type = 'text',
            summary = FALSE)

# Posts by Party

data %>%
  group_by(party_affil) %>%
  summarise(
    n()
  ) %>%
  stargazer(type = 'text',
            summary = FALSE)


# Reactions by Party / 
data %>%
  group_by(party_affil) %>%
  summarise(
    all_angry  = sum(numangry_int),
    all_loves  = sum(numloves_int),
    all_wows   = sum(numwow_int),
    all_haha   = sum(numhaha_int),
    all_sad    = sum(numsad_int),
    all_reactions = sum(numsad_int , numwow_int , numhaha_int , numangry_int , numloves_int),
    all_likes  = sum(numlikes_int),
    all_shares = sum(numshares_int)
  ) %>%
  stargazer(type = 'text',
            summary = FALSE)




# Reactions by Issue

table_labels = c(
  "Economy and Budget",
  "Education and Culture",
  "Environment",
  "Europe",
  "Foreign Policy",
  "Immigration",
  "Infrastructure",
  "Labour Market",
  "Security and Army",
  "Society",
  "Welfare State"
  
)

data %>%
  group_by(issue) %>%
  summarise(
    all_angry  = sum(numangry_int),
    all_loves  = sum(numloves_int),
    all_wows   = sum(numwow_int),
    all_haha   = sum(numhaha_int),
    all_sad    = sum(numsad_int),
    all_reactions = sum(numsad_int , numwow_int , numhaha_int , numangry_int , numloves_int),
    all_likes  = sum(numlikes_int),
    all_shares = sum(numshares_int)
  ) %>%
  mutate(
    issue = table_labels
  ) %>%
  stargazer(type = 'text',
            summary = FALSE)



# Table 1 -----------------------------------------------------------------


table1 <- t(table(data$party_affil, data$issue))
rownames(table1) <- table_labels

total_party <- colSums(table1)

table1 <- round(t(t(table1)/total_party) * 100, 1)



# Table 2 -----------------------------------------------------------------

table2 <- data %>%
  group_by(issue, party_affil) %>%
  summarise(mean_MIPP = mean(MIPP) * 100) %>%
  mutate(mean_MIPP = round(mean_MIPP, 1)) %>%
  ungroup() %>%
  pivot_wider(values_from = mean_MIPP,
              names_from = party_affil) %>%
  mutate(issue = table_labels)

table2
