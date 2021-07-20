require(tidyverse)
require(gtools)
require(ggthemes)
require(gridExtra)
require(brms)

color_scheme_set("gray")

load('bivariate_fit_main.rda') # after running the model

bivariate_fit <- bivariate_fit_main


# Interaction Plot --------------------------------------------------------

post_1 <- as.data.frame(bivariate_fit)



# Angry Reactions ---------------------------------------------------------

# Conditional Effect of Issue Importance

theta <- post_1[, "b_numangryint_MIPP"] + post_1[, "b_numangryint_MIPP:sentiment"] %*% t(seq(from = -.5, to = .5, by = 0.05))
colnames.theta <- as.vector(unname(t(seq(from = -.5, to = .5, by = 0.05))))
colnames(theta) <- format(colnames.theta, digits = 2, nsmall = 2)
plot_dat <- data.frame(M = format(colnames.theta, digits = 2, nsmall = 2),
                       theta = apply(theta, 2, mean),
                       lower = apply(theta, 2, quantile, 0.025),
                       upper = apply(theta, 2, quantile, 0.975))
plot_dat$M <- factor(plot_dat$M, levels = gtools::mixedsort(as.vector(plot_dat$M)))

p_sim_sent <- ggplot(plot_dat, aes(x = M, y = theta, group = 1)) +
  geom_line(color = 'gray14') +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'gray53', alpha = .2) +
  geom_hline(yintercept = 0, color = 'black', linetype = 2) +
  theme_tufte() +
  xlab("Moderator (Sentiment)") +
  ylab(expression(Conditional~effect~of~Issue~Salience~theta)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 22)) +
  theme(axis.text.y = element_text(size = 22)) +
  theme(axis.title = element_text(size = 22)) +
  theme(axis.title.y = element_text(size = 22)) +
  theme(text = element_text(family = 'Arial'))




# Conditional Effect of Sentiment

theta <- post_1[, "b_numangryint_sentiment"] + post_1[, "b_numangryint_MIPP:sentiment"] %*% t(seq(from = 0, to = .7, by = 0.05))
colnames.theta <- as.vector(unname(t(seq(from = 0, to = .7, by = 0.05))))
colnames(theta) <- format(colnames.theta, digits = 2, nsmall = 2)
plot_dat <- data.frame(M = format(colnames.theta, digits = 2, nsmall = 2),
                       theta = apply(theta, 2, mean),
                       lower = apply(theta, 2, quantile, 0.025),
                       upper = apply(theta, 2, quantile, 0.975))
plot_dat$M <- factor(plot_dat$M, levels = gtools::mixedsort(as.vector(plot_dat$M)))

p_sim_MIPP <- ggplot(plot_dat, aes(x = M, y = theta, group = 1)) +
  geom_line(color = 'gray14') +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill = 'gray53', alpha = .2) +
  geom_hline(yintercept = 0, color = 'black', linetype = 2) +
  theme_tufte() +
  xlab("Moderator (Issue Salience)") +
  ylab(expression(Conditional~effect~of~Sentiment~theta)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 22)) +
  theme(axis.text.y = element_text(size = 22)) +
  theme(axis.title = element_text(size = 22)) +
  theme(axis.title.y = element_text(size = 22)) +
  scale_y_continuous(position = "right") +
  theme(text = element_text(family = 'Arial'))
  

# Plot --------------------------------------------------------------------

grid.arrange(p_sim_sent, p_sim_MIPP,
             ncol = 2)
