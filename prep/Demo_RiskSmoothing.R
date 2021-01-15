## Demo: spatial-temporal smoothing of raisk estimates
require(TBDtoolbox)
require(tidyverse)
require(rstan)

## Binomial family
# make sure its a complete data frame.
df <- risk_drink %>%
  complete(age, sex, year, city)%>%
  mutate(
    Xc = as.integer(factor(city)),
    Xr = as.integer(factor(region)),
    isDat = as.integer(nsam > 0)
  )

X_fix <- model.matrix(~ age + sex , data = df)
X_hier <- as.matrix(select(df, Xc, Xr))

#
fit <- HIfit(X_fix = X_fix , X_hier = X_hier,
             Y = df$csam, Nsam = df$nsam, isDat = df$isDat,
             model = "binomial",
             chains = 4, core= 4,
             iter = 2000, warmup = 1000,
             control=list(adapt_delta = .8, max_treedepth = 10))

plot(fit)

check_hmc_diagnostics(fit$fit)


df <- bind_cols(df, fit$est[[1]])

df%>%
  mutate(
    age = factor(age),
    prev = ifelse(isDat == 1, prev, NA))%>%
  ggplot()+
  geom_point(aes(x=age, y=prev, color = sex, size = nsam), shape = 4)+
  geom_linerange(aes(x=age,
                      ymin =est_l, ymax =  est_u,
                      color = sex, group = interaction(sex, city)),
                 size = 2, alpha = .3)+
  geom_path(aes(x=age, y=est, color = sex,
                group = interaction(sex, city)), alpha = .5)+
  scale_color_manual(values = c("red4", "blue4"))+
  facet_wrap(.~city)+
  theme_bw()



## Normal family
df <- risk_drink %>%
  complete(age, sex, year, city)%>%
  mutate(
    Xc = as.integer(factor(city)),
    Xr = as.integer(factor(region)),
    isDat = as.integer(nsam > 0)
  )

X_fix <- model.matrix(~ age + sex , data = df)
X_hier <- as.matrix(select(df, Xc, Xr))

fit2 <- HIfit(X_fix = X_fix, X_hier = X_hier,
             Y = df$mean_x, isDat = df$isDat,
             fam = "normal", pop = df$nsam, sdv = df$sd_x,
             chains = 4, core= 4,
             iter = 2500, warmup = 1500,
             control=list(adapt_delta = .8, max_treedepth = 10))

df <- bind_cols(df, fit2$est[[1]])

df%>%
  mutate(
    age = factor(age),
    mean_x = ifelse(isDat == 1, mean_x, NA))%>%
  ggplot()+
  geom_point(aes(x=age, y=mean_x, color = sex, size = nsam), shape = 4)+
  geom_linerange(aes(x=age,
                     ymin =est_l, ymax =  est_u,
                     color = sex, group = interaction(sex, city)),
                 size = 2, alpha = .3)+
  geom_path(aes(x=age, y=est, color = sex,
                group = interaction(sex, city)), alpha = .5)+
  scale_color_manual(values = c("red4", "blue4"))+
  facet_wrap(.~city)+
  theme_bw()



## LDL
df <- risk_3H %>%
  filter(risk == "LDL")%>%
  mutate(
    age = factor(age),
    year = as.integer(factor(year)),
    Xc = as.integer(factor(city)),
    Xr = as.integer(factor(region)),
    isDat = as.integer(sd_x > 0)
  )

a <- mean(df$mean_x)
df <- df %>%
  mutate(mean_x = mean_x - a)


X_fix <- model.matrix(~ age + sex + year , data = df)
X_hier <- as.matrix(select(df, Xc, Xr))


fit3 <- HIfit(X_fix = X_fix, X_hier = X_hier,
              Y = df$mean_x, isDat = df$isDat,
              fam = "normal", pop = df$nsam, sdv = df$sd_x,
              chains = 4, core= 4,
              iter = 2500, warmup = 1500,
              control=list(adapt_delta = .8, max_treedepth = 10))

check_hmc_diagnostics(fit$fit)

bind_cols(df, fit3$est[[1]])%>%
  filter(year == 2)%>%
  mutate(
    age = factor(age),
    mean_x = mean_x + a,
    est = est + a,
    est_l = est_l + a,
    est_u = est_u + a,
    mean_x = ifelse(isDat == 1, mean_x, NA))%>%
  ggplot()+
  geom_point(aes(x=age, y=mean_x, color = sex), shape = 4)+
  geom_linerange(aes(x=age,
                     ymin =est_l, ymax =  est_u,
                     color = sex, group = interaction(sex, city)),
                 size = 2, alpha = .3)+
  geom_path(aes(x=age, y=est, color = sex,
                group = interaction(sex, city)), alpha = .5)+
  scale_color_manual(values = c("red4", "blue4"))+
  facet_wrap(.~city)+
  theme_bw()


## LDL: age sex interaction
df <- risk_3H %>%
  filter(risk == "LDL")%>%
  mutate(
    age = factor(age),
    year = as.integer(factor(year)),
    Xc = as.integer(factor(city)),
    Xr = as.integer(factor(region)),
    isDat = as.integer(sd_x > 0)
  )

a <- mean(df$mean_x)
df <- df %>%
  mutate(mean_x = mean_x - a)


X_fix <- model.matrix(~ age*sex + year , data = df)
X_hier <- as.matrix(select(df, Xc, Xr))


fit4 <- HIfit(X_fix = X_fix, X_hier = X_hier,
              Y = df$mean_x, isDat = df$isDat,
              fam = "normal", pop = df$nsam, sdv = df$sd_x,
              chains = 4, core= 4,
              iter = 2500, warmup = 1500,
              control=list(adapt_delta = .8, max_treedepth = 10))

bind_cols(df, fit4$est[[1]])%>%
  filter(year == 2)%>%
  mutate(
    age = factor(age),
    mean_x = mean_x + a,
    est = est + a,
    est_l = est_l + a,
    est_u = est_u + a,
    mean_x = ifelse(isDat == 1, mean_x, NA))%>%
  ggplot()+
  geom_point(aes(x=age, y=mean_x, color = sex), shape = 4)+
  geom_linerange(aes(x=age,
                     ymin =est_l, ymax =  est_u,
                     color = sex, group = interaction(sex, city)),
                 size = 2, alpha = .3)+
  geom_path(aes(x=age, y=est, color = sex,
                group = interaction(sex, city)), alpha = .5)+
  scale_color_manual(values = c("red4", "blue4"))+
  facet_wrap(.~city)+
  theme_bw()






##
