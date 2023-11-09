library(tidyverse)
library(lme4)

data<- read.csv("deaths_by_firearms-hw9.csv") %>%
  mutate(state = fct_relevel(state, 'Virginia'))



model_4i <- glmer.nb(number_of_deaths ~ (1 | state) + gun_control_laws * years_past_2010 * relationship * ownership,
      data = data)

model_3i <- glmer.nb(number_of_deaths ~ (1|state) + gun_control_laws * years_past_2010 * relationship +
                       gun_control_laws * relationship * ownership +
                       gun_control_laws * years_past_2010 * ownership +
                       years_past_2010 * relationship * ownership,
                     data = data)

model_2i <- glmer.nb(number_of_deaths ~ (1|state) + gun_control_laws * years_past_2010 +
                       gun_control_laws * relationship + 
                       gun_control_laws * ownership +
                       years_past_2010 * relationship +
                       years_past_2010 * ownership +
                       relationship * ownership,
                  data = data)

model_GY <- glmer.nb(number_of_deaths ~ (1|state) + gun_control_laws + 
                       years_past_2010 + 
                       relationship +
                       ownership + 
                       gun_control_laws * years_past_2010,
                     data = data)

model_GR <- glmer.nb(number_of_deaths ~ (1|state) + gun_control_laws + 
                       years_past_2010 + 
                       relationship +
                       ownership +
                       gun_control_laws * relationship,
                     data = data)

model_GO <- glmer.nb(number_of_deaths ~ (1|state) + gun_control_laws + 
                       years_past_2010 + 
                       relationship +
                       ownership + 
                       gun_control_laws * ownership,
                     data = data)

model_YR <- glmer.nb(number_of_deaths ~ (1|state) + gun_control_laws + 
                       years_past_2010 + 
                       relationship +
                       ownership + 
                       years_past_2010 * relationship,
                     data = data)

model_YO <- glmer.nb(number_of_deaths ~ (1|state) + gun_control_laws + 
                       years_past_2010 + 
                       relationship +
                       ownership + 
                       years_past_2010 * ownership,
                     data = data)

model_RO <- glmer.nb(number_of_deaths ~ (1|state) + gun_control_laws + 
                       years_past_2010 + 
                       relationship +
                       ownership + 
                       relationship * ownership,
                     data = data)

comparisons <- cbind(model = c("model_2i","model_GY", "model_GR", "model_GO", "model_YR", "model_YO", "model_RO"),
      npar = c(anova(model_GY, model_2i, test = 'LRT')$npar[2], anova(model_GY, model_2i, test = 'LRT')$npar[1],anova(model_GR, model_2i, test = 'LRT')$npar[1], anova(model_GO, model_2i, test = 'LRT')$npar[1], anova(model_YR, model_2i, test = 'LRT')$npar[1], anova(model_YO, model_2i, test = 'LRT')$npar[1], anova(model_RO, model_2i, test = 'LRT')$npar[1]),
      AIC = c(anova(model_GY, model_2i, test = 'LRT')$AIC[2], anova(model_GY, model_2i, test = 'LRT')$AIC[1],anova(model_GR, model_2i, test = 'LRT')$AIC[1], anova(model_GO, model_2i, test = 'LRT')$AIC[1], anova(model_YR, model_2i, test = 'LRT')$AIC[1], anova(model_YO, model_2i, test = 'LRT')$AIC[1], anova(model_RO, model_2i, test = 'LRT')$AIC[1]),
      Pval = c("NA", anova(model_GY, model_2i, test = 'LRT')$`Pr(>Chisq)`[2],anova(model_GR, model_2i, test = 'LRT')$`Pr(>Chisq)`[2], anova(model_GO, model_2i, test = 'LRT')$`Pr(>Chisq)`[2], anova(model_YR, model_2i, test = 'LRT')$`Pr(>Chisq)`[2], anova(model_YO, model_2i, test = 'LRT')$`Pr(>Chisq)`[2], anova(model_RO, model_2i, test = 'LRT')$`Pr(>Chisq)`[2]))

model_RO_GR <- glmer.nb(number_of_deaths ~ (1|state) + gun_control_laws + 
                         years_past_2010 + 
                         relationship +
                         ownership + 
                         relationship * ownership +
                         gun_control_laws * relationship,
                       data = data)
rbind(comparisons, c("model_RO_GR", anova(model_RO_GR, model_2i, test = 'LRT')$npar[1], anova(model_RO_GR, model_2i, test = 'LRT')$AIC[1], anova(model_RO_GR, model_2i, test = 'LRT')$`Pr(>Chisq)`[2]))

basic <- glmer.nb(number_of_deaths ~ (1|state) + gun_control_laws + 
                    years_past_2010 + 
                    relationship +
                    ownership,
                  data = data)

anova(basic, model_RO, test = 'LRT')

##final model is model_RO, plot built below

tibble(fitted = fitted(model_RO),
       resid = residuals(model_RO, type = 'response'),
       var = fitted + (fitted^2 * model_RO@theta)) %>% 
  ggplot(aes(x = fitted, y = resid^2)) +
  geom_point(color = 'gray50') +
  geom_line(aes(y = var), size = 1.2) +
  scale_y_log10() +
  scale_x_log10()

#same model but fitted using poisson
model_poisson <- glmer(number_of_deaths ~ (1|state) + gun_control_laws + 
                       years_past_2010 + 
                       relationship +
                       ownership + 
                       relationship * ownership,
                     data = data,
                     family = poisson('log'))

tibble(fitted = fitted(model_poisson),
       resid = residuals(model_poisson, type = 'response'),
       var = fitted) %>% 
  ggplot(aes(x = fitted, y = resid^2)) +
  geom_point(color = 'gray50') +
  geom_line(aes(y = var), size = 1.2) +
  scale_y_log10() +
  scale_x_log10()
