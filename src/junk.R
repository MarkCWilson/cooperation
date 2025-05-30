# linear regression - does it make sense - no?

lm_model <- linear_reg() %>% set_engine("lm")

lm_form_fit <- lm_model %>% fit(C ~ gender + age + crt + IC  + trust + disc + nfc, data = df2 %>% drop_na())
out0 <- lm_form_fit %>% extract_fit_engine() %>% summary()

lm_form_fit <- lm_model %>% fit(C ~ gender + age, data = df2)
out1 <- lm_form_fit %>% extract_fit_engine() %>% summary()
lm_form_fit <- lm_model %>% fit(R ~ gender + age, data = df2)
out2 <- lm_form_fit %>% extract_fit_engine() %>% summary()
lm_form_fit <- lm_model %>% fit(crt ~ gender + age, data = df2)
out3 <- lm_form_fit %>% extract_fit_engine() %>% summary()
lm_form_fit <- lm_model %>% fit(R ~ gender + crt, data = df2)
out4 <- lm_form_fit %>% extract_fit_engine() %>% summary()
lm_form_fit <- lm_model %>% fit(p10 ~ gender + crt, data = df2)
out5 <- lm_form_fit %>% extract_fit_engine() %>% summary()


lm_model2 <-linear_reg(penalty = double(1), mixture = double(1)) %>%
  set_engine("glmnet")
lm_form_fit2 <- lm_model %>% fit(R ~ gender + age + crt + IC  + trust + disc + nfc, data = df2 %>% drop_na())
out6 <- lm_form_fit2 %>% extract_fit_engine() %>% summary()
