


## -----------------------------------------------------------------------------
library(mosaic)
library(tidyverse)
library(lubridate)
library(corrr)
library(caret)
library(doMC)
library(ranger)
library(sjmisc)


## -----------------------------------------------------------------------------
library(nycflights13)
data(flights)
glimpse(flights)


## -----------------------------------------------------------------------------
favstats(arr_delay ~ 1, data = flights)
favstats(dep_delay ~ 1, data = flights)


## -----------------------------------------------------------------------------
flights %>% 
  select(arr_delay, dep_delay) %>% 
  map_df(favstats)


## -----------------------------------------------------------------------------
flights %>% 
  select(arr_delay, dep_delay) %>% 
  drop_na() %>% 
  map_df(iqr)


## ----fig.show = "hold"--------------------------------------------------------
gf_histogram( ~ arr_delay, data = flights)
gf_violin(arr_delay ~ 1, data = flights)


## -----------------------------------------------------------------------------
flights %>% 
  filter(arr_delay < 120) %>% 
  gf_violin(arr_delay ~ 1, data = .)


## -----------------------------------------------------------------------------
flights2 <- flights %>% 
  mutate(season = case_when(
    month %in% c(11, 12, 1, 2, 3) ~ "winter",
    month %in% c(6,7,9) ~ "summer",
    month %in% c(4, 5) ~ "spring",
    TRUE ~ "autumn"
  ))


## -----------------------------------------------------------------------------
favstats(arr_delay ~ season, data = flights2)


## -----------------------------------------------------------------------------
favstats(arr_delay ~ month, data = flights2)



## -----------------------------------------------------------------------------
flights3 <- flights2 %>% 
  mutate(dayinyear = yday(time_hour),
         day_id = 365-(365-dayinyear),
         week = week(time_hour))


## -----------------------------------------------------------------------------
flights3 %>% 
  filter( (time_hour > "2013-11-30 23:59:59") ) %>% 
  group_by(dayinyear) %>% 
  summarise(arr_delay = median(arr_delay, na.rm = TRUE)) %>% 
  gf_line(arr_delay ~ dayinyear, data = .)


## -----------------------------------------------------------------------------
flights3 %>% 
  mutate(weekend = if_else(wday(time_hour) %in% c(1,7), TRUE, FALSE)) %>% 
  group_by(weekend) %>% 
  summarise(arr_delay_md = median(arr_delay, na.rm = TRUE))


## -----------------------------------------------------------------------------
flights4 <- flights3 %>% 
  mutate(day_rounded = round_date(time_hour, "day"))

flights4 %>% 
  group_by(day_rounded) %>% 
  summarise(arr_delay_md = median(arr_delay, na.rm = TRUE)) %>% 
  gf_line(arr_delay_md ~ day_rounded, data = .) %>% 
  gf_smooth()


## -----------------------------------------------------------------------------
flights4 %>% 
  mutate(week_rounded = round_date(time_hour, "week")) %>% 
  group_by(week_rounded) %>% 
  summarise(arr_delay_md = median(arr_delay, na.rm = TRUE)) %>% 
  gf_line(arr_delay_md ~ week_rounded, data = .) %>% 
  gf_smooth()


## -----------------------------------------------------------------------------
data(weather)
glimpse(weather)

weather2 <- weather %>% 
  mutate(date_time = round_date(time_hour, "hour"),
         day_rounded = round_date(time_hour, "day")) %>% 
  group_by(day_rounded) %>% 
  summarise_at(vars(temp, humid, wind_speed, precip, visib), median, na.rm = TRUE)


## -----------------------------------------------------------------------------
flights5 <- flights4 %>% 
  inner_join(weather2, by = c("day_rounded" = "day_rounded"))


## -----------------------------------------------------------------------------
flights5 %>% 
  select(temp, humid, wind_speed, precip, visib, arr_delay) %>% 
  correlate() %>% 
  focus(arr_delay)


## -----------------------------------------------------------------------------
flights6 <- flights5 %>% 
  select(-year)


## -----------------------------------------------------------------------------
flights7 <- flights6 %>% 
  drop_na()


## -----------------------------------------------------------------------------
nrow(flights7)/nrow(flights6)


## -----------------------------------------------------------------------------
flights7a <- std(flights7, suffix = "") 


## ----my-crossval--------------------------------------------------------------
my_crossval <- trainControl(method = "cv",
                            number = 5,
                            allowParallel = TRUE,
                            verboseIter = FALSE)


## -----------------------------------------------------------------------------
doMC::registerDoMC(cores = 2)


## -----------------------------------------------------------------------------
flights8 <- flights7a %>% 
  select_if(is.numeric)


## -----------------------------------------------------------------------------
flights9 <- flights8 %>% 
  select(-dep_delay)


## ----findlind-combos----------------------------------------------------------
findLinearCombos(flights9)


## -----------------------------------------------------------------------------
flights9a <- flights9 %>% 
  select(-c(12,14))


## -----------------------------------------------------------------------------
n_uebung <- round(.8 * nrow(flights9a), digits = 0)

uebung_index <- sample(1:nrow(flights9a), size = n_uebung)

uebung_df <- filter(flights9a, row_number() %in% uebung_index)
test_df <- filter(flights9a, !(row_number() %in% uebung_index))


## -----------------------------------------------------------------------------
(nrow(uebung_df) + nrow(test_df)) == nrow(flights9a)


## ----lm-fit1, cache = TRUE----------------------------------------------------
start <- Sys.time()
lm_fit1 <- train(arr_delay ~ .,
                 data = uebung_df,
                 method = "lm",
                 trControl = my_crossval)
end <- Sys.time()

(time_taken_lm1 <- end - start)


#saveRDS(lm_fit1, file = "lm_fit1.rds")


## -----------------------------------------------------------------------------
summary(lm_fit1)


## -----------------------------------------------------------------------------
varImp(lm_fit1)


## ----tuning-grid--------------------------------------------------------------
rf_grid <- data.frame(
  .mtry = c(4, 5, 6, 7),
  .splitrule = "variance",
  .min.node.size = 5)

rf_grid


## -----------------------------------------------------------------------------
uebung_df_small <- sample_n(uebung_df, size = 1000)


## ----rf-fit1, cache = TRUE----------------------------------------------------
start <- Sys.time()
rf_fit1 <- train(arr_delay ~ .,
                 data = uebung_df_small,
                 method = "ranger",
                 trControl = my_crossval)
end <- Sys.time()

(time_taken <- end - start)

#saveRDS(rf_fit1, file = "lm_fit1.rds")
#readRDS("lm_fit1.rds")


## -----------------------------------------------------------------------------
rf_fit1


## -----------------------------------------------------------------------------
rf_fit1$bestTune


## -----------------------------------------------------------------------------
rf_fit1$metric


## -----------------------------------------------------------------------------
modelLookup("ranger")


## -----------------------------------------------------------------------------
plot(rf_fit1)


## ----rf-fit2, cache = TRUE----------------------------------------------------
start <- Sys.time()
rf_fit2 <- train(arr_delay ~ .,
                 data = uebung_df_small,
                 method = "ranger",
                 trControl = my_crossval,
                 tuneLength = 4)
end <- Sys.time()


(time_taken <- end - start)

saveRDS(rf_fit2, file = "lm_fit2.rds")


## -----------------------------------------------------------------------------
rf_fit2


## ----rf-fit3, cache = TRUE----------------------------------------------------
start <- Sys.time()
rf_fit3 <- train(arr_delay ~ .,
                 data = uebung_df_small,
                 method = "ranger",
                 trControl = my_crossval,
                 tuneGrid = rf_grid)
end <- Sys.time()


(time_taken <- end - start)

# saveRDS(rf_fit3, file = "lm_fit3.rds")


## -----------------------------------------------------------------------------
rf_fit3


## ----nn-fit1, cache = TRUE----------------------------------------------------
start <- Sys.time()
nn_fit1 <- train(arr_delay ~ .,
                 data = uebung_df_small,
                 method = "nnet",
                 trControl = my_crossval,
                 linout = TRUE)
end <- Sys.time()


(time_taken <- end - start)

saveRDS(nn_fit1, file = "nn_fit1.rds")


## ----nn-fit2, cache = TRUE----------------------------------------------------
start <- Sys.time()
nn_fit2 <- train(arr_delay ~ .,
                 data = uebung_df,
                 method = "nnet",
                 trControl = my_crossval,
                 linout = TRUE)
end <- Sys.time()


(time_taken <- end - start)

# saveRDS(nn_fit2, file = "nn_fit2.rds")


## -----------------------------------------------------------------------------
modelLookup("neuralnet")


## -----------------------------------------------------------------------------
getModelInfo("neuralnet")


## -----------------------------------------------------------------------------

test_preds <- test_df %>% 
  select(arr_delay) %>% 
  mutate(lm1_pred = predict(lm_fit1, newdata = test_df))

test_preds <- test_df %>% 
  select(arr_delay) %>% 
  mutate(lm1_pred = predict(lm_fit1, newdata = test_df),
         rf1_pred = predict(rf_fit2, newdata = test_df),
         rf2_pred = predict(rf_fit3, newdata = test_df),
         nn1_pred = predict(nn_fit1, newdata = test_df),
         nn2_pred = predict(nn_fit2, newdata = test_df))


## -----------------------------------------------------------------------------
postResample(pred = test_preds$lm1_pred, obs = test_preds$arr_delay)


## -----------------------------------------------------------------------------
model_names <- names(test_preds)


## -----------------------------------------------------------------------------
test_pred_df <- test_preds %>% 
  map_df(~ postResample(pred = ., obs = test_preds$arr_delay)) %>% 
  mutate(statistic = c("RMSE", "Rsquared", "MAE")) %>% 
  select(statistic, everything())

test_pred_df


## -----------------------------------------------------------------------------
test_pred_df_t <- test_pred_df %>% 
  gather(key = "model_name", value = "value", -c(statistic))


## -----------------------------------------------------------------------------
test_pred_df %>% 
  gather(key = "model_name", value = "value", -c(statistic)) %>% 
  spread(key = statistic, value = value)


## -----------------------------------------------------------------------------
test_pred_df_t %>% 
  filter(statistic == "RMSE") %>% 
  top_n(2, wt = -value)


## -----------------------------------------------------------------------------
test_pred_df_t %>% 
  filter(statistic == "Rsquared") %>% 
  top_n(2, wt = value)


## -----------------------------------------------------------------------------
test_pred_df_t %>% 
  group_by(statistic) %>% 
  mutate(is_max = value == max(value),
         is_min = value == min(value)) %>% 
  ggplot(aes(y = model_name, x = value, color = is_max, shape = is_min)) +
  geom_point(size = 5) +
  facet_wrap(~statistic, scales = "free_x") +
  theme(legend.position = "bottom")

