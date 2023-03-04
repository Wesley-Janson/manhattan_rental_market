# packages ----
rm(list=ls())
library(tidyverse)


# read in data --------
# no idea why this isn't working
#base_path <- system('git rev-parse --show-toplevel', intern = T)
base_path <-
  '/Users/michaelwagner/dropbox/Q2/time_series/final/chicago_rental_index/data/'

load(paste0(base_path, 'rent_arima_expanding_rmse.RData'))

load(paste0(base_path, 'rent_ets_expanding_rmse.RData'))

load(paste0(base_path, 'rent_var_expanding_rmse.RData'))

arma_error_inventory_results_cv <-
  read.csv(paste0(base_path, 'arma_error_inventory_results_cv.csv'))

# make data into data frame -------
results_df <- 
  data.frame(
    index = arma_error_inventory_results_cv$X,
    ets  = rent_ets_expanding_rmse,
    arima = rent_arima_expanding_rmse,
    arma_err = arma_error_inventory_results_cv$RMSE_arma_errors_inventory,
    var = rmse_exp)

# make graph -------

plot_object <- results_df %>% 
  pivot_longer(ets:var, names_to = 'model') %>% 
  ggplot(aes(x = index,
             y = value,
             color = model)) +
  geom_line(size = 0.6) +
  theme_classic() +
  scale_y_continuous(breaks=c(seq(0, 200, 50))) + 
  geom_hline(yintercept=seq(0,200, by=50), size=0.1, linetype=2)

colors_vec<- ggplot_build(plot_object)$data[[1]] %>% 
  count(colour) %>%
  pull(colour)


results_df %>% 
  pivot_longer(ets:var, names_to = 'model') %>% 
  ggplot(aes(x = index,
             y = value,
             color = model)) +
  geom_line(size = 0.6) +
  theme_classic() +
  labs(color = 'Model') + 
  scale_y_continuous(breaks=c(seq(0, 200, 50))) + 
  geom_hline(yintercept=seq(0,200, by=50), size=0.1, linetype=2) +
  scale_color_manual(labels = c("Arima", "Arma Errors", 'Exp Smoothing', 'Var'),
                     values = c('#F8766D', '#7CAE00', '#00BFC4', '#C77CFF'))

# arima, arma err, ets, var
# red, green, blue, purple

      
