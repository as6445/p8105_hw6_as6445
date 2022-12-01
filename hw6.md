Homework 6
================
Ayako Sekiya

## Problem 1

## Problem 2

``` r
homicide = 
  read_csv(file = "./data/homicide-data.csv") %>% 
  janitor::clean_names() 
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Create a city_state variable (e.g. “Baltimore, MD”), and a binary
variable indicating whether the homicide is solved. The number of
unsolved homicides is those for which the disposition is “Closed without
arrest” or “Open/No arrest”.Omit cities Dallas, TX; Phoenix, AZ; and
Kansas City, MO – these don’t report victim race. Also omit Tulsa, AL –
this is a data entry mistake. For this problem, limit your analysis
those for whom victim_race is white or black. Be sure that victim_age is
numeric.

``` r
homicide_data=
  homicide %>% 
  unite("city_state", city:state, remove = FALSE, sep=", ") %>% 
  group_by(city_state) %>% 
  filter(city_state!="Dallas, TX" & city_state!="Phoenix, AZ" & city_state!="Kansas City, MO"& city_state!="Tulsa, AL") %>%
  filter(victim_race=="White" | victim_race=="Black") %>% 
  mutate(
    resolved = as.numeric(disposition == "Closed by arrest"),
    victim_age = as.numeric(victim_age),
    victim_race = fct_relevel(victim_race, "White")) %>% 
  drop_na(victim_age) %>% 
  select(resolved, victim_age, victim_race, victim_sex)
```

    ## Adding missing grouping variables: `city_state`

For the city of Baltimore, MD, use the glm function to fit a logistic
regression with resolved vs unresolved as the outcome and victim age,
sex and race as predictors. Save the output of glm as an R object; apply
the broom::tidy to this object

``` r
fit_logistic= homicide_data %>% 
  filter(city_state=="Baltimore, MD") %>% 
  glm(resolved ~ victim_age + victim_race + victim_sex, data = ., family = binomial()) %>% 
  broom::tidy() 
```

``` r
summary(fit_logistic)
```

    ##      term              estimate         std.error          statistic      
    ##  Length:4           Min.   :-0.8545   Min.   :0.003324   Min.   :-6.1839  
    ##  Class :character   1st Qu.:-0.8449   1st Qu.:0.104463   1st Qu.:-5.1594  
    ##  Mode  :character   Median :-0.4242   Median :0.156446   Median :-3.4210  
    ##                     Mean   :-0.1378   Mean   :0.138235   Mean   :-2.0401  
    ##                     3rd Qu.: 0.2829   3rd Qu.:0.190218   3rd Qu.:-0.3018  
    ##                     Max.   : 1.1517   Max.   :0.236725   Max.   : 4.8653  
    ##     p.value         
    ##  Min.   :0.000e+00  
    ##  1st Qu.:8.600e-07  
    ##  Median :1.300e-06  
    ##  Mean   :1.074e-02  
    ##  3rd Qu.:1.074e-02  
    ##  Max.   :4.296e-02

Obtain the estimate and confidence interval of the adjusted odds ratio
for solving homicides comparing male victims to female victims keeping
all other variables fixed.

``` r
fit_logistic %>% 
  mutate(log_OR = estimate,
         OR = exp(estimate),
        ci_low=exp(estimate-1.96*std.error),
        ci_high=exp(estimate+1.96*std.error)) %>%
  select(term,log_OR, OR,ci_low,ci_high) %>% 
  filter(term == "victim_sexMale")
```

    ## # A tibble: 1 × 5
    ##   term           log_OR    OR ci_low ci_high
    ##   <chr>           <dbl> <dbl>  <dbl>   <dbl>
    ## 1 victim_sexMale -0.854 0.426  0.325   0.558

The adjusted odds ratio for solving homicides comparing male victims to
female victims keeping all other variables fixed is 0.426 (95%CI: 0.325,
0.558). In the city of Baltimore the odds of solving homicides among
male victims is 0.426 times the odds of solving homicides among female
victims.

#### Mapping

Now run glm for each of the cities in your dataset, and extract the
adjusted odds ratio (and CI) for solving homicides comparing male
victims to female victims. Do this within a “tidy” pipeline, making use
of purrr::map, list columns, and unnest as necessary to create a
dataframe with estimated ORs and CIs for each city.

``` r
homicide_all=
  homicide_data %>% 
  nest(data=-city_state) %>% 
  mutate(
    models = map(.x=data, ~glm(formula=resolved~victim_age + victim_race + victim_sex, data = .x, family = binomial())),
    results = map(models, broom::tidy)) %>% 
  select(-data, -models) %>% 
  unnest(results) %>% 
  mutate(log_OR = estimate,
         OR = exp(estimate),
        ci_low=exp(estimate-1.96*std.error),
        ci_high=exp(estimate+1.96*std.error)) %>%
  select(city_state,term,log_OR, OR,ci_low,ci_high) %>% 
  filter(term == "victim_sexMale")
```

Create a plot that shows the estimated ORs and CIs for each city.
Organize cities according to estimated OR, and comment on the plot.

``` r
homicide_plot= homicide_all %>% 
  filter(str_detect(term, "victim_sex")) %>% 
  ggplot(aes(x = reorder(city_state, OR), y = OR)) + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high)) +
  geom_point(alpha = 0.5 ) +  
  labs(
    title = "Adjusted OR for Solving Homicides comparing Males to Females",
    x = "city, state",
    y = "Adjusted OR") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.position = "none", 
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.7))
  
homicide_plot
```

<img src="hw6_files/figure-gfm/plot-1.png" width="90%" />

``` r
ggsave("results/homicide_plot.pdf", homicide_plot, width = 20, height = 10)
```

Based on this plot, New York, NY has the smallest point estimate for OR.
This means that New York has the smallest odds for solving homicides for
males compared to females. Albuquerque, NM has the highest point
estimate for OR which means that this city has the highest odds for
solving homicides for males compared to females. However, Albuquerque
has a very wide confidence interval, especially when comparing to other
cities.

## Problem 3

Load and clean the data for regression analysis (i.e. convert numeric to
factor where appropriate, check for missing data, etc.).

``` r
birthweight = 
  read_csv(file = "./data/birthweight.csv") %>% 
  janitor::clean_names() %>% 
  mutate( babysex = as.factor(babysex),
          frace = as.factor(frace),
          malform = as.factor(malform),
          mrace = as.factor(mrace))
```

    ## Rows: 4342 Columns: 20
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (20): babysex, bhead, blength, bwt, delwt, fincome, frace, gaweeks, malf...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sapply(birthweight, function(x) sum(is.na(x)))
```

    ##  babysex    bhead  blength      bwt    delwt  fincome    frace  gaweeks 
    ##        0        0        0        0        0        0        0        0 
    ##  malform menarche  mheight   momage    mrace   parity  pnumlbw  pnumsga 
    ##        0        0        0        0        0        0        0        0 
    ##    ppbmi     ppwt   smoken   wtgain 
    ##        0        0        0        0

#### Model Building

Propose a regression model for birthweight. This model may be based on a
hypothesized structure for the factors that underly birthweight, on a
data-driven model-building process, or a combination of the two.
Describe your modeling process and show a plot of model residuals
against fitted values – use add_predictions and add_residuals in making
this plot.

Bqsed on the homework question and the codebook, I will look at the
following predictors for my model: - `gaweeks`: gestational age
(weeks)  
- `mrace`: maternal race  
- `ppwt`: mother’s pre-pregnancy weight - `fincome`: family monthly
income (in hundreds, rounded)

The outcome variable will be: - `bwt`: baby’s birth weight (grams)

``` r
model = lm(bwt ~ gaweeks+mrace+ppwt+fincome, data = birthweight) %>% 
broom::tidy()
```

#### Cross Validation

Compare your model to two others:

One using length at birth and gestational age as predictors (main
effects only)

``` r
maineffect=lm(bwt ~ blength + gaweeks, data =birthweight) %>% 
broom::tidy()
```

One using head circumference, length, sex, and all interactions
(including the three-way interaction) between

``` r
interactions=
lm(bwt ~ blength*bhead*babysex, data = birthweight) %>% 
 broom::tidy()
```

Make this comparison in terms of the cross-validated prediction error;
use crossv_mc and functions in purrr as appropriate.

``` r
bw_df = crossv_mc(birthweight, 100)

bw_df= bw_df%>% 
    mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  )

bw_df= bw_df %>% 
  mutate(
    model  = map(train, ~lm(bwt ~ gaweeks+mrace+ppwt+fincome, data = .x)),
    maineffect  = map(train, ~lm(bwt ~ blength + gaweeks, data = .x)),
    interactions  = map(train, ~lm(bwt ~ blength*bhead*babysex, data = .x))) %>% 
  mutate(
    rmse_model = map2_dbl(model, test, ~rmse(model = .x, data = .y)),
    rmse_maineffect = map2_dbl(maineffect, test, ~rmse(model = .x, data = .y)),
    rmse_interactions = map2_dbl(interactions, test, ~rmse(model = .x, data = .y)))

# Checking for errors
bw_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model", 
    values_to = "rmse",
    names_prefix = "rmse_") %>% 
  mutate(model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin()
```

<img src="hw6_files/figure-gfm/cross_validation-1.png" width="90%" />
