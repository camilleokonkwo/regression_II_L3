week_3_activities
================
2024-02-09

### Exercise 1:

##### loading in data

``` r
whas100 = read_csv("data/whas100.csv")
```

    ## Rows: 100 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): admitdate, foldate
    ## dbl (7): id, los, lenfol, fstat, age, gender, bmi
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### Part A: Take the WHAS100 dataset, create a binary variable ‘losbin’ based on length of hospital stay such that 0: los ≤ 5 and 1: los \> 5.

##### creating a binary variable losbin using a binary operator

``` r
whas100$losbin = 1*(whas100$los>5)
```

### Part B: Test whether the survival curves for the two groups defined by ‘losbin’ are the same using the log-rank test at α = 0.05. Give the null and alternative hypothesis, test statistic, degrees of freedom, p-value, and conclusion

##### testing survival curves using log rank

``` r
library(survival)

print(survdiff(Surv(lenfol, fstat)~losbin, data = whas100), digits = 5)
```

    ## Call:
    ## survdiff(formula = Surv(lenfol, fstat) ~ losbin, data = whas100)
    ## 
    ##           N Observed Expected (O-E)^2/E (O-E)^2/V
    ## losbin=0 51       26   24.326   0.11517   0.22619
    ## losbin=1 49       25   26.674   0.10503   0.22619
    ## 
    ##  Chisq= 0.2  on 1 degrees of freedom, p= 0.634

#### Our hypotheses:

- H0 :Slos≤5(t) = Slos\>5(t), for all t ≤ τ
- Hα :Slos≤5(t) = / = Slos\>5(t), for some t ≤ τ

#### Test statistic:

- Q_log-rank = 0.2262

#### Degree of freedom:

- df = 1

#### P-value:

- Pr(χ21 ≥ 0.2262) = 0.634 \> 0.05

#### Conclusion:

We fail to reject H0 at the significance level 0.05. The survival curves
for patients stay in hospital for 5 days or less and patients there for
more than 5 days are not significantly different at the 5% level.
