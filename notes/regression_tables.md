Notes on Stargazer, Regression Tables, Confidence Interval
================
delpinolisette
12/01/2020

# Summary

1.  Regression Tables + Customizing them
2.  Interactions in Linear Regression Models
3.  Confidence Intervals

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------ tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts --------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(haven)
library(labelled)
library(stargazer) # creates regression tables
```

    ## Warning: package 'stargazer' was built under R version 4.0.3

    ## 
    ## Please cite as:

    ##  Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.

    ##  R package version 5.2.2. https://CRAN.R-project.org/package=stargazer

``` r
aca<-read_dta("aca.dta")
social<-read_csv("social.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   sex = col_character(),
    ##   yearofbirth = col_double(),
    ##   primary2004 = col_double(),
    ##   messages = col_character(),
    ##   primary2006 = col_double(),
    ##   hhsize = col_double()
    ## )

# Regression Tables

-   common in presenting results, people often use regression tables to
    compare several different models. example: use the `aca` data

``` r
# drop three treatment conditions to make it simples
aca<-subset(aca,cond==0|cond==1)

# first model, dependent varaible is aca (makes sense as binary output)
m1<-lm(aca~cond,aca) # DV = aca
m2<-lm(aca~cond+w1aca+w1cost,aca) # controlling for baseline covariates
m3<-lm(aca~cond*dem,aca) # interaction term between condition and Dem dummy
m4<-lm(aca~cond*dem+w1aca+w1cost,aca) # controls + interaction term
m5<-lm(cost~cond,aca)
m6<-lm(cost~cond+w1aca+w1cost,aca)
m7<-lm(cost~cond*dem,aca)
m8<-lm(cost~cond*dem+w1aca+w1cost,aca)

#stargazer(m1,m2,m3,m4,m5,m6,m7,m8) # why doesnt this make a table for me?
# this is the latex output thats why lol

stargazer(m1,m2,m3, type = "text")
```

    ## 
    ## =============================================================================================
    ##                                                Dependent variable:                           
    ##                     -------------------------------------------------------------------------
    ##                                                        aca                                   
    ##                               (1)                     (2)                      (3)           
    ## ---------------------------------------------------------------------------------------------
    ## cond                       -0.085***               -0.090***                 -0.075**        
    ##                             (0.026)                 (0.011)                  (0.035)         
    ##                                                                                              
    ## w1aca                                               0.849***                                 
    ##                                                     (0.024)                                  
    ##                                                                                              
    ## w1cost                                              0.070**                                  
    ##                                                     (0.033)                                  
    ##                                                                                              
    ## dem                                                                          0.480***        
    ##                                                                              (0.031)         
    ##                                                                                              
    ## cond:dem                                                                      -0.014         
    ##                                                                              (0.043)         
    ##                                                                                              
    ## Constant                   0.570***                 0.062***                 0.259***        
    ##                             (0.018)                 (0.013)                  (0.025)         
    ##                                                                                              
    ## ---------------------------------------------------------------------------------------------
    ## Observations                  596                     596                      523           
    ## R2                           0.018                   0.831                    0.495          
    ## Adjusted R2                  0.016                   0.830                    0.492          
    ## Residual Std. Error    0.315 (df = 594)         0.131 (df = 592)         0.229 (df = 519)    
    ## F Statistic         10.892*** (df = 1; 594) 968.112*** (df = 3; 592) 169.804*** (df = 3; 519)
    ## =============================================================================================
    ## Note:                                                             *p<0.1; **p<0.05; ***p<0.01

the best move here might be to have it as html and output as a html -
readable document or latex if i make sure to put it in math mode!! hm
looks like ill just have to copy and paste into latex doc or use
something like this:
<https://bookdown.org/yihui/rmarkdown-cookbook/latex-html.html>

i think i will use the stargazer table for exercise/assignment 5!

this creates a nice html file that we can copy and paste into the word
doc

``` r
#stargazer(m1,m2,m3, out = "table.html")
#stargazer(m1,m2,m3, type = "html")
```

and you can use star cutoff argument to tweak indicated statistical
significance cutoffs, cool.

``` r
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,type = "text",
          star.cutoffs = c(.05, .01, .001))
```

    ## 
    ## =======================================================================================================================================================================================================================
    ##                                                                                                             Dependent variable:                                                                                        
    ##                     ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ##                                                                    aca                                                                                              cost                                               
    ##                              (1)                     (2)                      (3)                      (4)                      (5)                     (6)                      (7)                     (8)           
    ## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ## cond                       -0.085**               -0.090***                 -0.075*                  -0.056**                -0.122***               -0.127***                -0.131***               -0.106***        
    ##                            (0.026)                 (0.011)                  (0.035)                  (0.020)                  (0.019)                 (0.013)                  (0.030)                 (0.025)         
    ##                                                                                                                                                                                                                        
    ## w1aca                                              0.849***                                          0.810***                                         0.230***                                         0.193***        
    ##                                                    (0.024)                                           (0.030)                                          (0.029)                                          (0.038)         
    ##                                                                                                                                                                                                                        
    ## w1cost                                              0.070*                                            0.087*                                          0.433***                                         0.439***        
    ##                                                    (0.033)                                           (0.035)                                          (0.040)                                          (0.044)         
    ##                                                                                                                                                                                                                        
    ## dem                                                                         0.480***                 0.064**                                                                  0.247***                  0.054*         
    ##                                                                             (0.031)                  (0.021)                                                                   (0.026)                 (0.026)         
    ##                                                                                                                                                                                                                        
    ## cond:dem                                                                     -0.014                  -0.054*                                                                    0.011                   -0.038         
    ##                                                                             (0.043)                  (0.024)                                                                   (0.037)                 (0.030)         
    ##                                                                                                                                                                                                                        
    ## Constant                   0.570***                0.062***                 0.259***                  0.035*                 0.475***                 0.150***                0.316***                 0.134***        
    ##                            (0.018)                 (0.013)                  (0.025)                  (0.017)                  (0.013)                 (0.016)                  (0.022)                 (0.021)         
    ##                                                                                                                                                                                                                        
    ## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ## Observations                 596                     596                      523                      523                      596                     596                      523                     523           
    ## R2                          0.018                   0.831                    0.495                    0.846                    0.068                   0.544                    0.324                   0.559          
    ## Adjusted R2                 0.016                   0.830                    0.492                    0.844                    0.066                   0.541                    0.320                   0.555          
    ## Residual Std. Error    0.315 (df = 594)        0.131 (df = 592)         0.229 (df = 519)         0.127 (df = 517)        0.227 (df = 594)         0.159 (df = 592)        0.195 (df = 519)         0.158 (df = 517)    
    ## F Statistic         10.892** (df = 1; 594) 968.112*** (df = 3; 592) 169.804*** (df = 3; 519) 566.951*** (df = 5; 517) 43.328*** (df = 1; 594) 235.187*** (df = 3; 592) 82.830*** (df = 3; 519) 131.235*** (df = 5; 517)
    ## =======================================================================================================================================================================================================================
    ## Note:                                                                                                                                                                                     *p<0.05; **p<0.01; ***p<0.001

in a paper, never use the coded variable name (ex: `cond`) because that
wont mean anything to the reader. instead relabel the regression table
with:

``` r
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,type = "text",
          star.cutoffs = c(.05, .01, .001),
          covariate.labels = c("Con Info", "Baseline Attitude", "Baseline Belief", "Democrat", "Con Info X Democrat"),
          dep.var.labels = c("Attitude toward the ACA", "Belief that ACA saves costs"))
```

    ## 
    ## =======================================================================================================================================================================================================================
    ##                                                                                                             Dependent variable:                                                                                        
    ##                     ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ##                                                          Attitude toward the ACA                                                                         Belief that ACA saves costs                                   
    ##                              (1)                     (2)                      (3)                      (4)                      (5)                     (6)                      (7)                     (8)           
    ## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ## Con Info                   -0.085**               -0.090***                 -0.075*                  -0.056**                -0.122***               -0.127***                -0.131***               -0.106***        
    ##                            (0.026)                 (0.011)                  (0.035)                  (0.020)                  (0.019)                 (0.013)                  (0.030)                 (0.025)         
    ##                                                                                                                                                                                                                        
    ## Baseline Attitude                                  0.849***                                          0.810***                                         0.230***                                         0.193***        
    ##                                                    (0.024)                                           (0.030)                                          (0.029)                                          (0.038)         
    ##                                                                                                                                                                                                                        
    ## Baseline Belief                                     0.070*                                            0.087*                                          0.433***                                         0.439***        
    ##                                                    (0.033)                                           (0.035)                                          (0.040)                                          (0.044)         
    ##                                                                                                                                                                                                                        
    ## Democrat                                                                    0.480***                 0.064**                                                                  0.247***                  0.054*         
    ##                                                                             (0.031)                  (0.021)                                                                   (0.026)                 (0.026)         
    ##                                                                                                                                                                                                                        
    ## Con Info X Democrat                                                          -0.014                  -0.054*                                                                    0.011                   -0.038         
    ##                                                                             (0.043)                  (0.024)                                                                   (0.037)                 (0.030)         
    ##                                                                                                                                                                                                                        
    ## Constant                   0.570***                0.062***                 0.259***                  0.035*                 0.475***                 0.150***                0.316***                 0.134***        
    ##                            (0.018)                 (0.013)                  (0.025)                  (0.017)                  (0.013)                 (0.016)                  (0.022)                 (0.021)         
    ##                                                                                                                                                                                                                        
    ## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ## Observations                 596                     596                      523                      523                      596                     596                      523                     523           
    ## R2                          0.018                   0.831                    0.495                    0.846                    0.068                   0.544                    0.324                   0.559          
    ## Adjusted R2                 0.016                   0.830                    0.492                    0.844                    0.066                   0.541                    0.320                   0.555          
    ## Residual Std. Error    0.315 (df = 594)        0.131 (df = 592)         0.229 (df = 519)         0.127 (df = 517)        0.227 (df = 594)         0.159 (df = 592)        0.195 (df = 519)         0.158 (df = 517)    
    ## F Statistic         10.892** (df = 1; 594) 968.112*** (df = 3; 592) 169.804*** (df = 3; 519) 566.951*** (df = 5; 517) 43.328*** (df = 1; 594) 235.187*** (df = 3; 592) 82.830*** (df = 3; 519) 131.235*** (df = 5; 517)
    ## =======================================================================================================================================================================================================================
    ## Note:                                                                                                                                                                                     *p<0.05; **p<0.01; ***p<0.001

but we can drop non essential summary stats since they are commonly
ommited on papers

``` r
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,type = "text",
          star.cutoffs = c(.05, .01, .001),
          covariate.labels = c("Con Info", "Baseline Attitude", "Baseline Belief", "Democrat", "Con Info X Democrat"),
          dep.var.labels = c("Attitude toward the ACA", "Belief that ACA saves costs"),
          keep.stat="n")
```

    ## 
    ## ================================================================================================
    ##                                                 Dependent variable:                             
    ##                     ----------------------------------------------------------------------------
    ##                           Attitude toward the ACA              Belief that ACA saves costs      
    ##                       (1)       (2)      (3)      (4)       (5)       (6)       (7)       (8)   
    ## ------------------------------------------------------------------------------------------------
    ## Con Info            -0.085** -0.090*** -0.075*  -0.056** -0.122*** -0.127*** -0.131*** -0.106***
    ##                     (0.026)   (0.011)  (0.035)  (0.020)   (0.019)   (0.013)   (0.030)   (0.025) 
    ##                                                                                                 
    ## Baseline Attitude            0.849***           0.810***           0.230***            0.193*** 
    ##                               (0.024)           (0.030)             (0.029)             (0.038) 
    ##                                                                                                 
    ## Baseline Belief               0.070*             0.087*            0.433***            0.439*** 
    ##                               (0.033)           (0.035)             (0.040)             (0.044) 
    ##                                                                                                 
    ## Democrat                               0.480*** 0.064**                      0.247***   0.054*  
    ##                                        (0.031)  (0.021)                       (0.026)   (0.026) 
    ##                                                                                                 
    ## Con Info X Democrat                     -0.014  -0.054*                        0.011    -0.038  
    ##                                        (0.043)  (0.024)                       (0.037)   (0.030) 
    ##                                                                                                 
    ## Constant            0.570*** 0.062***  0.259***  0.035*  0.475***  0.150***  0.316***  0.134*** 
    ##                     (0.018)   (0.013)  (0.025)  (0.017)   (0.013)   (0.016)   (0.022)   (0.021) 
    ##                                                                                                 
    ## ------------------------------------------------------------------------------------------------
    ## Observations          596       596      523      523       596       596       523       523   
    ## ================================================================================================
    ## Note:                                                              *p<0.05; **p<0.01; ***p<0.001

# AJPS Format

we can use the American Journal of Political Science format here use
this format for final paper unless theres a reason not to

``` r
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,type = "text",
          star.cutoffs = c(.05, .01, .001),
          covariate.labels = c("Con Info", "Baseline Attitude", "Baseline Belief", "Democrat", "Con Info X Democrat"),
          dep.var.labels = c("Attitude toward the ACA", "Belief that ACA saves costs"),
          keep.stat="n",
          style = "ajps")
```

    ## 
    ## ------------------------------------------------------------------------------------------------
    ##                           Attitude toward the ACA              Belief that ACA saves costs      
    ##                     Model 1   Model 2  Model 3  Model 4   Model 5   Model 6   Model 7   Model 8 
    ## ------------------------------------------------------------------------------------------------
    ## Con Info            -0.085** -0.090*** -0.075*  -0.056** -0.122*** -0.127*** -0.131*** -0.106***
    ##                     (0.026)   (0.011)  (0.035)  (0.020)   (0.019)   (0.013)   (0.030)   (0.025) 
    ## Baseline Attitude            0.849***           0.810***           0.230***            0.193*** 
    ##                               (0.024)           (0.030)             (0.029)             (0.038) 
    ## Baseline Belief               0.070*             0.087*            0.433***            0.439*** 
    ##                               (0.033)           (0.035)             (0.040)             (0.044) 
    ## Democrat                               0.480*** 0.064**                      0.247***   0.054*  
    ##                                        (0.031)  (0.021)                       (0.026)   (0.026) 
    ## Con Info X Democrat                     -0.014  -0.054*                        0.011    -0.038  
    ##                                        (0.043)  (0.024)                       (0.037)   (0.030) 
    ## Constant            0.570*** 0.062***  0.259***  0.035*  0.475***  0.150***  0.316***  0.134*** 
    ##                     (0.018)   (0.013)  (0.025)  (0.017)   (0.013)   (0.016)   (0.022)   (0.021) 
    ## N                     596       596      523      523       596       596       523       523   
    ## ------------------------------------------------------------------------------------------------
    ## ***p < .001; **p < .01; *p < .05

## exercise

-   Run the following models using the social pressure experiment data
    -   m1: regress 2006 turnout on messages
    -   m2: add age (2006-yearofbirth) as a control to m1
    -   m3: add the interaction between messages and 2004 turnout to m1
    -   m4: add age (2006-yearofbirth) as a control to m3

``` r
head(social)
```

    ## # A tibble: 6 x 6
    ##   sex    yearofbirth primary2004 messages   primary2006 hhsize
    ##   <chr>        <dbl>       <dbl> <chr>            <dbl>  <dbl>
    ## 1 male          1941           0 Civic Duty           0      2
    ## 2 female        1947           0 Civic Duty           0      2
    ## 3 male          1951           0 Hawthorne            1      3
    ## 4 female        1950           0 Hawthorne            1      3
    ## 5 female        1982           0 Hawthorne            1      3
    ## 6 male          1981           0 Control              0      3

``` r
names(social)
```

    ## [1] "sex"         "yearofbirth" "primary2004" "messages"    "primary2006"
    ## [6] "hhsize"

``` r
# first lets change the baseline
social$messages<-fct_relevel(social$messages, "Control")
m1<-lm(primary2006~messages,social)
social$age<-2006 - social$yearofbirth
m2<-lm(primary2006~messages+age,social)

m3<-lm(primary2006~messages*primary2004,social) # tracks interaction

m3<-lm(primary2006~messages*primary2004+age,social)
```

## exercise

-   Create a table that reports the results of m1 to m4, using stargazer

``` r
stargazer(m1,m4,type="text", keep.stat = "n", style="ajps")
```

    ## 
    ## ----------------------------------------
    ##                    primary2006    aca   
    ##                      Model 1    Model 2 
    ## ----------------------------------------
    ## messagesCivic Duty  0.018***            
    ##                      (0.003)            
    ## messagesHawthorne   0.026***            
    ##                      (0.003)            
    ## messagesNeighbors   0.081***            
    ##                      (0.003)            
    ## cond                           -0.056***
    ##                                 (0.020) 
    ## dem                            0.064*** 
    ##                                 (0.021) 
    ## w1aca                          0.810*** 
    ##                                 (0.030) 
    ## w1cost                          0.087** 
    ##                                 (0.035) 
    ## cond:dem                       -0.054** 
    ##                                 (0.024) 
    ## Constant            0.297***    0.035** 
    ##                      (0.001)    (0.017) 
    ## N                    305866       523   
    ## ----------------------------------------
    ## ***p < .01; **p < .05; *p < .1

## How to observe interactions:

example for observing interactions in the study:

non-multiplication way: 1. build two different models and compare by eye

``` r
summary(lm(primary2006~messages,subset(social,primary2004==0)))
```

    ## 
    ## Call:
    ## lm(formula = primary2006 ~ messages, data = subset(social, primary2004 == 
    ##     0))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.3064 -0.2554 -0.2371  0.6936  0.7629 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        0.237110   0.001278 185.471  < 2e-16 ***
    ## messagesCivic Duty 0.018293   0.003131   5.843 5.13e-09 ***
    ## messagesHawthorne  0.023296   0.003139   7.421 1.17e-13 ***
    ## messagesNeighbors  0.069296   0.003147  22.020  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4329 on 183094 degrees of freedom
    ## Multiple R-squared:  0.002736,   Adjusted R-squared:  0.00272 
    ## F-statistic: 167.5 on 3 and 183094 DF,  p-value: < 2.2e-16

``` r
summary(lm(primary2006~messages,subset(social,primary2004==1)))
```

    ## 
    ## Call:
    ## lm(formula = primary2006 ~ messages, data = subset(social, primary2004 == 
    ##     1))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.4823 -0.3858 -0.3858  0.5966  0.6142 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        0.385805   0.001770 218.019  < 2e-16 ***
    ## messagesCivic Duty 0.017641   0.004340   4.065 4.81e-05 ***
    ## messagesHawthorne  0.028281   0.004324   6.541 6.14e-11 ***
    ## messagesNeighbors  0.096525   0.004309  22.403  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4896 on 122764 degrees of freedom
    ## Multiple R-squared:  0.004136,   Adjusted R-squared:  0.004111 
    ## F-statistic: 169.9 on 3 and 122764 DF,  p-value: < 2.2e-16

``` r
# can just compare coefficients here
```

using multiplication syntax: 1. make one model and get the coefficients
on the interaction terms, it’s the same number as before but assures
accuracy

``` r
summary(lm(primary2006~messages*primary2004,social))
```

    ## 
    ## Call:
    ## lm(formula = primary2006 ~ messages * primary2004, data = social)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.4823 -0.3064 -0.2371  0.5966  0.7629 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     0.2371099  0.0013481 175.879  < 2e-16 ***
    ## messagesCivic Duty              0.0182927  0.0033013   5.541 3.01e-08 ***
    ## messagesHawthorne               0.0232963  0.0033105   7.037 1.97e-12 ***
    ## messagesNeighbors               0.0692962  0.0033186  20.881  < 2e-16 ***
    ## primary2004                     0.1486951  0.0021307  69.787  < 2e-16 ***
    ## messagesCivic Duty:primary2004 -0.0006521  0.0052225  -0.125    0.901    
    ## messagesHawthorne:primary2004   0.0049851  0.0052165   0.956    0.339    
    ## messagesNeighbors:primary2004   0.0272291  0.0052108   5.226 1.74e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4565 on 305858 degrees of freedom
    ## Multiple R-squared:  0.02944,    Adjusted R-squared:  0.02942 
    ## F-statistic:  1326 on 7 and 305858 DF,  p-value: < 2.2e-16

### On CI:

-   try to make your confidence interval as narrow as possible
-   if the confidence interval includes 0, even if its narrow, then your
    finding is not stat significantly finding.
-   unless you want a null finding, keep it nonzero and narrow
-   ex: effect of vaccination on autism - CI is 0 on this study -
    incredibly unlikely that autism caused by vaccination.
