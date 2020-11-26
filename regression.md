Regression
================
delpinolisette
11/26/2020

# Regression 1

So far, have only really manipulated data and reshaped/recoded
variables. The next step is to use this tidied data to perform
regression.

Linear regression can be thought of predicting one variable using a
second variable.

## Exercise 1:

Try to predict election outcome based on facial appearance (compare
election outcomes for a candiate based on ‘facial competence’ ratings by
surveyed students.) Build a linear regression model.

### Step 1

Decide which is the binary predictor variable and which is the binary
outcome variable.

``` r
# load in libraries to read csv and then plot using ggplot. 
library(tidyverse)
library(ggrepel)

face_rate<-read_csv("face.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   year = col_double(),
    ##   state = col_character(),
    ##   winner = col_character(),
    ##   loser = col_character(),
    ##   w.party = col_character(),
    ##   l.party = col_character(),
    ##   d.comp = col_double(),
    ##   r.comp = col_double(),
    ##   d.votes = col_double(),
    ##   r.votes = col_double()
    ## )

``` r
# take a look at face_rate
head(face_rate)
```

    ## # A tibble: 6 x 10
    ##    year state winner    loser     w.party l.party d.comp r.comp d.votes r.votes
    ##   <dbl> <chr> <chr>     <chr>     <chr>   <chr>    <dbl>  <dbl>   <dbl>   <dbl>
    ## 1  2000 CA    Feinstein Campbell  D       R        0.565  0.435 5790154 3779325
    ## 2  2000 DE    Carper    Roth      D       R        0.342  0.658  181387  142683
    ## 3  2000 FL    Nelson    McCollum  D       R        0.612  0.388 2987644 2703608
    ## 4  2000 GA    Miller    Mattingly D       R        0.542  0.458 1390428  933698
    ## 5  2000 HI    Akaka     Carroll   D       R        0.680  0.320  251130   84657
    ## 6  2000 IN    Lugar     Johnson   R       D        0.321  0.679  684242 1419629

It looks like the data frame contains `d.comp` and `r.comp` headers.
Those record measures of competency based only on brief viewings of the
candidate’s face.

Also, `l.party` tracks the party of the candidate.

`r.vote` and `d.vote` track the election outcomes (number of votes) for
that particular election.

Thus, the independent binary predictor should correspond to the
compentency ratings and the binary outcome should predict to whether the
candidate won or lost.

#### Prepping The Outcome Variable:

To prep the outcome variable, calculate the difference in the share of
votes

### Step 2
