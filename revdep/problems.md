# CALIBERrfimpute

<details>

* Version: 1.0-1
* Source code: https://github.com/cran/CALIBERrfimpute
* Date/Publication: 2018-06-11 10:40:00 UTC
* Number of recursive dependencies: 62

Run `revdep_details(,"CALIBERrfimpute")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘simstudy_survival.Rnw’using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘simstudy_survival.Rnw’
      ...
    
    > upper_bias <- bias + 1.96 * se_bias
    
    > plot(-100, 0, type = "p", pch = 15, cex = 1.3, ylab = "Bias", 
    +     xlab = "Number of trees", xlim = c(0, 100), ylim = c(min(lower_bias), 
    +        .... [TRUNCATED] 
    
      When sourcing ‘simstudy_survival.R’:
    Error: need finite 'ylim' values
    Execution halted
    ```

# cobalt

<details>

* Version: 4.0.0
* Source code: https://github.com/cran/cobalt
* URL: https://github.com/ngreifer/cobalt
* BugReports: https://github.com/ngreifer/cobalt/issues
* Date/Publication: 2020-01-08 14:20:10 UTC
* Number of recursive dependencies: 118

Run `revdep_details(,"cobalt")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      3   4  married  re74  re75
      3   5  married  re74  re75
      4   1  married  re74  re75
      4   2  married  re74  re75
      4   3  married  re74  re75
      4   4  married  re74  re75
      4   5  married  re74  re75
      5   1  married  re74  re75
      5   2  married  re74  re75
      5   3  married  re74  re75
      5   4  married  re74  re75
      5   5  married  re74  re75
    > 
    > #Matching using within-imputation propensity scores
    > mt.out1 <- matchthem(treat ~ age + educ + race + 
    +                        married + nodegree + re74 + re75, 
    +                        data = imp, approach = "within")
    Error in mice::is.mids(object) : 
      argument "object" is missing, with no default
    Calls: matchthem ... complete.mids -> <Anonymous> -> complete.mids -> <Anonymous>
    Execution halted
    ```

# hot.deck

<details>

* Version: 1.1
* Source code: https://github.com/cran/hot.deck
* Date/Publication: 2016-01-04 17:37:48
* Number of recursive dependencies: 104

Run `revdep_details(,"hot.deck")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘Using_Hot_Deck_Data.Rnw’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘Using_Hot_Deck_Data.Rnw’
      ...
    
    > s <- summary(pool(lm.mids(AI ~ lagAI + pctchgPCGNP + 
    +     PCGNP + pctchgLPOP + LPOP + MIL2 + LEFT + BRIT + POLRT + 
    +     CWARCOW + IWARCOW2, data .... [TRUNCATED] 
    
    > round(s, 4)
    
      When sourcing 'Using_Hot_Deck_Data.R':
    Error: non-numeric variable(s) in data frame: term
    Execution halted
    ```

