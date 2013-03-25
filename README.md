CASE-Spring-2013
================

Code to reproduce examples shared at my presentation given at the Spring 2013 meeting of the Casualty Actuaries of the Southeast.

The presentation focused on the view of loss reserving as an ordinary least squares regression (OLS) problem. Typically, actuaries weight together link ratios to arrive at a set of age-to-age factors. However, there is no a priori reason for doing so. Deviation from unweighted least squares reflects an assumption about the variance of the response variable. That assumption should be tested before another model is assumed.

Using NAIC data where the actual results are known, it is possible to test the efficacy of various weighting schemes. We find that unweighted OLS often performs at least as well as common weighting assumptions. Further, we find that an assumption of heteroskedastic error terms is not often borne out by the data. Non-normal error terms is another matter.
