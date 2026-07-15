# Assignment 2 - Covariate Adjustment and Causal Effect Estimation

This folder contains the second assignment of the Causal Inference course.

The objective of this analysis is to estimate the causal effect of being treated in a **large hospital** compared to a **small hospital** on one-year patient survival.

The analysis is based on the **Karolinska dataset**, where:

- Treatment variable: `hvdiag` (hospital type)
    - 0 = Small hospital
    - 1 = Large hospital
- Outcome variable: `survival` (one-year survival indicator)
- Covariates:
    - `age`
    - `rural`
    - `male`

## Analysis workflow

The analysis follows the main steps required in observational causal inference:

### 1. Covariate balance assessment

The first step evaluates whether treated and control groups are comparable before adjustment.

The analysis includes:

- descriptive statistics for treatment and control groups;
- visualization of covariate distributions;
- normalized differences (Delta) to measure imbalance;
- dispersion comparison for continuous variables.

The results highlight important differences between hospital groups, motivating the use of adjustment methods.

### 2. Stratification and Standardization (SRE)

A stratification approach is applied by creating subclasses based on:

- age categories;
- gender;
- rural/urban residence.

The goal is to compare treated and control units within comparable strata and estimate the Average Treatment Effect (ATE).

Confidence intervals and variance estimates are computed for the adjusted effect.

### 3. Lin Regression Adjustment

The Lin (2013) estimator is implemented to improve precision and adjust for remaining imbalance.

The model includes:

- centered covariates;
- treatment-covariate interactions;
- robust standard errors.

The resulting coefficient provides an adjusted estimate of the causal effect.

### 4. Propensity Score Adjustment

A logistic regression model is used to estimate the probability of treatment assignment:

\[
P(Hospital = 1 | Age, Rural, Male)
\]

The analysis includes:

- estimation of propensity scores;
- evaluation of overlap between treated and control groups;
- trimming of units outside the common support;
- re-estimation of propensity scores after trimming;
- balance evaluation before and after adjustment.

### 5. Propensity Score Stratification

The trimmed sample is divided into propensity score subclasses.

Within each subclass:

- covariate balance is assessed;
- normalized differences are computed;
- treatment effects are estimated.

A Love plot is used to compare covariate balance before and after stratification.

### 6. Matching Estimation

Finally, a matching approach is applied:

- 1-to-1 matching with replacement;
- Mahalanobis distance;
- exact matching on gender and rural status;
- bias adjustment using age.

The Average Treatment Effect on the Treated (ATT) is estimated together with confidence intervals.

## Methods implemented

The assignment applies several causal inference techniques:

- Covariate balance diagnostics
- Stratification adjustment
- Standard Regression Estimation (SRE)
- Lin regression adjustment
- Propensity Score Matching
- Propensity Score Stratification
- Covariate-adjusted matching

The complete analysis is implemented in **R**.
