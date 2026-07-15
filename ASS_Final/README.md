# Final Project - Instrumental Variables and Causal Effects under Non-Compliance

This folder contains the final project of the Causal Inference course.

The objective of this project is to estimate the causal effect of a job intervention program on participants' self-efficacy, measured through the `job_seek` outcome variable.

The analysis is based on the **Jobs II dataset**, where the intervention is characterized by a randomized assignment mechanism with imperfect compliance.

## Dataset structure

The main variables considered are:

- **Assignment variable (`Z`)**
    - Randomized invitation/encouragement to participate in the program.

- **Treatment receipt (`W`)**
    - Actual participation in the intervention.

- **Outcome (`Y`)**
    - `job_seek`, measuring job-seeking self-efficacy.

- **Covariates**
    - Age
    - Sex
    - Marital status
    - Non-white indicator
    - Education
    - Income categories
    - Pre-treatment depression score

## Analysis workflow

### 1. Non-compliance analysis

The first step evaluates the relationship between treatment assignment and actual treatment receipt.

The assignment mechanism is analyzed through:

- the joint distribution of `Z` and `W`;
- identification of the compliance structure;
- verification of a **one-sided non-compliance design**.

In this setting, some individuals assigned to treatment do not participate, while individuals in the control group cannot receive the treatment.

### 2. Covariate and outcome analysis

The analysis investigates:

- differences in covariates between assigned and non-assigned groups;
- differences between participants and non-participants;
- outcome distributions across assignment and treatment receipt groups.

Several graphical analyses are used to visualize potential imbalance and treatment effects.

### 3. Intention-To-Treat (ITT) estimation

The first causal estimand is the **Intention-To-Treat effect**:

\[
ITT = E[Y|Z=1] - E[Y|Z=0]
\]

This measures the causal effect of being assigned to the intervention, regardless of actual participation.

The uncertainty of the estimate is computed using analytical standard errors.

### 4. Complier Average Causal Effect (CACE)

Because of treatment non-compliance, the effect among actual participants cannot be directly estimated by comparing treated and untreated individuals.

Under the assumptions of:

- random assignment;
- exclusion restriction;
- monotonicity;

the **Complier Average Causal Effect (CACE)** is estimated:

\[
CACE = \frac{ITT_Y}{ITT_W}
\]

The CACE represents the causal effect of the intervention for individuals whose participation is influenced by the randomized assignment.

### 5. Method of Moments Estimation

A second approach estimates the principal strata parameters:

- proportion of compliers;
- proportion of never-takers;
- outcome means for different compliance groups;
- treatment effect among compliers.

The method exploits the one-sided compliance structure to recover latent quantities that cannot be directly observed.

### 6. Bootstrap Inference

Bootstrap procedures are implemented to evaluate uncertainty in the estimated quantities.

Different bootstrap sample sizes are considered:

- 2,000 repetitions;
- 10,000 repetitions;
- 50,000 repetitions.

For each bootstrap iteration:

- the dataset is resampled;
- ITT and CACE are re-estimated;
- standard errors and confidence intervals are computed.

## Methods implemented

The project applies:

- Randomized experiment analysis
- Non-compliance analysis
- Intention-To-Treat estimation
- Instrumental Variable framework
- CACE/LATE estimation
- Method of Moments estimation
- Bootstrap inference

The complete analysis is implemented in **R**.
