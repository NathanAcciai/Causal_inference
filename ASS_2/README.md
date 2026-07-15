# Assignment 2 - Instrumental Variables and Causal Effects

This folder contains the second assignment of the Causal Inference course.

The analysis focuses on causal estimation in the presence of **non-compliance**, where the treatment received by individuals may differ from the treatment assignment.

The dataset is analyzed using an encouragement design framework:

- `Z`: randomized treatment assignment (instrument);
- `W`: actual treatment received;
- `Y`: observed outcome.

The main objectives are:

- estimate the **Intention-To-Treat (ITT)** effect, measuring the impact of treatment assignment;
- estimate the **Complier Average Causal Effect (CACE/LATE)**, measuring the causal effect among individuals whose treatment status is affected by the assignment;
- verify compliance patterns;
- compute uncertainty of estimates using bootstrap procedures.

The analysis applies instrumental variable methods and follows the assumptions of:

- random assignment;
- exclusion restriction;
- monotonicity.

The implementation is developed in **R**.
