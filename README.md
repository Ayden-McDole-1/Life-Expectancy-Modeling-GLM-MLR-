Life Expectancy Modeling Project
Overview

This project analyzes the relationship between socioeconomic and health indicators and life expectancy using country-level data from the World Health Organization (WHO). The goal was to build predictive statistical models and evaluate how different modeling approaches perform when classical regression assumptions are violated.

The project focuses on the model-building process, including variable selection, diagnostic testing, and comparison between classical linear regression and generalized linear models.

Research Question

Which socioeconomic and health indicators are most strongly associated with life expectancy, and do generalized linear models outperform classical linear regression when modeling global life expectancy data?

Dataset

The dataset contains country-year observations including:

Life expectancy

Adult mortality

Infant and child mortality

Immunization rates

GDP and healthcare expenditure

HIV prevalence

Education levels

Source: World Health Organization life expectancy dataset.

Project Workflow
1. Data Cleaning (Python / Pandas)

Data preprocessing was performed in Python and included:

Identifying missing values

Removing rows with incomplete observations

Standardizing column names

Exporting a clean dataset for statistical modeling

2. Exploratory Data Analysis

Initial exploration included:

Distribution analysis of life expectancy

Scatterplots examining relationships between predictors

Identification of potential nonlinear patterns

3. Linear Regression Modeling (R)

Multiple linear regression models were constructed using:

Forward stepwise AIC selection

Backward stepwise AIC selection

Both procedures converged to the same reduced model.

4. Model Diagnostics

Several diagnostic tests were performed to evaluate regression assumptions:

Multicollinearity

Variance Inflation Factor (VIF) analysis revealed severe multicollinearity between infant deaths and under-five deaths.

The broader variable (under-five deaths) was retained.

Heteroscedasticity

Breusch-Pagan testing indicated heteroscedasticity in residuals. Several corrective approaches were explored:

Box-Cox transformation

Weighted least squares

5. Generalized Linear Modeling

Because life expectancy is a positive continuous variable with non-constant variance, a Gamma GLM was explored.

Two link functions were compared:

Inverse link

Log link

Stepwise AIC selection favored the Gamma model with inverse link.

6. Model Evaluation

Model performance was evaluated using out-of-sample mean squared error on a held-out test dataset.

Models compared:

Null model

Multiple linear regression

Gamma GLM

The Gamma GLM achieved the lowest test MSE, indicating improved predictive performance.

Key Findings

Important predictors of life expectancy include:

Adult mortality

Child mortality

HIV prevalence

Education levels

Immunization rates

Results highlight the strong relationship between public health infrastructure and population longevity.
