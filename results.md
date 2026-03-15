# Final Model Results

## Linear Regression Model

Final model predictors:

Life_Expectancy ~ Adult_Mortality + Alcohol + Percentage_Expenditure + 
BMI + Under_Five_Deaths + Polio + HIV_AIDS + 
Income_Composition_of_Resources + Schooling

Test MSE: 14.174649


## Gamma GLM (Inverse Link)

Life_Expectancy ~ Adult_Mortality + Infant_Deaths + 
Percentage_Expenditure + BMI + Under_Five_Deaths +
Polio + HIV_AIDS + Income_Composition_of_Resources + Schooling

Test MSE: 12.88644


## Model Comparison

| Model | Test MSE |
|------|------|
| Null Model | 74.94838 |
| Linear Regression | 14.174649 |
| Gamma GLM | 12.88644 |

The Gamma GLM achieved the lowest test MSE and was selected as the final model.
