import pandas as pd

## Load in Data
df = pd.read_csv(r"C:\Users\abmcd\OneDrive\Desktop\Career Stuff\Mini Project\Life Expectancy Data.csv")
print(df.head())

## See which columns have NA data
na_boolean_df = df.isnull().any()
na_boolean_df.columns = ["Column Name", "NA Present"]
print(na_boolean_df)

## Fill NA Columns
df_clean = df.dropna()


## Check Column Types
print(df_clean.dtypes)

## Change Column Names
df_clean = df_clean.rename(columns = {"Life expectancy " : "Life_Expectancy", "Adult Mortality" : "Adult_Mortality", "infant deaths" : "Infant_Deaths", 
                                      "percentage expenditure" : "Percentage_Expenditure", "Hepatitis B" : "Hepatitis_B", "Measles " : "Measles", " BMI " : "BMI",
                                      "under-five deaths " : "Under_Five_Deaths", "Total expenditure" : "Total_Expenditure", "Diphtheria " : "Diphtheria", " HIV/AIDS" : "HIV/AIDS",
                                      " thinness  1-19 years" : "Thinness_Ages_10_to_19", " thinness 5-9 years"
                                      : "Thinness_Ages_5_to_9", "Income composition of resources" : "Income_Composition_of_Resources"
})
print(df_clean.columns)

## Export Cleaned Data
df_clean.to_csv(r"C:\Users\abmcd\OneDrive\Desktop\Career Stuff\Mini Project\Life Expectancy Data Clean.csv")
