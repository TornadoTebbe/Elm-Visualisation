import pandas as pd

df = pd.read_csv("./Daten/Student_Behaviour.csv")

print(df.isna().sum())