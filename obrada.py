import pandas as pd

# Broj redaka koje treba preskočiti
skip_rows = 117

# Ukupan broj redaka u datoteci
total_lines = 381

# Izračunaj broj redaka koje treba preskočiti na kraju
end_skip = total_lines - 370

# Definiraj naslove stupaca
headers = [
    "Density determined from underwater weighing",
    "Percent body fat from Siri's (1956) equation",
    "Age (years)",
    "Weight (lbs)",
    "Height (inches)",
    "Neck circumference (cm)",
    "Chest circumference (cm)",
    "Abdomen 2 circumference (cm)",
    "Hip circumference (cm)",
    "Thigh circumference (cm)",
    "Knee circumference (cm)",
    "Ankle circumference (cm)",
    "Biceps (extended) circumference (cm)",
    "Forearm circumference (cm)",
    "Wrist circumference (cm)"
]

# Učitaj .txt datoteku preskačući zadane retke i dodaj naslove stupaca
df = pd.read_csv('bodyfat.txt', delimiter="\s+", skiprows=skip_rows, skipfooter=end_skip, names=headers, engine='python')

# Pretvori u .csv datoteku koristeći ";" kao separator
df.to_csv('data.csv', sep=";", index=False)

# Prikazuje prvih 5 redaka
print(df.head())
