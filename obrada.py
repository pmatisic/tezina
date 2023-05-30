import pandas as pd

# Broj redaka koje treba preskočiti
skip_rows = 117

# Ukupan broj redaka u datoteci
total_lines = 381

# Izračunaj broj redaka koje treba preskočiti na kraju
end_skip = total_lines - 370

# Definiraj naslove stupaca
headers = [
    "Density",
    "Percent body fat",
    "Age",
    "Weight",
    "Height",
    "Neck",
    "Chest",
    "Abdomen 2",
    "Hip",
    "Thigh",
    "Knee",
    "Ankle",
    "Biceps",
    "Forearm",
    "Wrist"
]

# Učitaj .txt datoteku preskačući zadane retke i dodaj naslove stupaca
df = pd.read_csv('bodyfat.txt', delimiter="\s+", skiprows=skip_rows, skipfooter=end_skip, names=headers, engine='python')

# Pretvori u .csv datoteku koristeći ";" kao separator
df.to_csv('data.csv', sep=";", index=False)

# Prikazuje prvih 5 redaka
print(df.head())
