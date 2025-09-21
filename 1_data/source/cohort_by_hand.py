import pandas as pd
import pyarrow.parquet as pq
from functools import reduce

##### ======================================================= #####
        # This script processes ENOE data to make cohorts
        # that were too large for automated R script to handle.
##### ======================================================= #####

# Convert .dta Stata files to Parquet format
    # Define paths {donwload, upload}
convert = pd.read_stata(f"{download_path}/2024_T4.dta")

# Write to Parquet format
convert.to_parquet(f"{upload_path}/2024_T4.parquet", engine="pyarrow")


def make_labels(filename):
    year = filename[:4]
    quarter = filename[5:7]
    return f"{year}_{quarter}"

def load_and_label(file_path, filename, ent_value):
    df = pd.read_parquet(file_path)
    df = df[df['n_ent'] == ent_value]
    df['trim'] = make_labels(filename)
    return df

base_path = "/Users/wernerd/Desktop"

first = load_and_label(f"{base_path}/2023_T3.parquet", "2023_T3.parquet", 1)
second = load_and_label(f"{base_path}/2023_T4.parquet", "2023_T4.parquet", 2)
third = load_and_label(f"{base_path}/2024_T1.parquet", "2024_T1.parquet", 3)
fourth = load_and_label(f"{base_path}/2024_T2.parquet", "2024_T2.parquet", 4)
fifth = load_and_label(f"{base_path}/2024_T3.parquet", "2024_T3.parquet", 5)

# Merge all
dfs = [first, second, third, fourth, fifth]
common_cols = list(reduce(lambda x, y: set(x) & set(y), [df.columns for df in dfs]))
dfs = [df[common_cols] for df in dfs]
panel = pd.concat(dfs, ignore_index=True)


# Convert age column
panel['age'] = pd.to_numeric(panel['eda'], errors='coerce')

# Group by individual-level identifiers
individual_keys = ['cd_a', 'ent', 'con', 'v_sel', 'n_hog', 'h_mud', 'n_ren']

# Compute age min/max/diff
panel['min_age'] = panel.groupby(individual_keys)['age'].transform('min')
panel['max_age'] = panel.groupby(individual_keys)['age'].transform('max')
panel['age_diff'] = abs(panel['max_age'] - panel['min_age'])

# Filter age discrepancy < 2
panel = panel[panel['age_diff'] < 2]

# Group again by individual *including sex*
individual_keys_with_sex = individual_keys + ['sex']

# Count number of appearances (n = 5 needed)
panel['total_n'] = panel.groupby(individual_keys_with_sex)['sex'].transform('count')
panel = panel[panel['total_n'] == 5]

# Create IDs — Individual
panel['id'] = panel.groupby(individual_keys).ngroup() + 1

# Household ID
hh_keys = ['cd_a', 'ent', 'con', 'v_sel', 'n_hog', 'h_mud']
panel['id_hog'] = panel.groupby(hh_keys).ngroup() + 1

# Dwelling ID
viv_keys = ['cd_a', 'ent', 'con', 'v_sel']
panel['id_viv'] = panel.groupby(viv_keys).ngroup() + 1

# Create full municipality code
panel = panel.dropna(subset=['ent', 'mun'])
panel['municipality'] = panel['ent'].astype(int).astype(str) + "0" + panel['mun'].astype(int).apply(lambda x: f"{x:02d}")



# Save to .dta
cohort_number = 62  
output_path = f"/Users/wernerd/Desktop/Cohort_{cohort_number}.dta"
panel.to_stata(output_path, write_index=False)
