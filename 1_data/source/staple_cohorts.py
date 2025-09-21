import os
import pandas as pd
import gc
import psutil

directory = "/Users/wernerd/Desktop/Daniel Werner/CleanCohorts"

dfs = []
i = 0
id_offset = 0 
hh_offset = 0

for filename in os.listdir(directory):
    if filename.endswith('.dta'):
        i += 1
        file_path = os.path.join(directory, filename)
        print(f"Reading file {i}: {filename}")
        
        df = pd.read_stata(file_path)
        dfs.append(df)
        
        # Check memory after each load
        process = psutil.Process()
        mem = process.memory_info().rss / 1e9
        print(f"Memory used so far: {mem:.2f} GB")
        
        gc.collect()

print("All cohorts loaded. Concatenating...")

# Do the concat just once — much more efficient
all_data = pd.concat(dfs, ignore_index=True)
del dfs
gc.collect()

print("Saving to disk...")
all_data.to_stata(os.path.expanduser("~/Desktop/ENOE_panel.dta"), write_index=False)
print("Done!")