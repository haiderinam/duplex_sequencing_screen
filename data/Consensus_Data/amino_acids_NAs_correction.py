import os
import pandas as pd
# 1.7.24 This code looks in all variant caller outputs (files called variants_unique_ann.csv), and makes sure N/A amino acid substitutions have an amino_acid change listed as "N/A" and not a blank amino acid substitution type.
def find_and_process_files(root_dir):
    for root, dirs, files in os.walk(root_dir):
        for file in files:
            if file == 'variants_unique_ann.csv':
                process_file(os.path.join(root, file))

def process_file(file_path):
    # Read the CSV file into a DataFrame
    df = pd.read_csv(file_path)

    # Identify rows where ref_aa is 'N' and alt_aa is 'A'
    condition = (df['ref_aa'] == 'N') & (df['alt_aa'] == 'A')

    # Update amino_acids column to 'N/A' for these rows
    df.loc[condition, 'amino_acids'] = 'N/A'

    # Overwrite the original file with the modified DataFrame
    df.to_csv(file_path, index=False)
#    print(f"Updated file: {file_path}")

# Replace 'your_directory_path' with the path of the directory you want to search
find_and_process_files('.')

