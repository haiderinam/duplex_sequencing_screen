import os
import pandas as pd

def process_file(file_path):
    # Read the CSV file
    df = pd.read_csv(file_path)

    # Filter out rows where the lengths of 'ref' and 'alt' strings are different
    df_filtered = df[df['ref'].str.len() == df['alt'].str.len()]

    # Write the modified dataframe back to the original file
    df_filtered.to_csv(file_path, index=False)

def find_and_process_variants_unique_ann(directory):
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file == 'variants_unique_ann.csv':
                file_path = os.path.join(root, file)
                process_file(file_path)
#                print(f'Processed: {file_path}')

# Replace '/path/to/your/directory' with your actual working directory path
working_directory = '.'
find_and_process_variants_unique_ann(working_directory)
