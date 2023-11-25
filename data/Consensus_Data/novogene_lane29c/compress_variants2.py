import os
import gzip
import shutil

def compress_file(file_path):
    with open(file_path, 'rb') as f_in:
        with gzip.open(file_path + '.gz', 'wb') as f_out:
            shutil.copyfileobj(f_in, f_out)

def find_and_compress_variants_ann(directory):
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file == 'variants_ann.csv':
                file_path = os.path.join(root, file)
                compress_file(file_path)
                print(f'Compressed: {file_path}')

# Replace '.' with your actual working directory path
working_directory = '.'
find_and_compress_variants_ann(working_directory)
