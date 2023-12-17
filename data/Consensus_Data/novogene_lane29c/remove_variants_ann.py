import os

def delete_csv_if_gz_exists(directory):
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file == 'variants_ann.csv':
                file_path = os.path.join(root, file)
                gz_file_path = file_path + '.gz'

                if os.path.exists(gz_file_path):
                    os.remove(file_path)
#                    print(f'Deleted: {file_path}')

# Replace '.' with your actual working directory path
working_directory = '.'
delete_csv_if_gz_exists(working_directory)

