import os
import gzip
import shutil

# Walk through the current directory and find files with the .tsv extension
for root, dirs, files in os.walk("."):
    for file in files:
        if file.endswith(".tsv"):  # Match all files ending with .tsv
            file_path = os.path.join(root, file)
            compressed_file_path = file_path + ".gz"

            try:
                # Compress the file using gzip
                with open(file_path, "rb") as f_in:
                    with gzip.open(compressed_file_path, "wb") as f_out:
                        shutil.copyfileobj(f_in, f_out)

                # Remove the original file after compression
                os.remove(file_path)
                print(f"Compressed and removed: {file_path} -> {compressed_file_path}")

            except Exception as e:
                print(f"Error processing {file_path}: {e}")

