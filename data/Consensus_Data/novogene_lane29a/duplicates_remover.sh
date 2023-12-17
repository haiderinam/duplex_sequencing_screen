#!/bin/bash

# Find files named 'variants_unique_ann.tsv' and process them
find . -type f -name 'variants_unique_ann.tsv' | while read file
do
    echo "Processing $file"
    # Assuming 'ref' is a column and we know its number, e.g., 2
    # Adjust the column number as needed
    awk -F'\t' 'length($4) != 4' "$file" > "${file%.tsv}_filtered.tsv"
done
