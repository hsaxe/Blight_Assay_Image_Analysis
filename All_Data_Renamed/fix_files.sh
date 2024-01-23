#! /usr/bin/bash

for file in *.csv; do
    echo -e "kernel_name\ttotal.area\tblighted.values" > "new_files/$file.new"
    cut -d "," -f4-6 "$file" > new_cols.tmp
    cat new_cols.tmp >> "new_files/$file.new"
    # mv "$file.new" "$file"
    rm new_cols.tmp
    echo "Header added to $file"
done


