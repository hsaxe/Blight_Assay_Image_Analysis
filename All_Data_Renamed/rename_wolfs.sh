#! usr/bin/bash

# for file in $(ls | grep -v "DS"); do
#     cp "$file" "$file.new"
#     sed 's/i_Side/i_DS1-3_Side/g' "$file.new" > "$file.new.tmp"
#     mv "$file.new.tmp" "$file.new"
# done

cd All_Data/

for file in $(ls | grep -v "DS"); do
    newname=$(echo "$file" | sed 's/dai_Side/dai_DS1-3_Side/g')
    cp "$file" "$newname"
done



