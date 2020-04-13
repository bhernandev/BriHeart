exec 3> $2

echo "creation_date,start_date,end_date,sleep_type" >&3 

grep 'HKCategoryTypeIdentifierSleepAnalysis' $1 | tr -d '/<>"' | sed 's/[a-zA-Z]*=/ /g' | awk -F "  " '{print $5","$6","$7","$8}' >&3
