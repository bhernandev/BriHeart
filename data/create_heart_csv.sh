exec 3> $2

echo "creation_date,start_date,end_date,heart_rate" >&3 

grep 'HKQuantityTypeIdentifierHeartRate' $1 | tr -d '/<>"' | sed 's/[a-zA-Z]*=/ /g' | awk -F "  " '{print $6","$7","$8","$9}' >&3

