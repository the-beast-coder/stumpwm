bat=$(acpi | awk '{print substr($4, 1, length($4)-1) " " substr($3, 1, 3) " " substr($5, 2, length($5)-1)}')
time=$(date '+%b %d (%a) %I:%M%p')
echo "$bat | $time "
