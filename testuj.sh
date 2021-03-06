make generate > /dev/null
echo "Rozpoczynam testowanie."
if [[ $1 == "" ]];
	then
		l=5
	else
		l=$1
	fi

for  (( i=1; i<=((l)); ++i ))
do
	./generate
	execution_time="$( TIMEFORMAT='%3R' ;time ( ./executable < test.in > answer.out ) 2>&1 1>/dev/null )"
	if y=$((cmp answer.out test.out > /dev/null) 2>&1);
	then
    	echo -e "Test $i -- \e[0;32mOK\e[0m -- $execution_time" ;
    else
    	echo -e "Test $i -- \e[0;31mWRONG\e[0m -- $execution_time" ;
		exit 1
    fi
done
make clean > /dev/null
