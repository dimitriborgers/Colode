compiler="./toplevel.native"
make clean
make
if [ $? -ne 0 ]; then
	echo "Compilation failed."
	exit 1
fi
success=0
failure=0
for t in ./Tests/*.cld; 
	do
		echo "================="
		name=$(head -1 $t | sed 's/^\/\///')
		echo $name
		echo "-----------------"
		$compiler -l $t > test.ll
		res=$?
		if echo $t | grep -qi "fail" ; then
			if [ $res -eq 0 ]; then
				echo "$name failed. Compilation should have failed."
				failure=$((failure +1))
			else
				echo "$name succeeded. Compilation failed correctly."
				success=$((success +1))
			fi
		else
			expected=$(head -2 $t | tail -1 | sed 's/^\/\///')
			llc test.ll > test.s
			gcc -fPIC -lc -static-libgcc -lm -no-pie -o test test.s matrix.o
			output=$(./test)
			echo $output
			echo $output | grep -qa "$expected"
			if [ $? -eq 0 ]; then
				echo "$name succeeded; output matched expected: $expected"
				success=$((success +1))
			else
				echo "$name failed; output did not match expected: $expected"
				failure=$((failure +1))
			fi
		fi
		rm -f test.ll test.s
		echo ""
	done
echo "================="
echo "$success tests passed and $failure tests failure."