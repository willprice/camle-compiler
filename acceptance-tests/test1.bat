COMPILER=../../dist/build/compiler/compiler


function compile() {
	local _program_name="$1"
	local _program="$(readlink -e "./programs/${_program_name}.w")"
	$COMPILER "${_program}" -o "${_program_name}"
}

@test "test1.w: Test expression evalutation with _writes_" {
#	compile test1
#	run ./test1
	read -r -d '' expected <<-EOF
	10023
	10023
	76
	76
EOF
	[ "$output" = "$expected" ]
}
