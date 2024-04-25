#!/bin/bash

# Set the path to your binary
BINARY="./flp23-log"

# Set the directory where your test files are located
TEST_DIR="./tests_large"

# Variables for colored output
GREEN='\033[0;32m'  # Green color for passed tests
RED='\033[0;31m'    # Red color for failed tests
NC='\033[0m'        # No color (to reset text formatting)

# Check if the binary exists
if [ ! -x "$BINARY" ]; then
    echo "Error: $BINARY binary not found or not executable"
    exit 1
fi

# Check if the test directory exists
if [ ! -d "$TEST_DIR" ]; then
    echo "Error: Test directory $TEST_DIR not found"
    exit 1
fi

# Initialize variables to count passed tests
passed_count=0
total_count=0

# Iterate over each test input file in the directory
for input_file in "$TEST_DIR"/*.in; do
    # Extract the base name of the test (without extension)
    test_name=$(basename "$input_file" .in)
    
    # Define paths to expected output, actual output, and exit status reference files
    expected_output_file="$TEST_DIR/$test_name.out"
    actual_output_file="$TEST_DIR/$test_name.actual_out"
    exit_status_ref_file="$TEST_DIR/$test_name.ref"
    
    # Check if the exit status reference file exists
    if [ ! -f "$exit_status_ref_file" ]; then
        echo "Error: Exit status reference file $exit_status_ref_file not found for test $test_name"
        continue
    fi
    
    # Read the expected exit status from the reference file
    expected_exit_status=$(<"$exit_status_ref_file")
    
    # Run the binary with the input file and capture the output
    "$BINARY" < "$input_file" > "$actual_output_file"
    
    # Capture the exit status of the binary
    actual_exit_status=$?
    
    # Compare the actual exit status with the expected exit status
    if [ "$actual_exit_status" -eq "$expected_exit_status" ] && \
       diff -q "$expected_output_file" "$actual_output_file" >/dev/null; then
        # Test passed
        echo -e "Test $test_name: ${GREEN}PASSED${NC}"
        ((passed_count++))
    else
        # Test failed
        echo -e "Test $test_name: ${RED}FAILED${NC} (Expected Exit Status: $expected_exit_status, Actual: $actual_exit_status)"
        echo "----- Expected Output ($expected_output_file) -----"
        cat "$expected_output_file"
        echo "----- Actual Output ($actual_output_file) -----"
        cat "$actual_output_file"
    fi
    
    ((total_count++))
done

# Display the total number of passed tests out of the total number of tests
echo ""
echo "Total Passed Tests: $passed_count / $total_count"

# Clean up - remove any actual output files generated
rm -f "$TEST_DIR"/*.actual_out
