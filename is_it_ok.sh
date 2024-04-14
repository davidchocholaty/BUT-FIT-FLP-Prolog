#!/usr/bin/env bash

# Autor: xchoch09
# Tento testovací skript vychází z testovacího skriptu is_it_ok.sh pana doktora Zbyňka Křivky
# Použití: ./test_project.sh xlogin00 testdir

# Funkce pro výpis barevného textu
function echo_color() {
    local color=$1
    local text=$2
    case $color in
        red) color='\033[1;31m' ;;
        green) color='\033[1;32m' ;;
        blue) color='\033[1;34m' ;;
        yellow) color='\033[1;33m' ;;
        *) color='\033[0m' ;;
    esac
    echo -e "${color}${text}\033[0m"
}

# Funkce pro porovnání výstupů
function compare_output() {
    # Získání argumentů
    local actual_output_file=$1
    local expected_output_file=$2

    # Normalizace skutečného a očekávaného výstupu
    local normalized_actual=$(normalize_output "$actual_output_file")
    local normalized_expected=$(normalize_output "$expected_output_file")

    # Porovnání skutečného a očekávaného výstupu
    echo -n "Comparing outputs..."
    if [[ "$normalized_actual" == "$normalized_expected" ]]; then
        echo_color green "OK"
    else
        echo_color red "ERROR (Actual output does not match the expected output.)"
        echo "Actual  : '$normalized_actual'"
        echo "Expected: '$normalized_expected'"
        exit 1
    fi
}

# Function to normalize and sort the cycles in output data
function normalize_output() {
    local file=$1
    local output=""
    while IFS= read -r line; do
        if [[ -n "$line" ]]; then
            local sorted_line=$(echo "$line" | tr ' ' '\n' | awk -F '-' '{if ($1 < $2) print $1"-"$2; else print $2"-"$1;}' | sort | tr '\n' ' ')
            output+=$(echo "$sorted_line" | tr ' ' '\n' | sort | tr '\n' ' ')$'\n'
        fi
    done < "$file"
    echo "$output" | sort
}

# Kontrola počtu argumentů
if [[ $# -ne 2 ]]; then
    echo_color red "Wrong count of arguments: "$#"!"
    echo "Usage: $0 LOGIN TESTDIR"
    echo "* LOGIN - student login, e.g., xlogin00"
    echo "* TESTDIR - directory to perform tests"
    exit 1
fi

# Získání argumentů programu
LOGIN=$1
TESTDIR=$2
ARCHIVE="flp-log-${LOGIN}.zip"

# Testovací vstup a očekávaný výstup
TEST_INPUT="S a B a\nB a B b\nB b B R\nB c B a\nB c F c\nB c B a\naaacaa"
EXPECTED_OUTPUT="Saaacaa\nBaaacaa\nBbaacaa\nbBaacaa\nbBbacaa\nbbBacaa\nbbBbcaa\nbbbBcaa\nbbbFcaa"

# Vstupní, aktuální a očekávaný výstupní soubor
INPUT_FILE="input.txt"
EXPECTED_OUTPUT_FILE="expected_output.txt"
ACTUAL_OUTPUT_FILE="actual_output.txt"

# Požadované soubory v archivu
REQUIRED_FILES=("Makefile" "README.md")

# Overeni serveru (ala Eva neni Merlin)
echo -n "Testing on Merlin: "
HN=`hostname`
if [[ $HN = "merlin.fit.vutbr.cz" ]]; then
  echo_color green "Yes"
else
  echo_color blue "No"
fi

# Příprava testovacího adresáře
echo -n "Preparing testing directory..."
if [[ -d "$TESTDIR" ]]; then
    echo_color yellow "Test directory $TESTDIR already exists."
    read -p "Do you want to delete the existing directory and continue? (y/n): " confirm
    if [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]]; then
        echo -n "Cleaning up test directory $TESTDIR..."
        rm -rf "$TESTDIR"
        mkdir -p "$TESTDIR"
        echo_color green "OK"
    else
        echo_color red "Test aborted."
        exit 1
    fi
else
    echo_color green "OK"
    mkdir -p "$TESTDIR"
fi
cd "$TESTDIR"

# Kontrola existence archivu
echo -n "Checking archive existence..."
if [[ ! -f "../$ARCHIVE" ]]; then
    echo_color red "ERROR (Archive $ARCHIVE does not exist.)"
    exit 1
fi
echo_color green "OK"

# Rozbalení archivu
echo -n "Unzipping archive..."
unzip -o "../$ARCHIVE" >/dev/null
if [[ $? -ne 0 ]]; then
    echo_color red "ERROR (Failed to unzip the archive.)"
    exit 1
fi
echo_color green "OK"

# Kontrola existence požadovaných souborů
echo -n "Checking for required files..."
for file in "${REQUIRED_FILES[@]}"; do
    if [[ ! -f "$file" ]]; then
        echo_color red "ERROR (Required file '$file' is missing in the archive.)"
        exit 1
    fi
done
echo_color green "OK"

# Kontrola toho zda archiv obsahuje i jiné soubory než požadované
for file in *; do
    # Ignorovat soubory, které končí na .pl
    if [[ "$file" == *.pl ]]; then
        continue
    fi
    if [[ ! " ${REQUIRED_FILES[*]} " =~ " ${file} " ]]; then
        if [[ -d "$file" ]]; then
            echo_color blue "Notice: A directory '$file' was found in the archive."
        elif [[ -f "$file" ]]; then
            echo_color yellow "Notice: An extra file '$file' found in the archive."
        fi
    fi
done

# Spuštění make
echo -n "Running make..."
make >/dev/null 2>&1
if [[ $? -ne 0 ]]; then
    echo_color red "ERROR (Make failed.)"
    exit 1
fi
echo_color green "OK"

echo -n "Verifying 'flp23-log' executable existence..."
if [[ ! -f "flp23-log" ]]; then
    echo_color red "ERROR (File 'flp23-log' was not created.)"
    exit 1
fi
echo_color green "OK"

# Vytvoření vstupního souboru
echo -n "Creating input file..."
echo -e $TEST_INPUT > $INPUT_FILE
echo_color green "OK"

# Vytvoření souboru s očekávaným výstupem
echo -n "Creating expected output file..."
echo -e $EXPECTED_OUTPUT > $EXPECTED_OUTPUT_FILE
echo_color green "OK"

# Spuštění programu s vstupním souborem a uložení výstupu do souboru
echo -n "Running program with test input..."
./flp23-log < $INPUT_FILE > $ACTUAL_OUTPUT_FILE
if [[ $? -ne 0 ]]; then
    echo_color red "ERROR (Program execution failed.)"
    exit 1
fi
echo_color green "OK"

# Porovnání skutečného výstupu s očekávaným výstupem
compare_output $ACTUAL_OUTPUT_FILE $EXPECTED_OUTPUT_FILE

echo_color green "All tests passed successfully!"
cd ..