#**********************************************************
#
# File: Makefile
# Project: Logic Project - Turing Machine
# Author: David Chocholatý <xchoch09>
# Year: 2024
#
#**********************************************************

PL = swipl
EXECUTABLE = flp23-log
MAIN = main.pl
LOGIN = xchoch09
ZIP_FILE = flp-log-$(LOGIN).zip

.PHONY: all pack clean

all: $(ALL)
	swipl -q -g start -o $(EXECUTABLE) -c $(MAIN)

pack: $(ZIP_FILE)

clean:
	rm -f $(EXECUTABLE) $(ZIP_FILE)

$(ZIP_FILE): *.pl Makefile README.md tests/*
	zip $@ $^
