# Logic Project - Turing Machine
The second project from the FLP course at BUT FIT.

- **Author**: David Chocholatý
- **Login**: xchoch09
- **Academic year**: 2023/2024
- **Assignment name**: Turing Machine

## Build and run

The program uses the Makefile for the compilation. To compile, run the following command:

```
make
```

To run the program using the own input file, run it as follows:

```
./flp23-log < your_input_file
```

To run the prepared test input file, replace ```your_input_file``` with some of the files mentioned in the [Custom inputs](#custom-inputs).

Extension: as an extension, a user parameter is defined. For the single sub-part of the cycle detection approach, it is necessary to define the maximum search depth. By default, the value is defined as ```10000```, however, it can be set by a user as follows (for depth ```2000```):

```
./flp23-log 2000 < your_input_file
```

## Program description
In this section, each part of the program is described.

### Input processing
For reading input the predicates from the provided ```input2.pl``` file are used (namely ```read_line/2```, ```isEOFEOL/1``` and ```read_lines/1```). Then the rules and tape are extracted from the input. For both, it is checked validity of the input (```valid_rules``` and ```is_valid_tape``` predicates with partial predicates used). For the rules, the necessary whitespaces are removed (```cut_whitespaces_ll``` predicate).

### Main algorithm
After the input processing, all rules are stored using the dynamic predicate ```rule/4```. Then all of the pre-processing is done.

The main Turing Machine simulation algorithm is implemented in the ```run``` predicate.

The first predicate clause handles the situation, if the state is the accepting state. If it is true, all configuration history including the last one is printed in the required format and the search is stopped. The history is passed as a predicate parameter.

The second predicate clause handles the most important part of the program. First, it checks the depth of search (because of the cycle detection more described in the following subsection). Then the current configuration can't be a part of the configuration history because for the same reason.

Then the next rule is chosen and executed, such as the tape character is replaced by a new one (```replace_symbol``` predicate) or the tape head is moved to left or right (```update_head_position``` predicate). The current configuration is added to the entire history of previous configurations and the simulation follows with the next simulation step.

The last, third predicate clause handles the situation to prevent cycling. It is checked if the current configuration is the same as some of the previous configurations. If so, the search path is cut. About cycle detection, the following subsection describes it more deeply.

Lastly, if the tape header exceeds the right tape, the whitespace character is added.

### Cycle detection
For the cycle detection, the two combined approaches were used. 

First, the maximum search depth is checked. If the depth of the search exceeds the maximum allowed depth (default or user-defined), the next search using this path is cut.

Second, it is checked (third ```run``` predicate clause) if the configuration was already defined in the configuration histories. If so, the search fails using this path and is cut.

## Custom inputs

The prepared custom test inputs are stored in the ```test``` directory. Each test contains ```test_name.in``` a file containing the output, ```test_name.out``` containing the expected output and as the last ```test_name.ref``` output containing the expected return code.

The following table briefly describes the tests and adds the run times:

| Test | Time (s) | Description |
| ---  | ---  | ---         |
| abcd | 0,098 | Move over the symbols and end with the accepting symbol on the empty symbol. |
| an2anbn | 22,300 | Convert language $\Delta a^n\Delta^{\omega}$ to language $\Delta a^nb^n\Delta^{\omega}$ for $n > 0$. This example is taken over from the [slides](https://www.fit.vutbr.cz/study/courses/TIN/public/Prednasky/tin-pr05-ts.pdf) for the TIN course at BUT FIT. |
| final_as_first | 0,085 | Even the non-acceptance path of configuration can be done, there is only one acceptance path by using the rule from the S to F state as the first rule. |
| looping | 0,033s | The rules are written as the Turing Machine will cycle in a loop. |
| move_left_abnormal | 0,048 | The rule moves the head only to the left. |
| move_right_abnormal | 0,060 | The rule moves the head only to the right. |
| reference | 0,032 | Reference test provided in the assignment. |

The measurements were provided using the ```test``` util as follows:

```
test ./flp23-log < your_input_file
```

System and HW parameters:
- OS: *Ubuntu 22.04.4 LTS*
- CPU: *Intel(R) Core(TM) i7-6500U CPU @ 2.50GHz*
- RAM: *8GiB SODIMM DDR4 Synchronous Unbuffered (Unregistered) 2133 MHz (0,5 ns)*
