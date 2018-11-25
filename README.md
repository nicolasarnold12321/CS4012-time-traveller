# Time-traveller
# CS4012: Lab 2

## Requirement

- [x] Step through program
- [x] Inspect variable values
- [x] Inspect History of variable
- [x] Step Backwards through program
- [x] Conditional BREAKPOINT.
- [x] Error recovery for when a variable doesn't exist

## Screenshot

![Screenshot]("./screenshot.png")

* The file column is the statements that have been executed, and the statement that is next to be executed.

* The variables column is the current environment.

* The output column is the accumulated output for the program so far.

## Setup

Build the stack project:
```bash
stack build
```

The program can be executed by passing in the path to the file as the first argument.

Run the interpreter with the input file test.txt:
```bash
stack exec time-traveller-exe resources/test.txt
```
## Commands
 * `next` : Execute the next statement
 * `back` : Step back and undo the last statement
 * `inspect` : Input a variable's name to view it's history
 * `state`: Shows the current stack of statements
