### Group
------
*Steven Bonilla*					sb3914@columbia.edu<br>
*Takuma Yasuda*						ty2353@columbia.edu<br>
*Willie Koomson*					wvk2101@columbia.edu<br>
*Tahmid Munat*						tfm2109@columbia.edu<br>
*Dimitri Borgers*					djb2195@columbia.edu

### Requirements
-----
To build the Colode compiler, you need a version of Ocaml with the Map.Make.find_opt function, 4.0.5 or greater. Additionally, you must install ocamlbuild, ocamlfind, and the llvm bindings for ocaml. 

### Build Colode compiler
------
	make

### Run & validate integration tests
------
	./integration_tests.sh

### Commands
------
    make -> to build
    make clean -> to clean

### Current issues
------
Comments in the same line as code will not be parsed correctly.


### Test Descriptions
-----
#### Positive tests:

 1. Array concatenation - validates the concatenation of two integer arrays into a new array with both of their elements. (not in MicroC)
 2. Assign Add - validates the AssignAdd operation, which is essentially syntax sugar for assigning the result of a binary expression (not in MicroC)
 3. DeclAssignment expression - validates the use declaration-assignment expressions as values in other expression. (not in MicroC)
 4. Exponentiation - validates the exponentiation operator on float types. (not in MicroC)
 5. If - ensures that ‘if’ statements branch correctly and execute the desired code.
 6. String Comparison - validates the comparison of two strings using the == operator (not in MicroC)
 7. String Concatenation - validates the concatenation of two strings into a new string. (not in MicroC)
 8. While - validates that the body of the while loop is executed the correct number of times (twice)

#### Failing tests:

 9. AssignAdd w/ wrong type - validates that the semantic checker rejects a binary add operation between two different type expressions (int and float).
 10. While w/ invalid predicate - validates that the semantic checker rejects a non-boolean predicate
 11. Invalid Assign - validates that the semantic checker rejects an assignment between names and values of different types (string and int).

