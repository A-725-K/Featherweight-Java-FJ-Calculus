#!/bin/bash

EXE='FJ_Interpreter.exe' #executable of the interpreter of FJ
PARSER_ERR='### Parser Error ! ###'

##################
### CHECK TEST ###
##################

@test 'test for tests: quis custodiet ipsos custodies ?' { }

#####################
### PARSER ERRORS ###
#####################

@test 'parser error: "extend" instead of "extends" in class declaration' {
    value=$(./$EXE <<< $'tests/parser_test1.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: parenthesis after class declaration (mistaken for a method)' {
    value=$(./$EXE <<< $'tests/parser_test2.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: no comma between two parameters in a method' {
    value=$(./$EXE <<< $'tests/parser_test3.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: no semicolon after field declaration' {
    value=$(./$EXE <<< $'tests/parser_test4.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: double quotes after extends in class declaration' {
    value=$(./$EXE <<< $'tests/parser_test5.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: class declaration without extends' {
    value=$(./$EXE <<< $'tests/parser_test6.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: class mispelled in class declaration' {
    value=$(./$EXE <<< $'tests/parser_test7.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: no class after extends in class declaration' {
    value=$(./$EXE <<< $'tests/parser_test8.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: no curly bracket in class declaration' {
    value=$(./$EXE <<< $'tests/parser_test9.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: no curly bracket in method declaration' {
    value=$(./$EXE <<< $'tests/parser_test10.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: field name starts with a number' {
    value=$(./$EXE <<< $'tests/parser_test11.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: class name starts with a number' {
    value=$(./$EXE <<< $'tests/parser_test12.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: method body does not terminate with semicolon' {
    value=$(./$EXE <<< $'tests/parser_test13.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: method does not contain keyword "return" in his body' {
    value=$(./$EXE <<< $'tests/parser_test14.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: method with empty body' {
    value=$(./$EXE <<< $'tests/parser_test15.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: class declaration without keyword class' {
    value=$(./$EXE <<< $'tests/parser_test16.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: semicolon after class declaration' {
    value=$(./$EXE <<< $'tests/parser_test17.txt' | tail -3 | head -1)
    expected='### Parser Error ==> ;'
    [ "$value" == "$expected" ]
}

@test 'parser error: new in field declaration' {
    value=$(./$EXE <<< $'tests/parser_test18.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

@test 'parser error: method before fields' {
	value=$(./$EXE <<< $'tests/parser_test19.txt' | tail -1)
    expected=$PARSER_ERR
    [ "$value" == "$expected" ]
}

##########################
### TYPECHECKER ERRORS ###
##########################

@test 'typechecker error: cyclic dependencies 4 classes' {
    value=$(./$EXE <<< $'tests/typechecker_test1.txt' | tail -1)
    expected='*** TypeException ==> Cyclic dependency between two types! ***'
    [ "$value" == "$expected" ]
}

@test 'typechecker error: mutual dependent classes' {
    value=$(./$EXE <<< $'tests/typechecker_test2.txt' | tail -1)
    expected='*** TypeException ==> Cyclic dependency between two types! ***'
    [ "$value" == "$expected" ]
}

@test 'typechecker error: two fields with the same name in class declaration' {
    value=$(./$EXE <<< $'tests/typechecker_test3.txt' | tail -1)
    expected='*** DuplicateFieldException ==> There are two fields with the same name in class A! ***'
    [ "$value" == "$expected" ]
}

@test 'typechecker error: two class with the same name in the program' {
    value=$(./$EXE <<< $'tests/typechecker_test4.txt' | tail -1)
    expected='*** DuplicateClassException ==> There are two classes with the same name: A! ***'
    [ "$value" == "$expected" ]
}

@test 'typechecker error: two variables with same identifier in a method' {
    value=$(./$EXE <<< $'tests/typechecker_test5.txt' | tail -1)
    expected='*** DuplicateVariableException ==> There are two params with the same name in method foo! ***'
    [ "$value" == "$expected" ]
}

@test 'typechecker error: type of a field does not exist' {
    value=$(./$EXE <<< $'tests/typechecker_test6.txt' | tail -1)
    expected='*** ClassNotFoundException ==> Type X does not exists! ***'
    [ "$value" == "$expected" ]
}

@test 'typechecker error: return type of a method does not exist' {
    value=$(./$EXE <<< $'tests/typechecker_test7.txt' | tail -1)
    expected='*** ClassNotFoundException ==> Type X not found! ***'
    [ "$value" == "$expected" ]
}

@test 'typechecker error: trying to redefine Object' {
    value=$(./$EXE <<< $'tests/typechecker_test8.txt' | tail -1)
    expected='*** DuplicateClassException ==> There are two classes with the same name: Object! ***'
    [ "$value" == "$expected" ]
}

@test 'typechecker error: return type does not match what declared' {
    value=$(./$EXE <<< $'tests/typechecker_test9.txt' | tail -1)
    expected='*** TypeException ==> Effective and declared return types are not compatible! ***'
    [ "$value" == "$expected" ]
}

@test 'typechecker error: two methods with the same name in the same class' {
    value=$(./$EXE <<< $'tests/typechecker_test10.txt' | tail -1)
    expected='*** DuplicateMethodException ==> There are two methods with the same name in class C! ***'
    [ "$value" == "$expected" ]
}

@test 'typechecker error: in class declaration extension of unknown class' {
    value=$(./$EXE <<< $'tests/typechecker_test11.txt' | tail -1)
    expected='*** ClassNotFoundException ==> Class X does not exist! ***'
    [ "$value" == "$expected" ]
}

@test 'typechecker error: variable not existent in method declaration' {
    value=$(./$EXE <<< $'tests/typechecker_test12.txt' | tail -1)
	expected='*** VariableNotFoundException ==> Variable y not found! ***'
    [ "$value" == "$expected" ]
}

@test 'typechecker error: overriding error, a parameter more in subclass method m' {
    value=$(./$EXE <<< $'tests/typechecker_test13.txt' | tail -1)
	expected='*** OverrideException ==> Methods with same name do not share same signature! ***'
    [ "$value" == "$expected" ]
}

@test 'typechecker error: overriding error, a parameter more in superclass method m' {
    value=$(./$EXE <<< $'tests/typechecker_test14.txt' | tail -1)
	expected='*** OverrideException ==> Methods with same name do not share same signature! ***'
    [ "$value" == "$expected" ]
}

@test 'typechecker error: overriding error, different return type' {
    value=$(./$EXE <<< $'tests/typechecker_test15.txt' | tail -1)
	expected='*** OverrideException ==> Methods with same name do not share same signature! ***'
    [ "$value" == "$expected" ]
}

##########################
### INTERPRETER ERRORS ###
##########################

@test 'runtime error: runtime exception of type ClassCastException' {
    value=$(./$EXE <<< $'tests/interp_test.txt\n(F)(Object)new G()' | tail -1)
    expected='*** ClassCastException ==> Cast not valid! ***'
    [ "$value" == "$expected" ]
}

@test 'runtime error: method not found exception' {
    value=$(./$EXE <<< $'tests/interp_test.txt\nnew F().foo()' | tail -1)
    expected='*** MethodNotFoundException ==> Method foo not found! ***'
    [ "$value" == "$expected" ]
}

@test 'runtime error: field not found exception' {
    value=$(./$EXE <<< $'tests/interp_test.txt\nnew F().f' | tail -1)
    expected='*** FieldNotFoundException ==> Field f not found! ***'
    [ "$value" == "$expected" ]
}

@test 'runtime error: wrong type of a parameter' {
    value=$(./$EXE <<< $'tests/interp_test.txt\nnew C(new B(new F(), new F())).changeB(new G())' | tail -1)
    expected='*** MismatchParamsException ==> Type of params does not match! ***'
    [ "$value" == "$expected" ]
}

@test 'runtime error: contructor with no parameter, pass a parameter' {
    value=$(./$EXE <<< $'tests/interp_test.txt\nnew F(new Object())' | tail -1)
    expected='*** MismatchParamsException ==> Params does not match! ***'
    [ "$value" == "$expected" ]    
}

@test 'runtime error: wrong number of parameters in contructor' {
    value=$(./$EXE <<< $'tests/interp_test.txt\nnew B(new Object())' | tail -1)
    expected='*** MismatchParamsException ==> Params does not match! ***'
    [ "$value" == "$expected" ]    
}

@test 'runtime error: creating an object of an unknown class' {
	value=$(./$EXE <<< $'tests/interp_test.txt\nnew H()' | tail -1)
	expected='*** ClassNotFoundException ==> Class H not found! ***'
    [ "$value" == "$expected" ]    
}

@test 'runtime error: overriding method with wrong number of parameters' {
	value=$(./$EXE <<< $'tests/interp_test3.txt\nnew CPair(new D(), new D()).first.m(new Object())' | tail -1)
	expected='*** MismatchParamsException ==> Params does not match! ***'
    [ "$value" == "$expected" ]    
}

##########################
### CORRECT EXECUTIONS ###
##########################

@test 'correct: sanity check, value = new F()' {
    value=$(./$EXE <<< $'tests/interp_test.txt\nnew B(new F(), new G()).first()\n0' | tail -5 | head -1)
    expected='new F([])'
    [ "$value" == "$expected" ]
}

@test 'correct: field access, value = new G()' {
    value=$(./$EXE <<< $'tests/interp_test.txt\nnew B(new F(), new G()).second()\n0' | tail -5 | head -1)
    expected='new G([])'
    [ "$value" == "$expected" ]
}

@test 'correct: method invocation, value = new B(new Object(), new Object())' {
    value=$(./$EXE <<< $'tests/interp_test.txt\nnew B(new F(), new G()).freshB(new Object(), new Object())\n0' | tail -5 | head -1)
    expected='new B([new Object([]),new Object([])])'
    [ "$value" == "$expected" ]
}

@test 'correct: object creation, value = new A(new F(), new F(), new G())' {
    value=$(./$EXE <<< $'tests/interp_test.txt\n(B)new A(new F(), new F(), new G())\n0' | tail -5 | head -1)
    expected='new A([new F([]),new F([]),new G([])])'
    [ "$value" == "$expected" ]
}

@test 'correct: field access, value = new G()' {
    value=$(./$EXE <<< $'tests/interp_test.txt\nnew D(new G()).x\n0' | tail -5 | head -1)
    expected='new G([])'
    [ "$value" == "$expected" ]
}

@test 'correct: invoke method of superclass, value = new F()' {
	value=$(./$EXE <<< $'tests/interp_test.txt\nnew A(new F(), new G(), new G()).first()\n0' | tail -5 | head -1)
    expected='new F([])'
    [ "$value" == "$expected" ]	
}

@test 'correct: access field of the superclass, value = new G()' {
	value=$(./$EXE <<< $'tests/interp_test.txt\nnew E(new G()).x\n0' | tail -5 | head -1)
    expected='new G([])'
    [ "$value" == "$expected" ]	
}

@test 'correct: quite complex program, value = new F()' {
    value=$(./$EXE <<< $'tests/interp_test.txt\n(F)new C(new B(new F(), new F())).changeB(new B(new F(), new F())).b.o1\n0' | tail -5 | head -1)
    expected='new F([])'
    [ "$value" == "$expected" ]
}

@test 'correct: importance of downcast, value = new B()' {
	value=$(./$EXE <<< $'tests/interp_test2.txt\n((Pair)new Pair(new Pair(new A(), new B()), new A()).first).second\n0' | tail -5 | head -1)
    expected='new B([])'
    [ "$value" == "$expected" ]	
}

@test 'correct: dynamic binding 1, value = new D()' {
	value=$(./$EXE <<< $'tests/interp_test3.txt\nnew CPair(new D(), new C()).first.m()\n0' | tail -5 | head -1)
	expected='new D([])'
    [ "$value" == "$expected" ]	
}

@test 'correct: dynamic binding 2, value = new C()' {
	value=$(./$EXE <<< $'tests/interp_test3.txt\nnew CPair(new D(), new C()).second.m()\n0' | tail -5 | head -1)
	expected='new C([])'
    [ "$value" == "$expected" ]	
}

#################
### MAIN TEST ###
#################

@test 'main error: input file not found' {
	value=$(./$EXE <<< $'X' | tail -3 | head -1)
	expected='ERROR ==> File X does not exist!'
    [ "$value" == "$expected" ]	
}
