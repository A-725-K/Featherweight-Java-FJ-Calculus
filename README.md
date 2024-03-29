# Interactive Interpreter for FJ Calculus
Implementation of an interactive interpreter for the calculus Featherweight Java.

## Info
This project was entirely developed under *Parrot OS* operating system, in *GNU-Emacs* text-editor, with the well-known software engineering technique *Test First*. The aim of this project is part of the exam *Principles and Paradigms of Programming Languages* that took place at the *University of Genoa*, during the second semester of the academic year 2018/2019 for the Master Degree in *Computer Science*. The code is written in **Haskell** with the only exception of the tests, since they are written in **Bash**.

## Getting started
First of all you have to clone or download this repository, with the command:
```
git clone https://github.com/A-725-K/Featherweight-Java-FJ-Calculus.git
```
The *Makefile* has been thought to simplify the life of the end user. So to compile the project it is sufficient to execute the following command:
```
make
```
The tests have been written exploiting a framework for unit testing of the *Bash* language which is called **Bats**. This choice is due to the fact that to load a class table as a context for the execution of your FJ program is quite a long work. It is simpler to wirte all the class definitions in a file and then load it into the interpreter. Moreover the command line of a GNU/Linux system offers some useful tools to check the results generated by the execution of the program. <br>
If you haven't already installed it on your system and for more information about Bash Automatic Testing System you can click <a href="https://github.com/sstephenson/bats">here</a> and check on the main page of the project. <br><br>
Finally, once you have compiled the project and installed *Bats*, you have two choices: <br><br>
**1. Run the tests, with the command:**
```
make tests
```
**2. Launch the interpreter, with the command:**
```
./FJ_Interpreter.exe
```
and then follow the instructions inside the program.

## How to use the *interactive interpreter*
<ul>
  <li>Starts the interpreter</i>
  <li>
    Give a file that contains the <i>class declarations</i> in input 
    <ul>
      <li>If the file does not exist, it will be notified</li>
      <li>The parser tries to create an <i>AST</i> starting from the content of the input file</li>
      <li>A typechecking operation of the class table takes place</li>
      <li>If an error arises during these two phases, the user receive a notification</li>
    </ul>
  </li>
  <li>
    Give an expression to the interpreter
    <ul>
      <li> Again the expression is parsed and then typechecked</li>
      <li> If it is statically correct than it is interpreted, otherwise an error is given in output</li>
    </ul>
  </li>
</ul>

## Brief description of files
<ul>
  <li><b><i>MainFJ.hs</i></b>: the main of the interactive interpreter</li>
  <li><b><i>MonadicParserFJ.hs</i></b>: all the parsers for the input</li>
  <li><b><i>TypeCheckerFJ.hs</i></b>: functions related to typechecking</li>
  <li><b><i>InterpreterFJ.hs</i></b>: interpreter ans substitution function</li>
  <li><b><i>DataTypesFJ.hs</i></b>: all the user defined data types used in the program</li>
  <li><b><i>UtilitiesFJ.hs</i></b>: a bunch of functions useful and some pre-loaded program for testing inside the interpreter</li>
  <li><b><i>unit_test.sh</i></b>: definition of all unit testing</li>
  <li><b><i>Makefile</i></b>: contains the real commands to compile the program and execute the tests</li>
</ul>

## Grammar
```haskell
p   ::= cd_1 ... cd_n
cd  ::= class C extends C' { fds mds }
fds ::= fd_1 ... fd_n
fd  ::= C f;
mds ::= md_1 ... md_n
md  ::= C_0 m(C_1 x_1, ..., C_n x_n) { return e; }
e   ::= x | e.f | e_0.m(e_1, ..., e_n) | new C(e_1, ..., e_n) | (C)e | ( e )
```

## Author

* **<i>Andrea Canepa</i>** - Computer Science, UNIGE - *Principles and Paradigms of Programming Languages a.y. 2018/2019*
