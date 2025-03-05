# Sample GUnit Test Suite (2025-02-04)
 
This was created to demonstrate use of a GUnit Test suite using GUnit Dynamically called modules.

Contact Frank Fella (Slack or Frank.Fella@Galvanize.com) with questions,
comments, assistance or feedback.    

### Technical Requirements

This project may be run using *gnuCOBOL* or files installed in appropriate mainframe COBOL compiler and link libraries and run in the mainframe environment.

To install gnuCOBOL:

- Mac: `brew install gnucobol`
- Windows: [gnuCOBOL Download Site](https://gnucobol.sourceforge.io/)

### Files included in this release:

### In the copybooks folder:

* **APPCalc.cbl** - Program with code to be tested by test suite.  It is included in the test suite via a *copy* statement following the end of the test suite code. This must be installed in a library accessible to the compile and link that creates *TSEXAMPL.cbl* Test Suite example program; Required by GUnit Test Suite example processing. 

* **GUINIT.cpy** - Copybook containing code for interacting with GUnit initialization processing; This must be installed in a library accessible to the compile and link that creates a GUnit Test Suite program; Required by GUnit Test Suite processing. 

* **GUNITFLD.cpy** - Copybook containing required fields for interacting with GUnit; This must be installed in a library accessible to the compile and link that creates a GUnit Test Suite program; Required by GUnit Test Suite processing. 

* **TSFIELDS.cpy** - Copybook containing GUnit Test Suite supporting code used by any GUnit Test Suite program such as *TSEXAMPL.cbl*; This must be installed in a library accessible to the compile and link that creates a GUnit Test Suite program; Required by GUnit Test Suite processing.  

* **TSGUAEQN.cpy** - Containing GUnit Test Suite supporting code to call the GUnit *Assert-Equal-Numeric* process; This must be installed in a library accessible to the compile and link that creates a GUnit Test Suite program; Required by GUnit Test Suite processing. 

* **TSGUAEQS.cpy** - Containing GUnit Test Suite supporting code to call the GUnit *Assert-Equal-String* process; This must be installed in a library accessible to the compile and link that creates a GUnit Test Suite program; Required by GUnit Test Suite processing. 

* **TSGUANEN.cpy** - Containing GUnit Test Suite supporting code to call the GUnit *Assert-Not-Equal-Numeric* process; This must be installed in a library accessible to the compile and link that creates a GUnit Test Suite program; Required by GUnit Test Suite processing. 

* **TSGUANES.cpy** - Containing GUnit Test Suite supporting code to call the GUnit *Assert-Not-Equal-String* process; This must be installed in a library accessible to the compile and link that creates a GUnit Test Suite program; Required by GUnit Test Suite processing. 

* **TSSHOW.cpy** - Containing GUnit Test Suite supporting code display the results of tests performed in the Test Suite; This must be installed in a library accessible to the compile and link that creates a GUnit Test Suite program; Required by GUnit Test Suite processing. 

### In the programs folder:

* **GUAxxxxx.dylib** - *FOR GNUCOBOL USE ONLY* - Compiled versions of the GUnit modules that will be dynamically loaded when called by a GUnit Test Suite is compiled and run using the GnuCOBOL `cobc -x` command; Must be in the same folder as the GUnit Test Suite program being run.

* **TSEXAMPLE.cbl** - Example of a GUnit Test Suite program;  Demonstrates how one would structure and create a viable GUnit Test Suite.  See instructions below on how to run with gnuCOBOL.

* **TSTEMPLT.cbl** - Template for a GUnit Test Suite with the general structure and supporting copy books;  Provided so one may copy as a starting point for a GUnit Test Suite.


### To run this project using gnuCOBOL:

1. Navigate to project folder with the .cbl files
2. Issue the command: `cobc -x -I 'relative-path-to-copybook-folder' TSEXAMPL.cbl `
<br/>
   ***Be sure to replace `relative-path-to-copybook-folder` in the above commmand with the relative path to your copybook folder***
3. If no errors, to run the code: `.TSEXAMPL`

If you run into issues, contact Frank Fella (Slack or Frank.Fella@Galvanize.com)    