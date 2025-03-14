       Identification Division.
       Program-id. TSTEMPLT.  *> change this if you'd like
      
       Data Division.
       Working-Storage Section.
      ****************************************************************
      * Define any data needed by the program you are testing
      * that will be sent as parameters here
      ****************************************************************
   
      ****************************************************************
      * Copybook with Fields required/used by Test Suite Template
      * Fields will be set by GUnit and used however the programmer
      * deems necessary for their test processing 
      ****************************************************************  
         copy 'TSFields'.
      ****************************************************************
      * Copybook with Fields required/used by GUnit
      * Fields will be set by the caller and returned by GUnit 
      ****************************************************************
       copy 'GUnitFld'.    
       
       Procedure Division.

           Perform Run-Tests.       *> Perform all tests in test suite
           Perform TS-Show-Results. *> Display results of all tests

           Goback.
       
       Run-Tests. *> Perform each test paragraph here

           Perform Test-1.

       Run-Tests-Exit.
           exit.


       Test-1. *> Each test should be placed in their own paragraph

      * Arrange - Setup data for test
      * Initialize GUnit-Test-Fields default values.
           perform GU-Initialize-Default-Values.

      * Set up GUnit-Test-Fields for test
           move "Decription of Test" *> Set Test Desc.
             to TS-Test-Description.

      * Set up parameters/Values need to be sent to program being tested
           move "Something" to WS-Parameter-To-Send
           Move 'Error Message' to GU-Error-Msg. *> Set err msg
       
      * Act - Perform process to obtain actual value     
           call 'TheProgram' using WhatEver-it-Needs
                                 , WS-Group-Item-To-Hold-Result.

      * Move the actual result to be tested to associated GUnit field
           move WS-the-Result  to GU-Actual-Value-Numeric.

      * Call GUnit and save result.     
      * Choose the one you want executed

      *    Perform GU-Assert-Numeric-Equal.
      *    Perform GU-Assert-Numeric-Not-Equal.
      *    Perform GU-Assert-String-Equal.
      *    Perform GU-Assert-String-Not-Equal.

       Test-1-Exit.
           Exit.

      ******************************************************************
      * COPY Test Suite Features Desired/Used before end of program
      ******************************************************************
       copy 'TSGUAEQN'. *> Assert Equals Numeric
       copy 'TSGUANEN'. *> Assert NotEquals Numeric
       copy 'TSGUAEQS'. *> Assert Equals String
       copy 'TSGUANES'. *> Assert NotEquals String
       copy 'TSSHOW'.   *> Test Suit Display Results
       copy 'GUINIT'.   *> GUnit Initialize to Defaults

       end program TSTEMPLT.  *> Name must match Program-Id.

      ******************************************************************
      * COPY program code to be tested AFTER end of Test Suite Program
      ******************************************************************
       copy './program-name.cbl'.     *> Program to be tested

