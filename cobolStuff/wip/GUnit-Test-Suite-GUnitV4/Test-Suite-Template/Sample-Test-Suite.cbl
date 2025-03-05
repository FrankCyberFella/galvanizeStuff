       identification division.
       program-id. TSTemplt.
       
       environment division.
       configuration section.
       
       data division.
       working-storage section.

         01 WS-Test-Number-Chars pic X(255).

         01 WS-My-Test-Result.
              05 WS-My-Result      comp-2.
              05 WS-My-Error-msg   pic X(255).

         01 WS-Expected-Table.
            05 WS-Expected-Elements occurs 5 times pic s9(9) comp
                                    value 1.

         01 WS-Actual-Table.
            05 WS-Actual-Elements occurs 5 times pic s9(9)   comp
                                    value 1.     

      ****************************************************************
      * Copybook with Fields required/used by Test Suite Template
      * Fields will be set by GUnit and used however the programmer
      * deems necessary for thier test processing 
      ****************************************************************  
         copy 'TSFields'.
      ****************************************************************
      * Copybook with Fields required/used by GUnit
      * Fields will be set by the caller and returned by GUnit 
      ****************************************************************
       copy 'GUnitFld'.    
       
       procedure division.

           Perform Run-Tests.       *> Perform all tests in test suite
           Perform TS-Show-Results. *> Display results

           goback.
       
       Run-Tests.

           Perform Test-1.
           Perform Test-2.
           Perform Test-3.
           Perform Test-4.
           Perform Test-5.
           Perform Test-6.
           Perform Test-7.
           Perform Test-8.

       Run-Tests-Exit.
           exit.


       Test-1.

      * Arrange - Setup data for test

      * Initialize GUnit-Test-Fields default values.
           perform GU-Initialize-Default-Values.

      * Set up GUnit-Test-Fields for test
           move "Test-1 - Verify Values Are Equal" *> Set Test Desc.
             to TS-Test-Description.
           
           move "-10,-11" to WS-Test-Number-Chars.
           move -1 to GU-Expected-Value-Numeric.
           
           Move 'Values are not equal' to GU-Error-Msg. *> Set err msg
       
      * Act - Perform process to obtain actual value
      
           call 'add' using WS-Test-Number-Chars, WS-My-Test-Result.
           move WS-My-Result    to GU-Actual-Value-Numeric.
           move WS-My-Error-msg to GU-Actual-Value-String.

      * Call GUnit and save result.     
           
           Perform GU-Assert-Numeric-Equal.

       Test-1-Exit.
           Exit.

       Test-2.

      * Arrange - Setup data for test

      * Initialize GUnit-Test-Fields default values.
           perform GU-Initialize-Default-Values.

      * Set up GUnit-Test-Fields for test
           move "Test-2 - Verify Values Are Equal" *> Set Test Desc.
             to TS-Test-Description.
           
           move "-10,-11" to WS-Test-Number-Chars.
           move 1 to GU-Expected-Value-Numeric. *> Purposely wrong
           
           Move 'Values are not equal' to GU-Error-Msg. *> Set err msg
       
      * Act - Perform process to obtain actual value
      
           call 'add' using WS-Test-Number-Chars, WS-My-Test-Result.
           move WS-My-Result    to GU-Actual-Value-Numeric.
           move WS-My-Error-msg to GU-Actual-Value-String.

      * Call GUnit and save result.     
           
           Perform GU-Assert-Numeric-Equal.

       Test-2-Exit.
           Exit.

       Test-3.

      * Arrange - Setup data for test

      * Initialize GUnit-Test-Fields default values.
           perform GU-Initialize-Default-Values.

      * Set up GUnit-Test-Fields for test
           move "Test-3 - Verify Values Are Not Equal" *> Set Test Desc.
             to TS-Test-Description.
           
           move "-10,-11" to WS-Test-Number-Chars.
           move -1 to GU-Expected-Value-Numeric.
           
           Move 'Values are not equal' to GU-Error-Msg. *> Set err msg
       
      * Act - Perform process to obtain actual value
      
           call 'add' using WS-Test-Number-Chars, WS-My-Test-Result.
           move WS-My-Result    to GU-Actual-Value-Numeric.
           move WS-My-Error-msg to GU-Actual-Value-String.

      * Call GUnit and save result.               
           Perform GU-Assert-Numeric-Not-Equal.

       Test-3-Exit.
           Exit.

       Test-4.

      * Arrange - Setup data for test

      * Initialize GUnit-Test-Fields default values.
           perform GU-Initialize-Default-Values.

      * Set up GUnit-Test-Fields for test
           move "Test-4 - Verify Values Are Not Equal" *> Set Test Desc.
             to TS-Test-Description.
           
           move "-10,-11" to WS-Test-Number-Chars.
           move 1 to GU-Expected-Value-Numeric. *> Purposely wrong
           
           Move 'Values are not equal' to GU-Error-Msg. *> Set err msg
       
      * Act - Perform process to obtain actual value
      
           call 'add' using WS-Test-Number-Chars, WS-My-Test-Result.
           move WS-My-Result    to GU-Actual-Value-Numeric.
           move WS-My-Error-msg to GU-Actual-Value-String.

      * Call GUnit and save result.               
           Perform GU-Assert-Numeric-Not-Equal.

       Test-4-Exit.
           Exit. 

       Test-5.

      * Arrange - Setup data for test

      * Initialize GUnit-Test-Fields default values.
           perform GU-Initialize-Default-Values.

      * Set up GUnit-Test-Fields for test
           move "Test-5 - Verify Strings Are Equal" *> Set Test Desc.
             to TS-Test-Description.
           
           move 'Al' to GU-Expected-Value-String. *> Set expected value
           
           Move 'Values are not equal' to GU-Error-Msg. *> Set err msg
       
      * Act - Perform process to obtain actual value     
           move 'Al' to GU-Actual-Value-String.

      * Call GUnit and save result. 
             Perform GU-Assert-String-Equal.

       Test-5-Exit.
           Exit. 

       Test-6.

      * Arrange - Setup data for test

      * Initialize GUnit-Test-Fields default values.
           perform GU-Initialize-Default-Values.

      * Set up GUnit-Test-Fields for test
           move "Test-6 - Verify Strings Are Equal" *> Set Test Desc.
             to TS-Test-Description.
           
           move 'Al' to GU-Expected-Value-String. *> Set expected value
           
           Move 'Values are not equal' to GU-Error-Msg. *> Set err msg
       
      * Act - Perform process to obtain actual value     
           move 'Pete' to GU-Actual-Value-String.

      * Call GUnit and save result.               
           Perform GU-Assert-String-Equal.

       Test-6-Exit.
           Exit. 

       Test-7.

      * Arrange - Setup data for test

      * Initialize GUnit-Test-Fields default values.
           perform GU-Initialize-Default-Values.

      * Set up GUnit-Test-Fields for test

           Set GU-Show-Values to True. 

           move "Test-7 - Verify Tables Are Equal" *> Set Test Desc.
             to TS-Test-Description.
           
           *> Set err msg
           Move 'Table Values are not equal' to GU-Error-Msg. 

           Move WS-Expected-Table to GU-Expected-Table.
       
      * Act - Perform process to obtain actual value 
           Move WS-Actual-Table to GU-Actual-Table.    

      * Call GUnit and save result.               
           Perform GU-Assert-Table-Equal.

       Test-7-Exit.
           Exit. 

       Test-8.

      * Arrange - Setup data for test

      * Initialize GUnit-Test-Fields default values.
           perform GU-Initialize-Default-Values.

      * Set up GUnit-Test-Fields for test

           Set GU-Show-Values to True. 

           move "Test-8 - Verify Tables Are Equal" *> Set Test Desc.
             to TS-Test-Description.

           *> Set err msg
           Move 'Table Values are not equal' to GU-Error-Msg. 

           Move WS-Expected-Table to GU-Expected-Table.
       
      * Act - Perform process to obtain actual value 
           move 99 to WS-Actual-Elements(1).
           Move WS-Actual-Table to GU-Actual-Table.   
           
      * Call GUnit and save result.               
           Perform GU-Assert-Table-Equal.

       Test-8-Exit.
           Exit. 


      ******************************************************************
      * COPY Test Suite Features Desired/Used before end of program
      ******************************************************************
       copy 'TSGUAEQN'. *> Assert Equals Numeric
       copy 'TSGUANEN'. *> Assert NotEquals Numeric
       copy 'TSGUAEQS'. *> Assert Equals String
       copy 'TSGUANES'. *> Assert Not Equals String
       copy 'TSGUAEQT'. *> Assert Equals Table
       copy 'TSSHOW'.   *> Test Suit Display Results
       copy 'GUINIT'.   *> GUnit Initialize to Defaults

       end program TSTemplt.

      ******************************************************************
      * COPY program code to be tested AFTER end of Test Suite Program
      ******************************************************************
       copy './appCalc.cbl'.             *> Program to be tested

      ******************************************************************
      * COPY program code to be tested AFTER end of Test Suite Program
      ******************************************************************       
       copy 'GUnitV4'.   *> GUnit Code
