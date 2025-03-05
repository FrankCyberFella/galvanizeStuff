       GU-Assert-Table-Equal.

      * Assert - Invoke Unit Test module
           add 1 to TS-Asserts-Performed.

      * Set GUnit Test Type
           Set GU-Table-Test to True. 

      * Save Expected, Actual Test Values along with other data
      *    passed to GUnit
           Move GU-Test-Type to TR-Test-Type(TS-Asserts-Performed).

      * Save Expected and Actual Test Values along with Error Message
           Move TS-Test-Description 
             to TR-Test-Description(TS-Asserts-Performed)
                GU-Test-Description.
           Move GU-Error-Msg to TR-Error-Msg(TS-Asserts-Performed).
           Move GU-Actual-Table   
              to TR-Actual-Table(TS-Asserts-Performed).
           Move GU-Expected-Table 
              to TR-Expected-Table(TS-Asserts-Performed).
           
           Call 'GU-AssertEquals-Table' using GUnit-Test-Fields.

           if GU-Test-Passed
              add 1 to TS-Asserts-Passed
              Set TR-Test-Passed(TS-Asserts-Performed) to True
           else
              add 1 to TS-Asserts-Failed
              Set TR-Test-Failed(TS-Asserts-Performed) to True
              move GU-Actual-Value-Numeric
                to TR-Actual-Value-Numeric(TS-Asserts-Performed)
              move GU-Error-Msg 
                to TR-Error-Msg(TS-Asserts-Performed)
           end-if.     
           
       GU-AssertEquals-Table-Exit.
           Exit.
           