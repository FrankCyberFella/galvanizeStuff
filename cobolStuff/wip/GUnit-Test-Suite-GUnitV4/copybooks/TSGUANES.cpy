       GU-Assert-String-Not-Equal.

      * Assert - Invoke Unit Test module
           add 1 to TS-Asserts-Performed.

      * Set GUnit Test Type
           Set GU-String-Test to True. 

      * Save Expected, Actual Test Values along with other data
      *    passed to GUnit
           Move GU-Test-Type to TR-Test-Type(TS-Asserts-Performed).
           Move TS-Test-Description 
             to TR-Test-Description(TS-Asserts-Performed)
             GU-Test-Description
           Move GU-Expected-Value-String
             to TR-Expected-Value-String(TS-Asserts-Performed).
           Move GU-Actual-Value-String
             to TR-Actual-Value-String(TS-Asserts-Performed).
           Move GU-Error-Msg to TR-Error-Msg(TS-Asserts-Performed).
           
           Call 'GU-AssertEquals-String' using GUnit-Test-Fields.

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
              move GU-Content-Length-Expected 
              to TR-Expected-String-Len-Used(TS-Asserts-Performed)
              move GU-Content-Length-Actual
              to TR-Actual-String-Len-Used(TS-Asserts-Performed)  
           end-if.     
           
       GU-Assert-String-Equal-Exit.
           Exit.
