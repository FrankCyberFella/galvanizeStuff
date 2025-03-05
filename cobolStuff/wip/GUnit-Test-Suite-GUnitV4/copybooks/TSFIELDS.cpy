        01 TS-Test-Number-Chars pic X(255).
        01 TS-Test-Description  pic X(255).
        01 TS-Test-Number-Out   pic zz9.
        01 TS-Separator-Line         pic x(80) value all '-'.
        01 TS-Test-Summary-String.
           05 TS-Test-Summary-String-Table occurs 256 Times Pic X.
        01 TS-Test-Result-Table.
           05 TS-Test-Result  occurs 500 times  
                           indexed by Test-Result-Index.

              10 TR-Test-Type          Pic x(8).
                 88  TR-Unknown-Type      value 'Unknown'.
                 88  TR-Numeric-Test      value 'Numeric'.
                 88  TR-String-Test       value 'String'.
                 88  TR-Table-Test        value 'Table'. 

              10 TR-Test-Description   pic x(255).                   
              10 TR-Pass-Fail          pic x.
                 88 TR-Test-Passed     value 'Y'.
                 88 TR-Test-Failed     value 'N'.                   
              10 TR-Result             comp-2.
              10 TR-Error-Msg          pic X(255).
              10 TR-Test-Length        pic s9(4) comp. 

              10 TR-Expected-Value-Numeric comp-2.
              10 TR-Actual-Value-Numeric   comp-2.

              10 TR-Expected-Value-String Pic x(32767).
              10 TR-Actual-Value-String   Pic x(32767).

              10 TR-Expected-String-Len-Used    pic s9(9) comp.
              10 TR-Actual-String-Len-Used      pic s9(9) comp.

              10 TR-Expected-Table.
                 15 TR-Expected-elements occurs 10 times pic x(10).

              10 TR-Actual-Table.
                 15 TR-Actual-Elements occurs 10 times pic x(10).   
       
      * Number-Tests-Performed is also used as subscript for Test Results        
        01 TS-Asserts-Performed  pic s9(3) comp value 0. 
        01 TS-Asserts-Passed     Pic s9(3) comp-3.
        01 TS-Asserts-Failed     Pic s9(3) comp-3.   

        01 TS-Asserts-Performed-Out  Pic ZZ9. 
        01 TS-Asserts-Passed-Out     Pic ZZ9.
        01 TS-Asserts-Failed-Out     Pic ZZ9.  
        
        01 TS-String-Length-Expected Pic ++++9.
        01 TS-String-Length-Actual   Pic ++++9. 
      
