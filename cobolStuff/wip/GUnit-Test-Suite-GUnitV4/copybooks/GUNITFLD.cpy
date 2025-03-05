       01  GUnit-Test-Fields.
      
      *****************************************************************
      * Result of Test Passed Back to Caller
      ***************************************************************** 
           05  GU-Test-Result          pic x(5).
               88  GU-Test-Passed      value 'True'.
               88  GU-Test-Failed      value 'False'.
      *****************************************************************
      * Switch to by caller to indicate if they want values displayed 
      ***************************************************************** 
           05  GU-Show-Values-Switch      pic x(5).
               88  GU-Show-Values         value 'Yes'.
               88  GU-Do-Not-Show-Values  value 'No'.

      ******************************************************************
      * Switch set to indicate type of test 
      ******************************************************************      
           05  GU-Test-Type                         pic x(8).
               88  GU-Unknown-Type      value 'Unknown'.
               88  GU-Numeric-Test      value 'Numeric'.
               88  GU-String-Test       value 'String'.
               88  GU-Table-Test        value 'Table'.         

      *****************************************************************
      * Test Description Passed to GUnit
      ***************************************************************** 
           05  GU-Test-Description        pic x(255).     
      
      *****************************************************************
      * Actual Value Of Data Passed To Be Used In The Numeric Tests
      *****************************************************************            
           05 GU-Actual-Value-Numeric         comp-2.

      *****************************************************************
      * Expected Value Of Data Passed To Be Used In The Numeric Tests
      *****************************************************************            
           05 GU-Expected-Value-Numeric        comp-2.

      *****************************************************************
      * Actual and Expected Value String 
      * Data Passed To Be Used In The Tests
      ***************************************************************** 
           05 GU-Actual-Value-String      pic x(32767).
           05 GU-Expected-Value-String    pic x(32767).

      *****************************************************************
      * Actual and Expected Value Table
      * Data Passed To Be Used In The Tests
      ***************************************************************** 
           05 GU-Actual-Table.
              10 GU-Actual-Elements occurs 10 times     pic x(10).
           05 GU-Expected-Table.
              10 GU-Expected-Elements occurs 10 times   pic x(10). 

      *****************************************************************
      * Fields used in table processing asserts 
      *****************************************************************      

      *****************************************************************
      * Calculated Length of Content Passed Without Trailing Spaces 
      ***************************************************************** 
           05  GU-Content-Length-Expected          pic s9(9)  comp.
           05  GU-Content-Length-Actual            pic s9(9)  comp.
      
      ******************************************************************
      * Switch set to indicate of expected and actual values same length 
      ******************************************************************      
           05  GU-Content-Length-Match-Switch       pic x(3).
               88  GU-Content-Lengths-Match         value 'Yes'.
               88  GU-Content-Lengths-Do-Not-Match  value 'No'.  

           05 GU-Error-Msg     pic x(255).  

      ******************************************************************
      * Fields to use when using the dyamically called GUNit modules 
      ******************************************************************  
           05 GU-InitializeDefaultValues      Pic x(8) value 'GUINIT'.
           05 GU-AssertEquals-Numeric         Pic x(8) value 'GUAEQNUM'.
           05 GU-AssertNotEquals-Numeric      pic x(8) value 'GUANENUM'.
           05 GU-AssertEquals-String          pic x(8) value 'GUAEQSTR'.
           05 GU-AssertNotEquals-String       pic x(8) value 'GUANESTR'.           

      ******************************************************************
      * The following fields are currently unused by GUnit
      ******************************************************************
      ******************************************************************
      * Switch set to indicate if content length should be ignored
      *        and Test-Length set when called should be used 
      ******************************************************************      
           05  GU-Should-Use-Test-Length            pic x(3).
               88  GU-Use-Test-Length               value 'Yes'.
               88  GU-Use-Content-Length            value 'No'.            
      
           05 GU-Test-Length              pic s9(9) comp.  
      ******************************************************************
      * End of Currently Unused Fields
      ****************************************************************** 
