      * Mock Randomizer to enable Unit Testing
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Mock-Randomizer.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  random-int pic 999.
       01  random-percent pic 999.
       
       LINKAGE SECTION.
       01  output-int pic 999.
       01  input-int pic 999.
       01  ceil pic 999.
       
       PROCEDURE DIVISION.

       entry 'set-random-int' using input-int.
           move input-int to random-int.
           goback.
            
       entry 'get-random-int' using output-int, ceil.
           move random-int to output-int.
           goback.

       entry 'set-random-percent' using input-int.
           move input-int to random-int.
           goback.
            
       entry 'get-random-percent' using output-int.
           move random-int to output-int.
           goback.
           

       end program Mock-Randomizer.
