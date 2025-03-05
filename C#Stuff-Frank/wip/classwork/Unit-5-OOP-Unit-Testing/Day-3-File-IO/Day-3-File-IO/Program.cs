using GeneralPurposeFunctions;
namespace Day_3_File_IO
{
    internal class Program
    {
        private static CommonlyUsedFunctions _myFuncs = new CommonlyUsedFunctions();

        static void Main(string[] args)
        {
            _myFuncs.WriteSeparatorLine("Welcome to a File I/O Example");

            // Display the current Execution folder (folder the app is running from)
            // The Environment object gives you access to information about the current execution environment
            Console.WriteLine($"Current exection folder is: {Environment.CurrentDirectory}");


            // Display all the lines in the file called: numbers.txt in files folder/diriectory

            // Define a StreamReader for the file path containing the file
            // files path is 3-levels up from our execution (../ - go up one level)
            //StreamReader allen = new StreamReader("../../../files/numbers.txt");

            // Define a StreamWriter object to hold the output from the program in a file
            StreamWriter streamWriter = new StreamWriter("../../../silence.output");
            // streamWriter.AutoFlush = true; // Automatically flush buffer when writing

            // Define a variable to hold the sum of the numbers
            int sum = 0;
            // Define a running total outside the loop
            int runningTotal = 0;
            try
            {
                using (StreamReader allen = new StreamReader("../../../files/numbers.txt"))
                    // Loop until all the lines have been processed (not EndOfStream)
                    while (!allen.EndOfStream)  // .EndOfStream will indicate when file is empty (no more data)
                    { 
                     //    Get a line from the file
                        string line = allen.ReadLine();

                        // Display and sum the numbers
                         string[] theValues = line.Split(","); // Split line up into values

                         sum = 0;  // reset sum before we start adding all the numbers


                        for (int i = 0; i < theValues.Length; i++) // Loop through the values
                        {
                            string aValue = theValues[i].Trim();   // Trim any leading or trailing spaces from the value                                   
                            int intValue = int.Parse(aValue);     // Convert a value from string to int

                            //  int intValue = int.Parse(theValues[i].Trim()); // Alternate to two statements above

                            // Add the int value to a sum
                            sum += intValue;

                            // Add the sum to running total (Kendall mod)
                            runningTotal += intValue;

                            // Display value in the file we created
                            streamWriter.WriteLine($"Value #{i}: {aValue}");
                        } // End of for-loop
                    }  // End of while loop for reading the file
                } // end of try block
                catch (Exception ex)  // If any exception occurs in the try block
                {
                    Console.WriteLine(ex.ToString());  // write out the system message for the error
                }
                // After we have processed all the numbers..
               
                streamWriter.WriteLine($"Sum is: {sum}");
                streamWriter.WriteLine($"Running subtotal: {runningTotal}");

            //    Display the line
            // Console.WriteLine(line);


            // Close the file when done to free up memory used to support the file
            //allen.Close(); // the name allen is out scope outside the try
            streamWriter.Close();  // Also cuase any data in the buffer to written to the disk

            _myFuncs.PauseProgram();
            _myFuncs.WriteSeparatorLine("Thanks for visiting a File I/O Example");

        }
    }
}
