using GeneralPurposeFunctions;
using System.Security.Cryptography.X509Certificates;

namespace Day_3_Logging_Example
{
    internal class Program
    {
        // By defining this outside any method, it's available to all methods on the class
        // static is required because the methdos will be used in side a static method (Main())
        static CommonlyUsedFunctions myFuncs = new CommonlyUsedFunctions();

        static string inputFilePath = "../../../data/IMDB-Top-25-SciFi-Movies.txt";

        // To avoid trying to define the logFile more than once....
        // Make the reference to the file static and outside any method...
        //      1. Defines one reference
        //      2. Shared by all methods

        static StreamWriter logFile = null;   // Define reference and set to null

        static void Main(string[] args)
        {
            // Log the program start
            LogEvent("Program started");

            // Read and store all the data in the IMDB-Top-SciFi-Movies file

            // Define a StreamReader for  the file
            StreamReader fileReader = new StreamReader(inputFilePath);

            // Define a List to hold each line from the file
            List<string> sciFiMovies = new List<string>();

            // Lets be sure we can read all the data successfully
            while (!fileReader.EndOfStream)  // Loop as long as its not the end of stream
            {
                string line = fileReader.ReadLine(); // Get a line from the file   
                sciFiMovies.Add(line);               // Add the line to our list
            }

            // Loop until the user indicates they are done by typing 'N' or "No" when asked
            while (myFuncs.moreInput())  // moreInput() returns true of user says they have more input
            {
                // Ask the user which movie they want from the list by partial word search
                // Prompt the user to enter a word to search for
                Console.WriteLine("Please a word in the title of the movie you're looking for...");
                // define a place to hold user's choice
                string userInput = Console.ReadLine(); // Get a word from the user

                LogEvent($"User requested {userInput}");

                // Check to see if any movies in the list contain words
                // Loop through the list of movies to see if the title contains the word
                foreach (string aLine in sciFiMovies)
                {
                    if (aLine.Contains(userInput))
                    {
                        LogEvent($"Found title {aLine}");
                        Console.WriteLine(aLine);
                    }
                } // ends the foreach
            } // ends the outer while

                fileReader.Close(); // Release memory used by supporting data structures

            LogEvent("Program ended");
            closeLogFile();

            myFuncs.PauseProgram();
            } // Ends the Main() method

        // Helper methods for Main()

        // Log program events including user interactions to a disk file
        public static void LogEvent(string messageToLog)
        {         
        if (logFile == null)  // If logFile is not asssigned an object...
            {
                // Define a disk file to hold the log and append any new lines (true option)
                logFile = new StreamWriter("../../../program.log", true);
            }
            // Add a timestamp to the log message
            // Get the current timestapp as a sting
            DateTime currentTimestamp = DateTime.Now; // Get current time stamp
            string formattedTime = currentTimestamp.ToString("yyyy-MM-dd-HH:mm:ss");

            logFile.WriteLine($"{formattedTime} - {messageToLog}");
            logFile.Flush();
        }
        static void closeLogFile()
        {
            logFile.Close();
        }

    } // Ends the class with Main()
    
  

    } // Ends the namespace
