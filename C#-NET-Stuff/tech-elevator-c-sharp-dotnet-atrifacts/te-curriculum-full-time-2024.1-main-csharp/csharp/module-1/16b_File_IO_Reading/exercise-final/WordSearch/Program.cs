using System;
using System.IO;

namespace WordSearch
{
    public class Program
    {
        public static void Main(string[] args)
        {
            Console.WriteLine("What is the fully qualified name of the file that should be searched?");
            string inputFileName = Console.ReadLine();

            Console.WriteLine("What is the search word you are looking for?");
            string searchWord = Console.ReadLine();

            Console.WriteLine("Should the search be case sensitive? (Y\\N)");
            bool caseSensitive = (Console.ReadLine().ToLower() == "y");

            try
            {
                using (StreamReader sr = new StreamReader(inputFileName))
                {
                    int i = 1;
                    while (!sr.EndOfStream)
                    {
                        string line = sr.ReadLine();
                        bool lineContainsWord = (!caseSensitive && line.Contains(searchWord, StringComparison.CurrentCultureIgnoreCase))
                                                    || line.Contains(searchWord);

                        if (lineContainsWord)
                        {
                            Console.WriteLine($"{i}) {line}");
                        }

                        i++;
                    }
                }
            }
            catch (IOException e)
            {
                Console.WriteLine(e.Message);
            }
        }
    }
}
