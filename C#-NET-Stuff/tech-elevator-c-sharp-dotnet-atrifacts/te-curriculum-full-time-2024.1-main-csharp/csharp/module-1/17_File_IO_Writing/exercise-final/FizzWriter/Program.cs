using System;
using System.IO;

namespace FizzWriter
{
    public class Program
    {
        public static void Main(string[] args)
        {
            // Get the location of the destination file
            Console.WriteLine("What is the destination file?");
            string destinationPath = Console.ReadLine();

            try
            {
                using (StreamWriter sw = new StreamWriter(destinationPath))
                {
                    for(int i = 1; i <= 300; i++)
                    {
                        if (DivisibleBy3(i) && DivisibleBy5(i))
                        {
                            sw.WriteLine("FizzBuzz");
                        }
                        else if (DivisibleBy5(i))
                        {
                            sw.WriteLine("Buzz");
                        }
                        else if (DivisibleBy3(i))
                        {
                            sw.WriteLine("Fizz");
                        }
                        else
                        {
                            sw.WriteLine(i);
                        }
                    }
                }
            }
            catch (IOException e)
            {
                Console.WriteLine(e.Message);
            }

            Console.WriteLine($"{destinationPath} has been created.");
        }

        private static bool DivisibleBy3(int i)
        {
            return (i % 3 == 0);
        }

        private static bool DivisibleBy5(int i)
        {
            return (i % 5 == 0);
        }

    }
}
