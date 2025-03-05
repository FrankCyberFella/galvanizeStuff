using System;
using System.Collections.Generic;
using System.IO;

namespace FileSplitter
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.Write("Where is the input file (please include the file path to the file)? ");
            string inputFilePath = Console.ReadLine();

            Console.Write("How many lines of text (max) should there be in the split files? ");
            string userInputMaxLineCount = Console.ReadLine();
            int maxLineCount = int.Parse(userInputMaxLineCount);

            FileInfo inputFile = new FileInfo(inputFilePath);
            Queue<string> linesInFile = new Queue<string>();

            using (StreamReader sr = inputFile.OpenText())
            {
                while (!sr.EndOfStream)
                {
                    linesInFile.Enqueue(sr.ReadLine());
                }
            }

            Console.WriteLine($"The input file has {linesInFile.Count} lines of text.{Environment.NewLine}");
            Console.WriteLine($"Each file that is created should have a sequential number assigned to it.{Environment.NewLine}");

            int numberOfFilesToCreate = (int)Math.Ceiling((double)linesInFile.Count / maxLineCount);

            Console.WriteLine($"For a {linesInFile.Count} line input file \"{inputFile.Name}\" this will produce {numberOfFilesToCreate} output files. {Environment.NewLine}");
            Console.WriteLine($"**GENERATING OUTPUT**{Environment.NewLine}");

            for (int fileNumber = 1; fileNumber <= numberOfFilesToCreate; fileNumber++)
            {
                string outputFileName = $"{Path.GetFileNameWithoutExtension(inputFile.Name)}-{fileNumber}{inputFile.Extension}";
                string outputFile = Path.Combine(inputFile.DirectoryName, outputFileName);
                Console.WriteLine($"Generating {outputFileName}");

                using (StreamWriter sw = new StreamWriter(outputFile))
                {
                    for (int currentLineNumber = 0; linesInFile.Count > 0 && currentLineNumber < maxLineCount; currentLineNumber++)
                    {
                        sw.WriteLine(linesInFile.Dequeue());
                    }
                }
            }

            Console.WriteLine();
        }
    }
}
