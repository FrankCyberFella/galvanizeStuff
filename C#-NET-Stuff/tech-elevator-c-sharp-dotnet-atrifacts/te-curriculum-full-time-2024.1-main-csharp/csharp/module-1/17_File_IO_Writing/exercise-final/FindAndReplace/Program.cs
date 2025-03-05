using System;
using System.IO;

namespace FindAndReplace
{
    public class Program
    {
		public static void Main(string[] args)
		{
			// Get the search word
			Console.WriteLine("What is the search word?");
			string searchWord = Console.ReadLine();

			// Get the replacement word
			Console.WriteLine("What is the replacement word?");
			string replacementWord = Console.ReadLine();

			// Get the location of the source file
			Console.WriteLine("What is the source file?");
			string sourcePath = Console.ReadLine();

			// Get the location of the destination file
			Console.WriteLine("What is the destination file?");
			string destinationPath = Console.ReadLine();

			// Replace word
			try
			{
				using (StreamReader reader = new StreamReader(sourcePath))
				using (StreamWriter writer = new StreamWriter(destinationPath))
				{
					while (!reader.EndOfStream)
					{
						string line = reader.ReadLine();
						writer.WriteLine(line.Replace(searchWord, replacementWord));
					}
				}
			}
			catch (FileNotFoundException e)
			{
				Console.WriteLine(e.Message);
			}
			catch (IOException e)
            {
				Console.WriteLine(e.Message);
            }
		}
    }
}
