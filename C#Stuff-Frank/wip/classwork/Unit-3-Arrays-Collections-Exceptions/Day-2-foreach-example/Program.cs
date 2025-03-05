using System;

namespace Day_2_foreach_example
{
    internal class Program
    {
        static void Main(string[] args)
        {
            // Define array of doubles
            double[] doubleList = { 2000, 1.5, 6.7, 13.3 };


            Console.WriteLine("\nDisplay all elements using for-loop");
            // Display all the elements in the array using a for-loop
            for (int i = 0; i < doubleList.Length; i++)
            {
                Console.WriteLine("Element #" + i + ": " + doubleList[i]);
            }

            Console.WriteLine("\nDisplay 2nd and 3rd elements using for-loop");
            // Display the 2nd and 3rd elements in the array using a for-loop
            // Start the loop control variable at 1 (index of second element
            // Make the condition stop at index 2 (i < 3)
            // Index 2 is the third element
            for (int i = 1; i < 3; i++)
            {
                Console.WriteLine("Element #" + i + ": " + doubleList[i]);
            }
            
            Console.WriteLine("\nDisplay all elements using foreach-loop");
            // Display all the elements in the array using a foreach loop
            //
            // The foreach will go through the entire array from start to finish
            //             one element at a time assigning the current element
            //             to the variable specified
            //
            // syntax: foreach (datatype variable in array-name)
            //
            //         use the variable inside the loop to reference the current element
            //
            // a foreach has no way of know which element (index) is the current element
            //
            // a foreach cannot start anywhere but the first element
            //              nor stop before the last element

            foreach (double anElement in doubleList)
            {
                Console.WriteLine("Element: " + anElement);
            }

            // if you need to know the element being processed in a foreach
            // YOU must keep track of it (foreach does not)


            Console.WriteLine("\nDisplay all elements using foreach-loop with index");
            int counter = 0;
            foreach (double anElement in doubleList)
            {
                Console.WriteLine("Element #" + counter +": " + anElement);
                counter++;  // increment the element number
            }


        } // End of Main
    } // End of program class
} // End of namespace
