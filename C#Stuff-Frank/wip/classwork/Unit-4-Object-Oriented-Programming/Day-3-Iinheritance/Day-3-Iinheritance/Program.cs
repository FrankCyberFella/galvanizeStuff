using GeneralPurposeFunctions;
using System;
using Day_3_Iinheritance;


namespace Day_3_Inheritance
{
    internal class Program
    {
        // Instantiate a copy of the code in CommonlyUsedFunctions called myFuncs
        // It's static because its used in Main() which is static
        static CommonlyUsedFunctions myFuncs = new CommonlyUsedFunctions();

        // This is the application program - Main()
        //
        // It will instantiate and manipulate objects of various classes

        static void Main(string[] args)
        {
            myFuncs.WriteSeparatorLine("Welcome to our first OOP Example");

            /*************************************************************************
             * Define/Instantiate a PlayingCard - an Ace of Hearts which is red
             *************************************************************************/
            myFuncs.WriteSeparatorLine("Instantiate and display a PlayingCard");

            //      data-type  name  = new data-type(initializers)
            PlayingCard aCard = new PlayingCard(1, "Hearts", "Red");

            Console.WriteLine($"aCard is: {aCard}");  // Display the PlayingCard

            /*************************************************************************
             * Define/Instantiate a new PlayingCard to be the same as aCard
             *     and display them both
             *************************************************************************/

            // PlayingCard newCard = aCard;  // This makes both newCard and aCard point to the same object
            PlayingCard newCard = new PlayingCard(aCard); // Use copy constructor

            Console.WriteLine($"  aCard is: {aCard}");
            Console.WriteLine($"newCard is: {newCard}");

            /*************************************************************************
             * Change the value in newCard to be a two
             *************************************************************************/
            myFuncs.WriteSeparatorLine("Change value in newCard to 2");

            //  newCard.cardValue = 2;   // Cannot access private data in an object
            newCard.CardValue = 2;       // Use property to change the value
            Console.WriteLine($"newCard is: {newCard}");
            Console.WriteLine($"  aCard is: {aCard}");


            /*******************************************************************************
             * Inheritance usage start here
             ******************************************************************************/

            myFuncs.WriteSeparatorLine("Instantiate an AmericanPlayingCard and display it");

            // Instantiate a subclass object using subclass ctor
            AmericanPlayingCard theCard = new AmericanPlayingCard(10, "Spades", "Black");
            
            // This will use the PlayingCard ToString() method to convert theCard to a straing
            Console.WriteLine($"theCard: {theCard}");

            AmericanPlayingCard theCard2 = new AmericanPlayingCard(10, "Spades", "Black");


            // This will use the PlayingCard .Equals() method
            if (theCard.Equals(theCard2))
            {
                Console.WriteLine("They are EQUAL");
            }
            else
            {
                {
                    Console.WriteLine("They are NOT equal");
                }
            }

            myFuncs.WriteSeparatorLine("Thanks for trying out our first OOP application!");
            myFuncs.PauseProgram();
        }
    }
}

