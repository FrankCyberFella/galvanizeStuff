using System;
using System.Collections;
using GeneralPurposeFunctions;   // Give me access to the GeneralPurposeFunction Code

namespace Day_3_Playing_Card_Example_V2
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
            PlayingCard aCard = new PlayingCard(1, "Hearts"); // Call the 2-arg ctor

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

            /*************************************************************************
             * Try and define a PlayingCard with an invalid Suit (Josh)
             *************************************************************************/
            myFuncs.WriteSeparatorLine("Try and define a PlayingCard with an invalid Suit (Josh)");
            PlayingCard card3 = new PlayingCard(3, "Josh");
            Console.WriteLine($"  card3 is: {card3}");

            /*************************************************************************
             * Try and define a PlayingCard with an invalid value (42)
             *************************************************************************/
            myFuncs.WriteSeparatorLine("Try and define a PlayingCard with an invalid value (42)");
            PlayingCard card4 = new PlayingCard(42, "Diamonds");
            Console.WriteLine($"  card4 is: {card4}");

            /*************************************************************************
             * Determine if two PlayingCards are equal to each other
             *************************************************************************/
            myFuncs.WriteSeparatorLine("Determine if two PlayingCards are equal to each other");
          
            // Create a new card that is a copy of aCard
            PlayingCard card5 = new PlayingCard(aCard); // Use copy constructor

            // Display the two cards
            Console.WriteLine($"aCard is: {aCard}");
            Console.WriteLine($"card5 is: {card5}");

            // == compares the location of the objects NOT the data within the objects
            if (aCard == card5)
            {
                Console.WriteLine("The cards ARE equal!");
            }
            else
            {
                Console.WriteLine("The cards not NOT equal!");
            }
            myFuncs.WriteSeparatorLine("Compare objects using .Equals");
            // We need to use the .Equals() method to compare the content of object
            if (aCard.Equals(card5))
            {
                Console.WriteLine("The cards ARE equal!");
            }
            else
            {
                Console.WriteLine("The cards not NOT equal!");
            }
            /*************************************************************************
             * Display the HashCodes of two equal objects
             *************************************************************************/
            myFuncs.WriteSeparatorLine("Display the HashCodes of two equal objects");
            Console.WriteLine($"aCard HashCode is: {aCard.GetHashCode()}");
            Console.WriteLine($"card5 HashCode is: {card5.GetHashCode()}");
            Console.WriteLine($"card4 HashCode is: {card4.GetHashCode()}");

            myFuncs.WriteSeparatorLine("Thanks for trying out our first OOP application!");
            myFuncs.PauseProgram();
        }
    }
}
