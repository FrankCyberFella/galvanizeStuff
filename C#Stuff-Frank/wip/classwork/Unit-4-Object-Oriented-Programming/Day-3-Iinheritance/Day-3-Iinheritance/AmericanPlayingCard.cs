using Day_3_Inheritance;

namespace Day_3_Iinheritance
{
    // This will be a subclass of a PlayingCard
    // and will add what an AmericanPlaying needs that differs from a PlayingCard

    public class AmericanPlayingCard : PlayingCard  // Indicate PlayingCard is our base class
    {
        // NO Data in the subclass
        // NO methods other than the constructor

        // We get access to all the data and methods in the base class PlayingCard

        // 3-arg Constructor for an AmericanPlayingCard
        //       it must call the base() to initialize the base class
        //        with any values it gets when instantiated
        public AmericanPlayingCard(int theValue, string theSuit, string theColor) 
                                : base(theValue, theSuit, theColor) // Pass all data to base class ctor
        { }


    }
}