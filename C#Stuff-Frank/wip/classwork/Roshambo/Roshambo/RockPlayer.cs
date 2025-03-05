

using System.Reflection.Metadata.Ecma335;

namespace Roshambo
{
    // This is a subclass of the Player class
    // Always throws Rock
    // It doe not have a user supplied name 
    public class RockPlayer : Player
    {
        // No need for a ctor that takes any arguments
       
        // We do need a default ctor since values are constant (never change)
        public RockPlayer() : base("TheRock", Roshambo.RoshamboValues.Rock) { }

        public override Roshambo.RoshamboValues GenerateRoshambo() 
        {
            return Roshambo.RoshamboValues.Rock;
        }
    }
}
