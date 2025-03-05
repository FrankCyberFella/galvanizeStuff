using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Roshambo
{
    public class RandomPlayer : Player
    {
        public RandomPlayer(string name, Roshambo.RoshamboValues choice) : base(name, choice) { }

        public override Roshambo.RoshamboValues GenerateRoshambo()
        {
            Random randomNum = new Random();
            int randomChoice = randomNum.Next(0, 3); // generate a random number between  0 and 2
            //switch (randomChoice)
            //{
            //    case 1:  return Roshambo.RoshamboValues.Rock;
            //    case 2:  return Roshambo.RoshamboValues.Paper;
            //    case 3:  return Roshambo.RoshamboValues.Scissors;
            //    default: return Roshambo.RoshamboValues.Rock;
            //}
            return (Roshambo.RoshamboValues)randomChoice;

        }
    }
}
