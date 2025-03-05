namespace Exercises
{
    public class SquareWall : RectangleWall
    {
        public SquareWall(string name, string color, int sideLength) : base(name, color, sideLength, sideLength) { }

        public override string ToString()
        {
            return $"{Name} ({Length}x{Height}) square";
        }
    }
}