namespace Exercises
{
    public class RectangleWall : Wall
    {
        public RectangleWall(string name, string color, int length, int height) : base(name, color)
        {
            Height = height;
            Length = length;
        }

        public int Height { get; }
        public int Length { get; }

        public override int GetArea()
        {
            return Height * Length;
        }

        public override string ToString()
        {
            return $"{Name} ({Length}x{Height}) rectangle";
        }
    }
}