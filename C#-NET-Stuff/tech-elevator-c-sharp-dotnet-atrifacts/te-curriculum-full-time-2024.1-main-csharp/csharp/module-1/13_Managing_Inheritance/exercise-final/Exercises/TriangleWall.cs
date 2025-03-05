namespace Exercises
{
    public class TriangleWall : Wall
    {
        public TriangleWall(string name, string color, int @base, int height) : base(name, color)
        {
            Height = height;
            Base = @base;
        }

        public int Height { get; }
        public int Base { get; }

        public override int GetArea()
        {
            return (Base * Height) / 2;
        }

        public override string ToString()
        {
            return $"{Name} ({Base}x{Height}) triangle";
        }
    }
}