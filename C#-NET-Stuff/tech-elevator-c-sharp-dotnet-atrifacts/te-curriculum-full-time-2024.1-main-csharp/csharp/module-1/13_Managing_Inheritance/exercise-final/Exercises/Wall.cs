namespace Exercises
{
    public abstract class Wall
    {
        public Wall(string name, string color)
        {
            Color = color;
            Name = name;
        }

        public string Color { get; }
        public string Name { get; }

        public abstract int GetArea();
    }
}