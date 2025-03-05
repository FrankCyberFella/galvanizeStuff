namespace Exercises
{
    public partial class StringExercises
    {
        /*
        Given a string, return true if it ends in "ly".
        EndsLy("oddly") → true
        EndsLy("y") → false
        EndsLy("oddy") → false
        */
        public bool EndsLy(string str)
        {
            int len = str.Length;
            if (len < 2)
            {
                return false;
            }
            else
            {
                return (str.Substring(len - 2) == ("ly"));
            }
        }
    }
}
