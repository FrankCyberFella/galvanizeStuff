namespace Exercises
{
    public partial class StringExercises
    {
        /*
        Suppose the string "yak" is unlucky. Given a string, return a version where all the "yak" are removed.
        The "yak" strings will not overlap.
        StringYak("yakpak") → "pak"
        StringYak("pakyak") → "pak"
        StringYak("yak123ya") → "123ya"
        */
        public string StringYak(string str)
        {
            int posYak = str.IndexOf("yak");
            while (posYak != -1)
            {
                str = str.Substring(0, posYak) + str.Substring(posYak + 3);
                posYak = str.IndexOf("yak");
            }
            return str;
        }
    }
}
