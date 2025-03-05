namespace Exercises
{
    public partial class StringExercises
    {
        /*
        Given a non-empty string like "Code" return a string like "CCoCodCode".
        StringSplosion("Code") → "CCoCodCode"
        StringSplosion("abc") → "aababc"
        StringSplosion("ab") → "aab"
        */
        public string StringSplosion(string str)
        {
            string result = "";
            for (int i = 0; i <= str.Length; i++)
            {
                result += str.Substring(0, i);
            }
            return result;
        }
    }
}
