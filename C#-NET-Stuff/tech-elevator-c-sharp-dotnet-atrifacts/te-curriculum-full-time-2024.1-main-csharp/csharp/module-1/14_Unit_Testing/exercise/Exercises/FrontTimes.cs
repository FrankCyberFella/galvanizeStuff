﻿namespace Exercises
{
    public class FrontTimes
    {
        /*
        Given a string and a non-negative int n, we'll say that the front of the string is the first 3 chars, or
        whatever is there if the string is less than length 3. Return n copies of the front.
        GenerateString("Chocolate", 2) → "ChoCho"
        GenerateString("Chocolate", 3) → "ChoChoCho"
        GenerateString("Abc", 3) → "AbcAbcAbc"
        */
        public string GenerateString(string str, int n)
        {
            string result = "";
            if (!string.IsNullOrEmpty(str))
            {
                if (str.Length <= 3)
                {
                    for (int i = 0; i < n; i++)
                    {
                        result += str;
                    }
                }
                else
                {
                    for (int i = 0; i < n; i++)
                    {
                        result += str.Substring(0, 3);
                    }
                }
            }
            return result;
        }
    }
}
