﻿namespace Exercises
{
    public class StringBits
    {
        /*
        Given a string, return a new string made of every other char starting with the first, so "Hello" yields "Hlo".
        GetBits("Hello") → "Hlo"
        GetBits("Hi") → "H"
        GetBits("Heeololeo") → "Hello"
        */
        public string GetBits(string str)
        {
            string result = "";
            if (!string.IsNullOrEmpty(str))
            { 
                for (int i = 0; i < str.Length; i++)
                {
                    if (i % 2 == 0)
                    {
                        result += str[i];
                    }
                }
            }
            return result;
        }
    }
}
