﻿namespace Exercises
{
    public partial class StringExercises
    {
        /*
        The web is built with HTML strings like "<i>Yay</i>" which draws Yay as italic text. In this example,
        the "i" tag makes <i> and </i> which surround the word "Yay". Given tag and word strings, create the
        HTML string with tags around the word, e.g. "<i>Yay</i>".
        MakeTags("i", "Yay") → "<i>Yay</i>"
        MakeTags("i", "Hello") → "<i>Hello</i>"
        MakeTags("cite", "Yay") → "<cite>Yay</cite>"
        */
        public string MakeTags(string tag, string word)
        {
            return "<" + tag + ">" + word + "</" + tag + ">";
        }
    }
}
