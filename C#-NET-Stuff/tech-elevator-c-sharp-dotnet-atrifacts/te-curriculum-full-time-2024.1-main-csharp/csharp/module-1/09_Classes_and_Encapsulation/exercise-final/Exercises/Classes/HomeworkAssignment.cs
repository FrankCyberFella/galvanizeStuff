namespace Exercises.Classes
{
    public class HomeworkAssignment
    {
        /// <summary>
        /// Correct marks received on the assignment
        /// </summary>
        public int EarnedMarks { get; set; }


        /// <summary>
        /// Possible number of marks to get right on the homework assignment.
        /// </summary>
        public int PossibleMarks { get; private set; }


        /// <summary>
        /// Name of the person who submitted the homework assignment
        /// </summary>
        public string SubmitterName { get; private set; }


        /// <summary>
        /// Letter grade for the assignment. (90-100 A, 80-89 B, 70-79 C, 60-69 D, otherwise F)
        /// </summary>
        public string LetterGrade
        {
            get
            {
                double percentage = (double)EarnedMarks / PossibleMarks;
                if (percentage >= 0.9)
                {
                    return "A";
                }
                else if (percentage >= 0.8)
                {
                    return "B";
                }
                else if (percentage >= 0.7)
                {
                    return "C";
                }
                else if (percentage >= 0.6)
                {
                    return "D";
                }
                else
                {
                    return "F";
                }
            }
        }


        /// <summary>
        /// Homework assignment requires possible marks
        /// </summary>
        /// <param name="possibleMarks"></param>
        public HomeworkAssignment(int possibleMarks, string submitterName)
        {
            this.PossibleMarks = possibleMarks;
            this.SubmitterName = submitterName;
        }
    }
}
