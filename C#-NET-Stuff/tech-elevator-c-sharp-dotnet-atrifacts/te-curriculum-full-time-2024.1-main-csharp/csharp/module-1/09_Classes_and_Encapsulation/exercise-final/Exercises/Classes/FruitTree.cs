namespace Exercises.Classes
{
    public class FruitTree
    {
        /// <summary>
        /// Type of fruit on the tree
        /// </summary>
        public string TypeOfFruit { get; private set; }


        /// <summary>
        /// Number of fruit pieces remaining
        /// </summary>
        public int PiecesOfFruitLeft { get; private set; }


        /// <summary>
        /// Creates a new fruit tree.
        /// </summary>
        /// <param name="typeOfFruit">type of fruit the tree holds</param>
        /// <param name="startingPiecesOfFruit">number of fruit pieces to start on the tree</param>
        public FruitTree(string typeOfFruit, int startingPiecesOfFruit)
        {
            this.PiecesOfFruitLeft = startingPiecesOfFruit;
            this.TypeOfFruit = typeOfFruit;
        }

        /// <summary>
        /// Picks fruit off of the tree and reduces the number of remaining pieces.
        /// </summary>
        /// <param name="numberOfPiecesToRemove">number of fruit pieces to remove</param>
        /// <returns>True if there is enough fruit to pick, false if not.</returns>
        public bool PickFruit(int numberOfPiecesToRemove)
        {
            if (this.PiecesOfFruitLeft - numberOfPiecesToRemove >= 0)
            {
                this.PiecesOfFruitLeft -= numberOfPiecesToRemove;
                return true;
            }

            return false;
        }
    }
}
