using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Exercises.Tests
{
    [TestClass]
    public class MaxEndTests
    {
        /*
        Given an array of ints length 3, figure out which is larger between the first and last elements
        in the array, and set all the other elements to be that value. Return the changed array.
        MakeArray([1, 2, 3]) → [3, 3, 3]
        MakeArray([11, 5, 9]) → [11, 11, 11]
        MakeArray([2, 11, 3]) → [3, 3, 3]
        */

        [TestMethod]
        public void EndLargerTest()
        {
            //Arrange
            MaxEnd3 exercises = new MaxEnd3();

            //Assert
            CollectionAssert.AreEqual(new int[] { 3, 3, 3 }, exercises.MakeArray(new int[] { 1, 0, 3 }));
            CollectionAssert.AreEqual(new int[] { 3, 3, 3 }, exercises.MakeArray(new int[] { 1, 10, 3 }));
        }

        [TestMethod]
        public void BeginningLargerTest()
        {
            //Arrange
            MaxEnd3 exercises = new MaxEnd3();

            //Assert
            CollectionAssert.AreEqual(new int[] { 13, 13, 13 }, exercises.MakeArray(new int[] { 13, 0, 3 }));
            CollectionAssert.AreEqual(new int[] { 13, 13, 13 }, exercises.MakeArray(new int[] { 13, 20, 3 }));
        }

        [TestMethod]
        public void EndSameSizesTest()
        {
            //Arrange
            MaxEnd3 exercises = new MaxEnd3();

            //Assert
            CollectionAssert.AreEqual(new int[] { 10, 10, 10 }, exercises.MakeArray(new int[] { 10, 0, 10 }));
            CollectionAssert.AreEqual(new int[] { 10, 10, 10 }, exercises.MakeArray(new int[] { 10, 20, 10 }));
        }
    }
}
