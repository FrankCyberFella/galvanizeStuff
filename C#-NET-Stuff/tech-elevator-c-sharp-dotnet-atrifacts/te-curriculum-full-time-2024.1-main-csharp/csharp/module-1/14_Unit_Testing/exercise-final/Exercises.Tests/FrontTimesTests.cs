using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Exercises.Tests
{
    [TestClass]
    public class FrontTimesTests
    {
        /*
        Given a string and a non-negative int n, we'll say that the front of the string is the first 3 chars, or
        whatever is there if the string is less than length 3. Return n copies of the front.
        GenerateString("Chocolate", 2) → "ChoCho"
        GenerateString("Chocolate", 3) → "ChoChoCho"
        GenerateString("Abc", 3) → "AbcAbcAbc"
        */

        [TestMethod]
        public void StringGreaterThan3Tests()
        {
            //Arrange
            FrontTimes exercise = new FrontTimes();

            //Assert
            Assert.AreEqual("JosJosJos", exercise.GenerateString("Josh", 3));
            Assert.AreEqual("JosJos", exercise.GenerateString("Josh", 2));
            Assert.AreEqual("Jos", exercise.GenerateString("Josh", 1));
        }

        [TestMethod]
        public void StringLessThan3Tests()
        {
            //Arrange
            FrontTimes exercise = new FrontTimes();

            //Assert
            Assert.AreEqual("JoJoJo", exercise.GenerateString("Jo", 3));
            Assert.AreEqual("JoJo", exercise.GenerateString("Jo", 2));
            Assert.AreEqual("Jo", exercise.GenerateString("Jo", 1));
        }

        [TestMethod]
        public void EmptyStringTests()
        {
            //Arrange
            FrontTimes exercise = new FrontTimes();

            //Assert
            Assert.AreEqual("", exercise.GenerateString("", 3));
            Assert.AreEqual("", exercise.GenerateString("", 2));
            Assert.AreEqual("", exercise.GenerateString("", 1));
        }

        [TestMethod]
        public void NullStringTests()
        {
            //Arrange
            FrontTimes exercise = new FrontTimes();

            //Assert
            Assert.AreEqual("", exercise.GenerateString(null, 3));
            Assert.AreEqual("", exercise.GenerateString(null, 2));
            Assert.AreEqual("", exercise.GenerateString(null, 1));
        }
    }
}
