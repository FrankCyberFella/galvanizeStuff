using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Exercises.Tests
{
    [TestClass]
    public class NonStartTests
    {
        /*
        Given 2 strings, return their concatenation, except omit the first char of each. The strings will
        be at least length 1.
        GetPartialString("Hello", "There") → "ellohere"
        GetPartialString("java", "code") → "avaode"
        GetPartialString("shotl", "java") → "hotlava"
        */

        [TestMethod]
        public void FullStrings_ExpectNormalConcatenation()
        {
            //Arrange
            NonStart exercises = new NonStart();

            //Assert
            Assert.AreEqual("ihere", exercises.GetPartialString("Hi", "There"));
        }

        [TestMethod]
        public void PartialStrings_ExpectPartialConcatenation()
        {
            //Arrange
            NonStart exercises = new NonStart();

            //Assert
            Assert.AreEqual("here", exercises.GetPartialString("", "There"));
            Assert.AreEqual("i", exercises.GetPartialString("Hi", ""));
            Assert.AreEqual("here", exercises.GetPartialString(null, "There"));
            Assert.AreEqual("i", exercises.GetPartialString("Hi", null));
        }

        [TestMethod]
        public void NullString_ExpectEmptyString()
        {
            //Arrange
            NonStart exercises = new NonStart();

            //Assert
            Assert.AreEqual("here", exercises.GetPartialString(null, "There"));
            Assert.AreEqual("i", exercises.GetPartialString("Hi", null));
            Assert.AreEqual("", exercises.GetPartialString(null, null));
        }
    }
}
