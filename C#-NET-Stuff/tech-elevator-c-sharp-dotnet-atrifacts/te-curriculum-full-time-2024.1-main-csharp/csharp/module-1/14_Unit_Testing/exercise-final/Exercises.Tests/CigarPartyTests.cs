using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Exercises.Tests
{
    [TestClass]
    public class CigarPartyTests
    {
        /*
        HaveParty(30, false) → false
        HaveParty(50, false) → true
        HaveParty(70, true) → true
        */
        [TestMethod]
        public void RangeOfValuesForWeekendTests()
        {
            //Arrange
            CigarParty party = new CigarParty();

            //Assert
            //Good Parties
            Assert.AreEqual(true, party.HaveParty(40, true));
            Assert.AreEqual(true, party.HaveParty(60, true));
            Assert.AreEqual(true, party.HaveParty(80, true));

            //Bad Parties
            Assert.AreEqual(false, party.HaveParty(39, true));
        }

        [TestMethod]
        public void RangeOfValuesForWeekdayTests()
        {
            //Arrange
            CigarParty party = new CigarParty();

            //Assert
            //Good Parties
            Assert.AreEqual(true, party.HaveParty(40, false));
            Assert.AreEqual(true, party.HaveParty(60, false));

            //Bad Parties
            Assert.AreEqual(false, party.HaveParty(39, false));
            Assert.AreEqual(false, party.HaveParty(61, false));
        }
    }
}
