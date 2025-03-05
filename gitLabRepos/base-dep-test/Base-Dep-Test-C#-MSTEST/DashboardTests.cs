using Moq;

namespace Base_Dep_Test_C__MSTEST;

[TestClass]
public class DashboardTests
{
    [TestMethod]
    public void CanBeCreated()
    {
        Dashboard aDashboard = new Dashboard();
        Assert.IsNotNull(aDashboard);
    }

    [TestMethod]
    public void CanGetEmptyStockList()
    {
        Dashboard aDashboard = new Dashboard();
        List<string> results = aDashboard.getStocks();
        Assert.AreEqual(0, results.Count);
    }
    [TestMethod]
    public void CanAddAStock()
    {
        Dashboard dashboard = new Dashboard();
        string stock = "LRN";
        dashboard.addStock(stock);
        List<string> results = dashboard.getStocks();
        Assert.AreEqual(1, results.Count());
        Assert.AreEqual(stock, results[0]);
    }

    [TestMethod]
    public void canDisplayStockPrices() {
        String stock = "LRN";
        var mockedApi = new Mock<IStockApi>();
        mockedApi.Setup(api => api.GetPrice("LRN")).Returns(20);
        Dashboard dashboard = new Dashboard(mockedApi.Object);
        dashboard.addStock(stock);
        String output = dashboard.display();
        Assert.That.ToString().Contains("LRN");
        Assert.That.ToString().Contains("$20");
    }

}



