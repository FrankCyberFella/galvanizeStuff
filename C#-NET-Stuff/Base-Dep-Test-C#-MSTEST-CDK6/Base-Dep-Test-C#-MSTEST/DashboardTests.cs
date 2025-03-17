using System.Collections.Generic;

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
        Assert.AreEqual(1, results.Count);
        Assert.AreEqual(stock, results[0]);
    }
    /*
         [TestMethod]
         void canDisplayStockPrices() {
             string stock = "LRN";
             StockApi mockedApi = Mockito.mock(StockApi.class);
             Mockito.when(mockedApi.getPrice(stock)).thenReturn(BigDecimal.valueOf(20.00));
             Dashboard dashboard = new Dashboard(mockedApi);
             dashboard.addStock(stock);
             string output = dashboard.display();
             MatcherAssert.assertThat(output, CoreMatchers.containsstring("LRN"));
             MatcherAssert.assertThat(output, CoreMatchers.containsstring("$20"));
         }
    */
}



