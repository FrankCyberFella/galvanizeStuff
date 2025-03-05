using Microsoft.VisualStudio.TestTools.UnitTesting;
[TestClass]
class DashboardTests {
  
    [TestMethod]
    void CanBeCreated() {
        Dashboard dashboard = new Dashboard();
        Assert.IsNotNull(dashboard);
    }

    [TestMethod]
    public void CanGetEmptyStockList() {
        Dashboard dashboard = new Dashboard();
        List<String> results = dashboard.getStocks();
        Assert.Equals(0, results.Count());
    }

    [TestMethod]
    public void CanAddAStock() {
        Dashboard dashboard = new Dashboard();
        String stock = "LRN";
        dashboard.addStock(stock);
        List<String> results = dashboard.getStocks();
        Assert.Equals(1, results.Count());
        Assert.Equals(stock, results[0]);
    }
/*
    [TestMethod]
    void canDisplayStockPrices() {
        String stock = "LRN";
        StockApi mockedApi = Mockito.mock(StockApi.class);
        Mockito.when(mockedApi.getPrice(stock)).thenReturn(BigDecimal.valueOf(20.00));
        Dashboard dashboard = new Dashboard(mockedApi);
        dashboard.addStock(stock);
        String output = dashboard.display();
        MatcherAssert.assertThat(output, CoreMatchers.containsString("LRN"));
        MatcherAssert.assertThat(output, CoreMatchers.containsString("$20"));
    }
    */
}
