using Microsoft.VisualStudio.TestTools.UnitTesting;

public class StockApiTests {
    [TestMethod]
    void throwsException() {
        StockApi stockApi = new StockApi();
        
        /*
        Exception exception = Assert.ThrowsException<Exception>(() -> {
            stockApi.GetPrice("TEST"));
        });
        Assert.IsTrue(exception.Message.Contains("Mock"));
        */
    }
}
