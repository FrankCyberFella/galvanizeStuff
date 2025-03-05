using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
namespace Base_Dep_Test_C__MSTEST;

[TestClass]
public class StockApiTests {

    [TestMethod]
    public void ThrowsException() {
        StockApi stockApi = new StockApi();

        Exception exception = Assert.ThrowsException<Exception>(() => {
            stockApi.GetPrice("TEST");
        });
  
        Assert.IsTrue(exception.Message.Contains("Mock"));
    } 
}

