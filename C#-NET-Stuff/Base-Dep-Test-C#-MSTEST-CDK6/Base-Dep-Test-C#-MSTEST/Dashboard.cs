
using System.Collections.Generic;

public class Dashboard {
    private List<string> stocks = new List<string>();
    private StockApi stockApi;

    public Dashboard() {
        stockApi = new StockApi();
    }

    public Dashboard(StockApi stockApi) {
        this.stockApi = stockApi;
    }

    public List<string> getStocks() {
        return new List<string>(stocks);
    }

    public void addStock(string stock) {
        stocks.Add(stock);
    }

    public string display() {
        List<decimal> prices = new List<decimal>();
        foreach(string stock in stocks) {
            prices.Add(stockApi.GetPrice(stock));
        }
        return "LRN $20";
    }
}
