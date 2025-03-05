
public class Dashboard {
    private List<String> stocks = new List<String>();
    private StockApi stockApi;

    public Dashboard() {
        stockApi = new StockApi();
    }

    public Dashboard(StockApi stockApi) {
        this.stockApi = stockApi;
    }

    public List<String> getStocks() {
        return new List<String>(stocks);
    }

    public void addStock(String stock) {
        stocks.Add(stock);
    }

    public String display() {
        List<decimal> prices = new List<decimal>();
        foreach(String stock in stocks) {
            prices.Add(stockApi.GetPrice(stock));
        }
        return "LRN $20";
    }
}
