public class StockApi : IStockApi
{
    public Decimal GetPrice(String stock)
    {
        throw new Exception("Mock out external API requests");
    }
}
