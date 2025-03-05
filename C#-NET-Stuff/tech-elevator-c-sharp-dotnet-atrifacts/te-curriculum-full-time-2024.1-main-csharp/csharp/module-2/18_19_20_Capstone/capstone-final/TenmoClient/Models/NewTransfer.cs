namespace TenmoClient.Models
{
    public class NewTransfer
    {
        public int UserFrom { get; set; }
        public int UserTo { get; set; }
        public decimal Amount { get; set; }
        public TransferType TransferType { get; set; }
    }
}
