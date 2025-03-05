namespace TenmoClient.Models
{
    public class Account
    {
        public int AccountId { get; set; }
        public int UserId { get; set; }
        public string Username { get; set; }
        public decimal Balance { get; set; }
    }
}
