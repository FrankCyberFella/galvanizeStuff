using RestSharp;
using System.Collections.Generic;
using TenmoClient.Models;

namespace TenmoClient.Services
{
    public class TenmoApiService : AuthenticatedApiService
    {
        public readonly string ApiUrl;

        public TenmoApiService(string apiUrl) : base(apiUrl) { }

        // Add methods to call api here...

        //users endpoints
        public List<User> GetUsers()
        {
            RestRequest request = new RestRequest("users");
            IRestResponse<List<User>> response = client.Get<List<User>>(request);

            CheckForError(response);
            return response.Data;
        }

        //transfers endpoint
        public Transfer GetTransfer(int id)
        {
            RestRequest request = new RestRequest("transfers/" + id);
            IRestResponse<Transfer> response = client.Get<Transfer>(request);

            CheckForError(response);
            return response.Data;
        }

        public Transfer AddTransfer(NewTransfer newTransfer)
        {
            RestRequest request = new RestRequest("transfers");
            request.AddJsonBody(newTransfer);
            IRestResponse<Transfer> response = client.Post<Transfer>(request);

            CheckForError(response);
            return response.Data;
        }

        public Transfer ApproveTransfer(int transferId)
        {
            RestRequest request = new RestRequest("transfers/" + transferId + "/approve");
            IRestResponse<Transfer> response = client.Put<Transfer>(request);

            CheckForError(response);
            return response.Data;
        }

        public Transfer RejectTransfer(int transferId)
        {
            RestRequest request = new RestRequest("transfers/" + transferId + "/reject");
            IRestResponse<Transfer> response = client.Put<Transfer>(request);

            CheckForError(response);
            return response.Data;
        }

        //account endpoints
        public decimal GetBalance()
        {
            RestRequest request = new RestRequest("account/balance");
            IRestResponse<decimal> response = client.Get<decimal>(request);

            CheckForError(response);
            return response.Data;
        }

        public List<Transfer> GetTransfers() //maybe should be `/transfers`?
        {
            RestRequest request = new RestRequest("account/transfers");
            IRestResponse<List<Transfer>> response = client.Get<List<Transfer>>(request);

            CheckForError(response);
            return response.Data;
        }

        public List<Transfer> GetPendingTransfers()
        {
            RestRequest request = new RestRequest("account/pending");
            IRestResponse<List<Transfer>> response = client.Get<List<Transfer>>(request);

            CheckForError(response);
            return response.Data;
        }
    }
}
