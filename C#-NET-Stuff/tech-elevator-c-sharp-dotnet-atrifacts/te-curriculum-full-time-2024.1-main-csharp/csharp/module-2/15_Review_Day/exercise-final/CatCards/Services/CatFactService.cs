﻿using CatCards.Models;
using RestSharp;

namespace CatCards.Services
{
    public class CatFactService : ICatFactService
    {
        private static readonly string API_URL = "https://teapi.netlify.app/api/cats/facts/random";
        private readonly RestClient client = new RestClient();

        public CatFact GetFact()
        {
            RestRequest request = new RestRequest(API_URL);
            IRestResponse<CatFact> response = client.Get<CatFact>(request);
            if (response.ResponseStatus == ResponseStatus.Completed && response.IsSuccessful)
            {
                return response.Data;
            }
            else
            {
                return null;
            }
        }
    }
}
