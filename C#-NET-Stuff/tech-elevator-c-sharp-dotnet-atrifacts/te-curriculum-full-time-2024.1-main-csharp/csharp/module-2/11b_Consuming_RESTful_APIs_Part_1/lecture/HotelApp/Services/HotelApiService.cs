using HotelApp.Models;
using RestSharp;
using System;
using System.Collections.Generic;
using System.Net.Http;

namespace HotelApp.Services
{
    public class HotelApiService
    {
        protected static RestClient client = null;

        public HotelApiService(string apiUrl)
        {
            if (client == null)
            {
                client = new RestClient(apiUrl);
            }
        }

        public List<Hotel> GetHotels()
        {
            throw new NotImplementedException();
        }

        public List<Review> GetReviews()
        {
            throw new NotImplementedException();
        }

        public Hotel GetHotel(int hotelId)
        {
            throw new NotImplementedException();
        }

        public List<Review> GetHotelReviews(int hotelId)
        {
            throw new NotImplementedException();
        }

        public List<Hotel> GetHotelsWithRating(int starRating)
        {
            throw new NotImplementedException();
        }

        public City GetPublicAPIQuery()
        {
            throw new NotImplementedException();
        }
    }
}
