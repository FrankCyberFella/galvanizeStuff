using System.Collections.Generic;
using Microsoft.AspNetCore.Mvc;
using AuctionApp.Models;
using AuctionApp.DAO;

namespace AuctionApp.Controllers
{
    [Route("[controller]")]
    [ApiController]
    public class AuctionsController : ControllerBase
    {
        private readonly IAuctionDao dao;

        public AuctionsController(IAuctionDao auctionDao = null)
        {
            if (auctionDao == null)
            {
                dao = new AuctionMemoryDao();
            }
            else
            {
                dao = auctionDao;
            }
        }

        [HttpGet]
        public List<Auction> List(string title_like = "", double currentBid_lte = 0)
        {
            if (title_like != "" && currentBid_lte > 0)
            {
                return dao.GetAuctionsByTitleAndMaxBid(title_like, currentBid_lte);
            }
            else if (title_like != "")
            {
                return dao.GetAuctionsByTitle(title_like);
            }
            else if (currentBid_lte > 0)
            {
                return dao.GetAuctionsByMaxBid(currentBid_lte);
            }
            else
            {
                return dao.GetAuctions();
            }
        }

        [HttpGet("{id}")]
        public Auction Get(int id)
        {
            Auction auction = dao.GetAuctionById(id);
            return auction;
        }

        [HttpPost]
        public Auction Create(Auction auction)
        {
            if (auction.IsValid)
            {
                return dao.CreateAuction(auction);
            }
            return null;
        }
    }
}
