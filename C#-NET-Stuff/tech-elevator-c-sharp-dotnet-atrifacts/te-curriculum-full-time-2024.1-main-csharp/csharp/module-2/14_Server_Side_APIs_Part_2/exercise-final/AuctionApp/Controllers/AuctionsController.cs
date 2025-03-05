using System.Collections.Generic;
using Microsoft.AspNetCore.Mvc;
using AuctionApp.Models;
using AuctionApp.DAO;
using AuctionApp.Exceptions;

namespace AuctionApp.Controllers
{
    [Route("[controller]")]
    [ApiController]
    public class AuctionsController : ControllerBase
    {
        private readonly IAuctionDao dao;

        public AuctionsController(IAuctionDao auctionDao)
        {
            dao = auctionDao;
        }

        [HttpGet]
        public List<Auction> List(string title_like = "", double currentBid_lte = 0)
        {
            if (title_like != "")
            {
                return dao.GetAuctionsByTitle(title_like);
            }
            if (currentBid_lte > 0)
            {
                return dao.GetAuctionsByMaxBid(currentBid_lte);
            }

            return dao.GetAuctions();
        }

        [HttpGet("{id}")]
        public ActionResult<Auction> Get(int id)
        {
            Auction auction = dao.GetAuctionById(id);
            if (auction != null)
            {
                return Ok(auction);
            }
            else
            {
                return NotFound();
            }
        }

        [HttpPost]
        public ActionResult<Auction> Create(Auction auction)
        {
            Auction returnAuction = dao.CreateAuction(auction);
            return Created($"/auctions/{returnAuction.Id}", returnAuction);
        }

        [HttpPut("{id}")]
        public ActionResult<Auction> Update(int id, Auction auction)
        {
            // The id on the URL takes precedence over the id in the request body, if any
            auction.Id = id;

            try
            {
                Auction result = dao.UpdateAuction(auction);
                return Ok(result);
            }
            catch (DaoException)
            {
                return NotFound("Auction to update not found");
            }
        }

        [HttpDelete("{id}")]
        public ActionResult Delete(int id)
        {
            Auction auction = dao.GetAuctionById(id);
            if (auction == null)
            {
                return NotFound();
            }

            bool result = dao.DeleteAuctionById(id);
            if (result)
            {
                return NoContent();
            }
            else
            {
                return BadRequest();
            }
        }
    }
}
