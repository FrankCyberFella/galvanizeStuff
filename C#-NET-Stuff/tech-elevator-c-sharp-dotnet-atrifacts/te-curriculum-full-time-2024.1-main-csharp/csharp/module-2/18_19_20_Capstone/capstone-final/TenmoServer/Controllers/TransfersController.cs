using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using TenmoServer.DAO;
using TenmoServer.Exceptions;
using TenmoServer.Models;

namespace TenmoServer.Controllers
{
    [Route("[controller]")]
    [ApiController]
    [Authorize]
    public class TransfersController : ControllerBase
    {
        private readonly ITransferDao transferDao;
        private readonly IAccountDao accountDao;

        public TransfersController(ITransferDao transferDao, IAccountDao accountDao)
        {
            this.transferDao = transferDao;
            this.accountDao = accountDao;
        }

        [HttpGet("{id}")]
        public IActionResult GetTransfer(int id)
        {
            int? userId = GetCurrentUserId();
            if (!userId.HasValue)
            {
                return BadRequest();
            }

            try
            {
                Transfer t = transferDao.GetTransferById(id);
                // Only return the transfer if it involves the current user
                if (t != null && (t.AccountFrom.UserId == userId || t.AccountTo.UserId == userId))
                {
                    return Ok(t);
                }
                else
                {
                    return NotFound();
                }
            }
            catch (DaoException)
            {
                return StatusCode(500);
            }
        }

        [HttpPost]
        public IActionResult CreateTransfer(NewTransfer transfer)
        {
            IActionResult result = BadRequest();

            int? userId = GetCurrentUserId();
            if (!userId.HasValue)
            {
                return BadRequest();
            }

            // If this is a Send, the current user *must be* the from user.
            if (transfer.TransferType == TransferType.Send && transfer.UserFrom != userId)
            {
                return Forbid();
            }

            // If this is a Request, the current user *must be* the to user.
            if (transfer.TransferType == TransferType.Request && transfer.UserTo != userId)
            {
                return Forbid();
            }

            // If this is a Send (pre-approved), make sure there is enough balance
            try
            {
                Account fromAcct = accountDao.GetAccountByUserId(transfer.UserFrom);
                if (transfer.TransferType == TransferType.Send && fromAcct.Balance < transfer.Amount)
                {
                    return StatusCode(402); //402 = Payment Required, could be BadRequest
                }
            }
            catch (DaoException)
            {
                return StatusCode(500);
            }

            // Passed all checks, create transfer
            try
            {
                Transfer newTransfer = transferDao.CreateTransfer(transfer);
                if (newTransfer != null)
                {
                    result = Created("transfers/" + newTransfer.TransferId, newTransfer);
                }
            }
            catch (DaoException)
            {
                return StatusCode(500);
            }
            
            return result;
        }

        [HttpPut("{id}/approve")]
        public IActionResult ApproveTransfer(int id)
        {
            int? userId = GetCurrentUserId();
            if (!userId.HasValue)
            {
                return BadRequest();
            }

            Transfer transfer;
            try
            {
                transfer = transferDao.GetTransferById(id);
            }
            catch (DaoException)
            {
                return StatusCode(500);
            }

            if (transfer == null)
            {
                return NotFound();
            }

            // only requests can be approved, only the user who's been requested from can approve
            if (transfer.TransferType == TransferType.Request && transfer.AccountFrom.UserId == userId)
            {
                if (transfer.TransferStatus != TransferStatus.Pending)
                {
                    return Conflict();
                }
                else if (transfer.AccountFrom.Balance < transfer.Amount)
                {
                    // Insufficient balance to do the transfer
                    return StatusCode(402); //402 = Payment Required, could be BadRequest
                }
                else
                {
                    transfer.TransferStatus = TransferStatus.Approved;
                    try
                    {
                        Transfer updatedTransfer = transferDao.UpdateTransferStatus(transfer);
                        if (updatedTransfer != null)
                        {
                            return Ok(updatedTransfer); // return newly approved transaction
                        }
                    }
                    catch (DaoException)
                    {
                        return StatusCode(500);
                    }
                }
            }
            else // if not a request or not the user being requested from
            {
                return Conflict(); //or BadRequest
            }

            return BadRequest();
        }

        [HttpPut("{id}/reject")]
        public IActionResult RejectTransfer(int id)
        {
            int? userId = GetCurrentUserId();
            if (!userId.HasValue)
            {
                return BadRequest();
            }

            Transfer transfer;
            try
            {
                transfer = transferDao.GetTransferById(id);
            }
            catch (DaoException)
            {
                return StatusCode(500);
            }

            // only requests can be rejected, only the user who's been requested from can reject
            if (transfer.TransferType == TransferType.Request && transfer.AccountFrom.UserId == userId)
            {
                if (transfer.TransferStatus != TransferStatus.Pending)
                {
                    return Conflict();
                }
                else
                {
                    transfer.TransferStatus = TransferStatus.Rejected;
                    try
                    {
                        Transfer updatedTransfer = transferDao.UpdateTransferStatus(transfer);
                        if (updatedTransfer != null)
                        {
                            return Ok(updatedTransfer); // return newly rejected transaction
                        }
                    }
                    catch (DaoException)
                    {
                        return StatusCode(500);
                    }
                }
            }
            else // if not a request or not the user being requested from
            {
                return Conflict(); //or BadRequest
            }

            return BadRequest();
        }

        private int? GetCurrentUserId()
        {
            string userId = User.FindFirst("sub")?.Value;
            if (string.IsNullOrWhiteSpace(userId)) return null;
            int.TryParse(userId, out int userIdInt);
            return userIdInt;
        }
    }
}
