using System.Collections.Generic;
using TenmoServer.Models;

namespace TenmoServer.DAO
{
    public interface ITransferDao
    {
        Transfer GetTransferById(int transferId);

        Transfer CreateTransfer(NewTransfer transfer);

        IList<Transfer> GetTransfersByUserId(int userId);

        IList<Transfer> GetPendingTransfersByUserId(int userId);

        Transfer UpdateTransferStatus(Transfer transfer);
    }
}
