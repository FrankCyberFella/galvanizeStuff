using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using TenmoServer.Exceptions;
using TenmoServer.Models;

namespace TenmoServer.DAO
{
    public class TransferSqlDao : ITransferDao
    {
        private readonly string connectionString;
        private readonly string SQL_SelectTransfer = "SELECT transfer_id, transfer_type_id, transfer_status_id, amount, " +
                                                        "aFrom.account_id as 'fromAcct', aFrom.user_id as 'fromUser', uFrom.username as 'fromName', aFrom.balance as 'fromBal', " +
                                                        "aTo.account_id as 'toAcct', aTo.user_id as 'toUser', uTo.username as 'toName', aTo.balance as 'toBal' " +
                                                        "FROM transfer " +
                                                        "INNER JOIN account aFrom on account_from = aFrom.account_id " +
                                                        "INNER JOIN tenmo_user uFrom on uFrom.user_id = aFrom.user_id " +
                                                        "INNER JOIN account aTo on account_to = aTo.account_id " +
                                                        "INNER JOIN tenmo_user uTo on uTo.user_id = aTo.user_id ";

        public TransferSqlDao(string dbConnectionString)
        {
            connectionString = dbConnectionString;
        }

        public Transfer GetTransferById(int transferId)
        {
            Transfer transfer = null;

            string sql = SQL_SelectTransfer + "WHERE transfer_id = @transferId";

            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@transferId", transferId);
                    SqlDataReader reader = cmd.ExecuteReader();

                    if (reader.Read())
                    {
                        transfer = MapRowToTransfer(reader);
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("SQL exception occurred", ex);
            }

            return transfer;
        }

        public Transfer CreateTransfer(NewTransfer transfer)
        {
            Transfer newTransfer = null;

            string sql = "INSERT INTO transfer (transfer_type_id, transfer_status_id, amount, account_from, account_to) " +
                         "OUTPUT INSERTED.transfer_id " +
                         "VALUES (@transferType, @transferStatus, @amount, " +
                            "(SELECT account_id FROM account WHERE user_id = @userFrom), " +
                            "(SELECT account_id FROM account WHERE user_id = @userTo))";

            int newTransferId = 0;
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@transferType", transfer.TransferType);
                    cmd.Parameters.AddWithValue("@transferStatus",
                                        transfer.TransferType == TransferType.Request ?
                                        TransferStatus.Pending : TransferStatus.Approved); //pending if request, approved if send
                    cmd.Parameters.AddWithValue("@userFrom", transfer.UserFrom);
                    cmd.Parameters.AddWithValue("@userTo", transfer.UserTo);
                    cmd.Parameters.AddWithValue("@amount", transfer.Amount);

                    newTransferId = Convert.ToInt32(cmd.ExecuteScalar());

                    if (transfer.TransferType == TransferType.Send) //sending doesn't need approval, just do it
                    {
                         TransferMoney(transfer.Amount, transfer.UserFrom, transfer.UserTo);
                    }
                }
                newTransfer = GetTransferById(newTransferId);
            }
            catch (SqlException ex)
            {
                throw new DaoException("SQL exception occurred", ex);
            }

            return newTransfer;
        }

        public IList<Transfer> GetTransfersByUserId(int userId)
        {
            IList<Transfer> transfers = new List<Transfer>();

            string sql = SQL_SelectTransfer + "WHERE (account_from IN (SELECT account_id FROM account WHERE user_id = @user_id) OR account_to IN (SELECT account_id FROM account WHERE user_id = @user_id))";

            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@user_id", userId);
                    SqlDataReader reader = cmd.ExecuteReader();

                    while (reader.Read())
                    {
                        Transfer transfer = MapRowToTransfer(reader);
                        transfers.Add(transfer);
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("SQL exception occurred", ex);
            }

            return transfers;
        }

        public IList<Transfer> GetPendingTransfersByUserId(int userId)
        {
            IList<Transfer> transfers = new List<Transfer>();

            string sql = SQL_SelectTransfer + "WHERE transfer_status_id = 1 AND account_from IN (SELECT account_id FROM account WHERE user_id = @user_id)";

            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@user_id", userId);
                    SqlDataReader reader = cmd.ExecuteReader();

                    while (reader.Read())
                    {
                        Transfer transfer = MapRowToTransfer(reader);
                        transfers.Add(transfer);
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("SQL exception occurred", ex);
            }

            return transfers;
        }

        public Transfer UpdateTransferStatus(Transfer transfer)
        {
            Transfer updatedTransfer = null;

            string sql = "UPDATE transfer SET transfer_status_id = @transfer_status_id WHERE transfer_id = @transfer_id";

            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@transfer_status_id", (int)transfer.TransferStatus);
                    cmd.Parameters.AddWithValue("@transfer_id", transfer.TransferId);

                    int rowsAffected = cmd.ExecuteNonQuery();

                    if (rowsAffected == 0)
                    {
                        throw new DaoException("Zero rows affected, expected at least one");
                    }

                    if (transfer.TransferStatus == TransferStatus.Approved)
                    {
                        TransferMoney(transfer.Amount, transfer.AccountFrom.UserId, transfer.AccountTo.UserId);
                    }
                }
                updatedTransfer = GetTransferById(transfer.TransferId);
            }
            catch (SqlException ex)
            {
                throw new DaoException("SQL exception occurred", ex);
            }

            return updatedTransfer;
        }

        private void TransferMoney(decimal amount, int userFrom, int userTo)
        {
            string sql = "UPDATE account SET balance = (balance - @amount) WHERE account_id = (SELECT account_id FROM account WHERE user_id = @userFrom); " +
                         "UPDATE account SET balance = (balance + @amount) WHERE account_id = (SELECT account_id FROM account WHERE user_id = @userTo)";

            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@amount", amount);
                    cmd.Parameters.AddWithValue("@userFrom", userFrom);
                    cmd.Parameters.AddWithValue("@userTo", userTo);
                    int rowsAffected = cmd.ExecuteNonQuery();

                    if (rowsAffected != 2) // expect two rows affected, no more no less
                    {
                        throw new DaoException($"Expected two rows affected, but {rowsAffected} affected instead");
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("SQL exception occurred", ex);
            }
        }

        private Transfer MapRowToTransfer(SqlDataReader reader)
        {
            Transfer transfer = new Transfer();
            transfer.TransferId = Convert.ToInt32(reader["transfer_id"]);
            transfer.TransferType = (TransferType)Convert.ToInt32(reader["transfer_type_id"]);
            transfer.TransferStatus = (TransferStatus)Convert.ToInt32(reader["transfer_status_id"]);
            transfer.Amount = Convert.ToDecimal(reader["amount"]);

            Account accountFrom = new Account();
            accountFrom.AccountId = Convert.ToInt32(reader["fromAcct"]);
            accountFrom.UserId = Convert.ToInt32(reader["fromUser"]);
            accountFrom.Username = Convert.ToString(reader["fromName"]);
            accountFrom.Balance = Convert.ToInt32(reader["fromBal"]);
            transfer.AccountFrom = accountFrom;

            Account accountTo = new Account();
            accountTo.AccountId = Convert.ToInt32(reader["toAcct"]);
            accountTo.UserId = Convert.ToInt32(reader["toUser"]);
            accountTo.Username = Convert.ToString(reader["toName"]);
            accountTo.Balance = Convert.ToInt32(reader["toBal"]);
            transfer.AccountTo = accountTo;

            return transfer;
        }
    }
}
