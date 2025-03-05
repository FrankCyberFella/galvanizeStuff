using System;
using System.Collections.Generic;
using TenmoClient.Models;

namespace TenmoClient.Services
{
    public class TenmoConsoleService : ConsoleService
    {
        /************************************************************
            Print methods
        ************************************************************/
        public void PrintLoginMenu()
        {
            Console.Clear();
            Console.WriteLine("");
            Console.WriteLine("Welcome to TEnmo!");
            Console.WriteLine("1: Login");
            Console.WriteLine("2: Register");
            Console.WriteLine("0: Exit");
            Console.WriteLine("---------");
        }

        public void PrintMainMenu(string username)
        {
            Console.Clear();
            Console.WriteLine("");
            Console.WriteLine($"Hello, {username}!");
            Console.WriteLine("1: View your current balance");
            Console.WriteLine("2: View your past transfers");
            Console.WriteLine("3: View your pending requests");
            Console.WriteLine("4: Send TE bucks");
            Console.WriteLine("5: Request TE bucks");
            Console.WriteLine("6: Log out");
            Console.WriteLine("0: Exit");
            Console.WriteLine("---------");
        }
        public LoginUser PromptForLogin()
        {
            string username = PromptForString("User name");
            if (String.IsNullOrWhiteSpace(username))
            {
                return null;
            }
            string password = PromptForHiddenString("Password");

            LoginUser loginUser = new LoginUser
            {
                Username = username,
                Password = password
            };
            return loginUser;
        }

        // Add application-specific UI methods here...

        public void PrintTransfers(List<Transfer> transfers, int currentUserId)
        {
            Console.WriteLine("|--------------------------- Transfers ----------------------------|");
            Console.WriteLine("|    Id  | From/To                        |    Amount | Status     |");
            Console.WriteLine("|--------+--------------------------------+-----------+------------|");

            if (transfers.Count == 0)
            {
                Console.WriteLine("|               ***There are no transfers to list***               |");
            }
            foreach (Transfer transfer in transfers)
            {
                //show only the "From" or "To" for the other user in the transaction
                string fromTo;
                if (currentUserId == transfer.AccountFrom.UserId)
                {
                    fromTo = $"To: {transfer.AccountTo.Username}";
                }
                else
                {
                    fromTo = $"From: {transfer.AccountFrom.Username}";
                }
                Console.WriteLine($"| {transfer.TransferId,6} | {fromTo,-30} | {transfer.Amount,9:C} | {transfer.TransferStatus,-10} |");
            }
            Console.WriteLine("|------------------------------------------------------------------|");
        }

        public void PrintTransfer(Transfer transfer)
        {
            Console.WriteLine("--------------------------------------------");
            Console.WriteLine("Transfer Details");
            Console.WriteLine("--------------------------------------------");
            Console.WriteLine(" Id: " + transfer.TransferId);
            Console.WriteLine(" From: " + transfer.AccountFrom.Username);
            Console.WriteLine(" To: " + transfer.AccountTo.Username);
            Console.WriteLine(" Type: " + transfer.TransferType.ToString());
            Console.WriteLine(" Status: " + transfer.TransferStatus.ToString());
            Console.WriteLine(" Amount: " + transfer.Amount.ToString("C"));
        }

        public void PrintUsers(List<User> users, int currentUserId)
        {
            Console.WriteLine("|-------------- Users --------------|");
            Console.WriteLine("|    Id | Username                  |");
            Console.WriteLine("|-------+---------------------------|");
            foreach (User user in users)
            {
                if (user.UserId != currentUserId)
                {
                    Console.WriteLine($"|{user.UserId,6} | {user.Username,-25} |");
                }
            }
            Console.WriteLine("|-----------------------------------|");
        }

        /// <summary>
        /// Prompts the user to whether they want to approve or reject any 
        /// of the pending transfers that were displayed.
        /// </summary>
        /// <returns>"approve" or "reject" if the user wants to take action, NULL if not.</returns>
        public string PromptForApproveOrReject()
        {
            Console.WriteLine("");
            Console.WriteLine("Do you want to approve or reject any of these transactions?");
            Console.WriteLine("1: Approve");
            Console.WriteLine("2: Reject");
            Console.WriteLine("0: Don't approve or reject");
            int choice = PromptForInteger("Please choose an option", 0, 2, 0);
            switch (choice)
            {
                case 1:
                    return "approve";
                case 2:
                    return "reject";
            }
            return null;
        }

        /// <summary>
        /// Prompts for transfer ID to view, approve, or reject
        /// </summary>
        /// <param name="action">String to print in prompt. Expected values are "Approve" or "Reject" or "View"</param>
        /// <returns>ID of transfers to view, approve, or reject</returns>
        public Transfer PromptForTransferId(string action, List<Transfer> transfers)
        {
            Console.WriteLine("");
            while (true)
            {
                int transferId = PromptForInteger("Please enter transfer Id to " + action + " (0 to cancel)", 0);
                if (transferId == 0)
                {
                    return null;
                }
                Transfer transfer = FindTransfer(transfers, transferId);
                if (transfer != null)
                {
                    return transfer;
                }
                PrintError("Invalid transfer id. Type 0 to cancel.");
            }
        }

        private Transfer FindTransfer(List<Transfer> transfers, int transferToFind)
        {
            foreach (Transfer transfer in transfers)
            {
                if (transfer.TransferId == transferToFind)
                {
                    return transfer;
                }
            }
            return null;
        }

        /// <summary>
        /// Shows the user a list of all other users, and prompts to select one
        /// </summary>
        /// <param name="message">Prompt message</param>
        /// <param name="users">List of system users</param>
        /// <param name="currentUserId">Current user id. This will be filtered from the list when the list is displayed.</param>
        /// <returns>Selected user, NULL if the user canceled the operation.</returns>
        public User PromptForOtherUser(string message, List<User> users, int currentUserId)
        {
            while (true)
            {
                PrintUsers(users, currentUserId);
                int userId = PromptForInteger(message, 0);
                if (userId == 0)
                {
                    // cancel
                    return null;
                }
                if (userId == currentUserId)
                {
                    PrintError("You may not select yourself.");
                }
                else
                {
                    User user = FindUser(users, userId);
                    if (user != null)
                    {
                        return user;
                    }
                    PrintError("Invalid user id. Type 0 to cancel.");
                }
            }
        }

        private User FindUser(List<User> users, int userToFind)
        {
            foreach (User user in users)
            {
                if (user.UserId == userToFind)
                {
                    return user;
                }
            }
            return null;
        }

        /// <summary>
        /// Prompts for details on new transfers
        /// </summary>
        /// <returns>A NewTransfer object containing info to send to server</returns>
        public NewTransfer PromptForTransfer(TransferType transferType, List<User> users, int currentUserId)
        {
            //user to/from
            User otherUser = PromptForOtherUser(
                $"Id of the user you are {(transferType == TransferType.Request ? "requesting from" : "sending to")}",
                users, currentUserId);

            if (otherUser == null)
            {
                // cancel
                return null;
            }

            //amount
            decimal amount = PromptForDecimal($"Enter amount to {transferType.ToString().ToLower()}");
            NewTransfer newTransfer = new NewTransfer()
            {
                TransferType = transferType,
                UserFrom = transferType == TransferType.Request ? otherUser.UserId : currentUserId,
                UserTo = transferType == TransferType.Request ? currentUserId : otherUser.UserId,
                Amount = amount
            };
            return newTransfer;
        }
    }
}
