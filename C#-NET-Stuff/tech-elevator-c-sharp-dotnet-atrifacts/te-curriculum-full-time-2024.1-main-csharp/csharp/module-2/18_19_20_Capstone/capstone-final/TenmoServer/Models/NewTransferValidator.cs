using System.ComponentModel.DataAnnotations;

namespace TenmoServer.Models
{
    /// <summary>
    /// This is a Custom Validator. Since there is no Data Annotation attribute that can be applied
    /// to make sure two fields are not equal, this custom validator does it. It's applied to the NewTransfer 
    /// class using the line:
    ///     [CustomValidation(typeof(NewTransferValidator), "ValidateUsers")]
    /// 
    /// We could have easily checked this in the controller and just returned BadRequest, but this is an opportunity 
    /// to show one of the thousands of ways the MVC framework is extensible. The hardest part is finding the 
    /// documentation (which I never did).
    /// </summary>
    public static class NewTransferValidator
    {
        public static ValidationResult ValidateUsers(object value, ValidationContext context)
        {
            // Validate that the users are different
            NewTransfer newTransfer = (NewTransfer)value;
            if (newTransfer.UserFrom == newTransfer.UserTo)
            {
                return new ValidationResult("From and To users must not be the same");
            }
            return ValidationResult.Success;
        }
    }
}
