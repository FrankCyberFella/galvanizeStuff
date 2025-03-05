using System.Collections.Generic;

namespace Exercises
{
    public partial class Exercises
    {
        /*
         * Given two Dictionaries, Dictionary<string, int>, merge the two into a new Dictionary, Dictionary<string, int> where keys in Dictionary2,
         * and their int values, are added to the int values of matching keys in Dictionary1. Return the new Dictionary.
         *
         * Unmatched keys and their int values in Dictionary2 are simply added to Dictionary1.
         *
         * ConsolidateInventory({"SKU1": 100, "SKU2": 53, "SKU3": 44}, {"SKU2":11, "SKU4": 5})
         * 	 → {"SKU1": 100, "SKU2": 64, "SKU3": 44, "SKU4": 5}
         *
         */
        public Dictionary<string, int> ConsolidateInventory(Dictionary<string, int> mainWarehouse,
                                                            Dictionary<string, int> remoteWarehouse)
        {
            //Create a new dictionary, passing the mainWarehouse as a seed
            Dictionary<string, int> consolidatedInventory = new Dictionary<string, int>(mainWarehouse);

            foreach (KeyValuePair<string, int> kvp in remoteWarehouse)
            {
                string sku = kvp.Key;
                int value = kvp.Value;

                // if it contains it, add to it
                if (consolidatedInventory.ContainsKey(sku))
                {
                    consolidatedInventory[sku] = consolidatedInventory[sku] + value;
                }
                else // add the value of the sku by itself
                {
                    consolidatedInventory[sku] = value;
                }
            }

            return consolidatedInventory;
        }
    }
}
