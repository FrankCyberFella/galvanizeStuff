import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

public class App {
    public static void main(String[] args) throws Exception {
        System.out.println("Hello, World!");

        // A Java Map is like a C# Dictionary
        //
        // Used to store a key/value pair (aka Associative Array)
        //
        // the key must be unique (just like C#)
        // the value does not have to be unique
        //
        // Maps can hold objects (no primitives like int, float, double, etc)
        //
        // if an attempt is made to add element with and existing key:
        //
        //      C# Dictionary: an Exception is thrown
        //      Java Map: the value is updated (Java does not tell you it updated the value)
        //
        // Just like in C#, it's a good idea to check and see if a key exists before you 
        //      add an element so you can handle the situation (if you want to)
        //
        // Java has three types of Maps:
        //
        //       HashMap - elements are stored in an unknown order (most efficient)
        //       TreeMap - elements are stored in key sequence order
        //       linkedHashMap - elements are stored in entry-sequence
        // 
        // To Define: Map<key-type, value-type> name = new type-of-map<key-type, value-type>();
        //
        // .put(key,value) - add elements to the Map
        // .get(key) - retrieve and element based on it's key (value is returned)

        // Define a map to hold someone's favorite number
        //
        //     key - name (no duplicate names)
        //     value - favorite number

        // Since Maps can only hold objects, Java gives use Wrapper Classes
        //       to represent primitives:
        //
        //   Integer, Double, Float, Boolean, Char
    
        // Define a Map
        Map<String, Integer> favNumberMap = new LinkedHashMap<String, Integer>();

        // Add some data to the Map
        favNumberMap.put("Ethan", 3);
        favNumberMap.put("Evan", 77);
        favNumberMap.put("Josh", 47);
        favNumberMap.put("Ashley", 19);
        favNumberMap.put("Kendall", 16);
        favNumberMap.put("Jay", 3);
        favNumberMap.put("Frank", 42);

        System.out.println("Josh favorite number:" + favNumberMap.get("Josh"));
        System.out.println("Evan favorite number:" + favNumberMap.get("Evan"));
        System.out.println("Jay favorite number:" + favNumberMap.get("Josh"));

        // Display all the entries in the Map
        //
        // We need to get all the keys in the Map
        // Iterate through the keys one at a time retrieving their value

        // .keySet() will return all the keys in a Map as a Set object
        //
        // A Set is like an ArrayList that doesn't allow duplicate values

        // Define a Set to hold the keys from the Map
        Set<String> theKeys = favNumberMap.keySet();

        // Loop through the list of keys and process each element
        for(String aKey : theKeys) {
            System.out.println(aKey+"'s favorite number:" + favNumberMap.get(aKey));
        }




    }
}
