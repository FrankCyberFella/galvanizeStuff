# SQL Injection demo walkthrough

The SQL injection demo code is similar to the U.S. Cities and Parks application that you used for the main lecture. It's pared down to just a State DAO with two implementations of the "get by abbreviation" method—one using a SQL parameter, one using string concatenation. The State DAO also has a method that returns multiple records, it also uses string concatenation to allow a different type of SQL injection attack.

A Park DAO is also supplied with one method providing the number of parks in the database. One of the things you'll do during the demonstration is to delete all parks from the table, the park count can verify the deletion.

A separate SQL script is also provided in the `database` folder to create the database without the `city` and `park_state` tables, so there are no foreign key constraints. It's _highly recommended_ to create the database using a different name, such as `SqlInjectionDemo`. The demo SQL script and code are already written to use that name.

## Demonstration

You don't need to walk through much of the application code besides the `StateSqlDao` class. There are three DAO methods—`GetStateByAbbreviation()`, `GetStateByAbbreviationConcatenation()`, and `GetStatesByName()`. Show the students the methods and note that the two `GetStateByAbbreviation` methods are identical except one uses string concatenation. The `GetStatesByName()` also uses string concatenation to retrieve more information. The application has menu options to call each method:

```
-----------------------------------------
|      SQL Injection Demonstration      |
-----------------------------------------
1. Get State By Abbreviation (parameter)
2. Get State By Abbreviation (string concatenation)
3. Get States By Name (string concatenation)
4. Get Park Count
5. Exit
```

Start with using option `4. Get Park Count`. This option calls the Park DAO method `GetParkCount()` that returns the count of rows in the `park` table. Confirm that you receive the output `There are 63 parks`.

Next, show the two "Get State By Abbreviation" behave the same—prompt for a state abbreviation and display the state's name. Try a couple of states with each option to verify they work.

After demonstrating the happy path, use the string concatenation option and enter `'; DELETE FROM park;--`. Nothing seems to happen, right? Go to option `4. Get Park Count` again and you're informed `There are 0 parks`.

Restore the database data. Get the park count again and verify it's 63 again. Try to run the same `'; DELETE FROM park;--` input with the menu option that uses a parameter. You get a message that `State is null` and if you get the park count again, it's still 63.

## How did that happen?

Take the string from the concatenation method and write it on the board or in a text editor:

```csharp
"SELECT state_abbreviation, state_name FROM state WHERE state_abbreviation = '" + stateAbbreviation + "';";
```

When the method received `'; DELETE FROM park;--` as the `stateAbbreviation`, it concatenated them together and created:

```sql
SELECT state_abbreviation, state_name FROM state WHERE state_abbreviation = ''; DELETE FROM park;--';
```

The original SQL query ended by closing the string with a single quote and a semi-colon. It then runs another statement—`DELETE FROM park;`. To make sure there was nothing else in the original query, the attacker comments out the rest of it with `--`.

Parameters prevent that from happening because of how the database handles them. It doesn't concatenate them, but is like a variable declared and replaced with the appropriate value. Point out the parameter version doesn't have single quotes around the question mark:

```csharp
"SELECT state_abbreviation, state_name FROM state WHERE state_abbreviation = @state_abbreviation;"
```

The database server takes the value of the parameter and safely wraps it to become:

```sql
SELECT state_abbreviation, state_name FROM state WHERE state_abbreviation = '''; DELETE FROM park;--'
```

This then behaves like a normal query which returns zero results since no states match that abbreviation.

## UNION injection attack

If your students are still interested, you can show a "UNION injection" to read data that a user shouldn't be able to read.

The `3. Get States By Name (string concatenation)` option calls a DAO method that returns multiple rows. It performs like a search method, with wildcards automatically added instead of using a boolean like the reading to keep it simple. This type of method is useful for retrieving more data from the database. There are many ways to do this, but you can demonstrate a "UNION injection" attack with this one.

> Note: the students likely haven't seen `UNION` before, but you can explain it just combines two queries together in the same result set.

Perform a normal search like "New" or "Carolina" to show the method works. Next, enter `' UNION SELECT '', park_name FROM park--` for the search term. The query not only returned _all_ states, but also all the park names. This works because the columns in the second query take on the column names of the first query, so the `MapRowToState()` method still maps the park names to the `StateName` field of the `State` object.

> The `''` for the first column is necessary because the data types have to match, and the `park` table doesn't have another `varchar` column.

This is a simple example but shows what's possible. Attacks like this allow someone to steal or read data they shouldn't have access to.

## Taking it further

You can be a little more destructive and do `'; DELETE FROM state;--` or `'; DROP TABLE park;--`.

There's also an online interactive demo that you can show to students or provide them with the link to watch later: https://www.hacksplaining.com/exercises/sql-injection
