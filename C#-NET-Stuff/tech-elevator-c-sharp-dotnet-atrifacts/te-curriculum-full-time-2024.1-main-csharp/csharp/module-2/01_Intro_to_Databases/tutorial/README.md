# Intro to databases tutorial

In this tutorial, you'll learn how to:

* Write SQL statements to retrieve data from a Microsoft SQL Server (MSSQL) database
* Sort data with the `ORDER BY` clause
* Filter data with the `WHERE` clause
* Restrict the number of results with the `TOP` clause

You'll use a database that a local pizza shop might use to run their business. The `PizzaShop` database has tables that represent pizzas, toppings, sales, and customers.

**Before** you get started with this tutorial, you must setup the `PizzaShop` database if you haven't already. The "Database setup" lesson in the Intro to Tools unit for Microsoft SQL Server shows you how to do this.

You'll write your statements in **SQL Server Management Studio (SSMS)**. SSMS connects to a MSSQL database server so you can run SQL statements and perform other database-related functions. Refer to the unit about MSSQL in the Intro to Tools section of this course for details on getting started with Microsoft SQL Server and SSMS.

## Getting started

In SSMS, connect to your local MSSQL instance (usually `.\SQLEXPRESS` or `.`) and expand the **Databases** node by double-clicking it or clicking the **+** on its left side. Locate the "PizzaShop" database in the list, and double-click it. This establishes "PizzaShop" as the database for your statements to run on.

Now, click the **New Query** button to open the **Query Window** which allows you to enter SQL statements—also known as queries:

![New query button](./img/new-query-button.png)

> Note: there's a drop-down on the toolbar which shows the _active database_. That's the database on which the queries you type into the Query Window execute. If this drop-down doesn't show "PizzaShop", find that database in the list and select it.
>
> ![Active database](./img/current-database.png)

All queries in this tutorial are also in the `.sql` files in the `tutorial-final` folder. If you encounter an issue or want to run the pre-typed queries, you can open these files in SSMS by using the **Open File** button on the toolbar:

![Open file button](./img/open-file.png)

After you open a file, remember to check the _active database_ drop-down to verify that "PizzaShop" is still your active database.

## Part One: Selecting data from tables

To retrieve data from a table, you use a `SELECT` statement. A `SELECT` statement consists of the names of the columns you want returned and the table they come from.

### Size table

The `size` table represents the different sizes of pizza that you can order. The table has four columns: `size_id`, `size_description`, `diameter`, and `base_price`. To get the data from the `size` table, write this statement in the **Query Editor** of SSMS:

```sql
SELECT size_id, size_description, diameter, base_price
    FROM size;
```

Run the statement by pressing the `F5` key on your keyboard, or clicking the **Execute** button in the toolbar:

!["Execute" button](./img/execute-button.png)

In the bottom half of the SSMS window under "Results", you'll see the data from the `size` table. The `size_id` is a single character that represents the size—you'll see the significance of this later. The `size_description` column contains the size spelled out. The `diameter` column is the diameter in inches of each size. The `base_price` column has numeric values, representing the price that the pizza shop charges for a plain pizza or before adding toppings.

### Topping table

Now, you'll write a statement to retrieve the data from the `topping` table.

You can leave the `topping` statement if you want. By default, SSMS executes all the statements in the **Query Window** and displays the results. If you highlight a statement before running it, SSMS executes only that statement instead. You can also open a new **Query Window** by clicking the **New Query** button again.

The `topping` table has two columns: `topping_name` and `additional_price`. Write and run this statement to get these columns from the `topping` table:

```sql
SELECT topping_name, additional_price
    FROM topping;
```

In the bottom half of the SSMS window, you'll now see the data from the `topping` table. The `topping_name` column has values like "Pepperoni" and "Mushrooms"—toppings that you might order on a pizza. The `additional_price` column is like the one you saw in the `size` table, but this is the price the pizza shop charges to add the topping to a pizza.

Notice there doesn't seem to be any order to the results, neither alphabetically or numerically. It's always a good practice to include an `ORDER BY` clause in a `SELECT` statement so that you're guaranteed the order you want. Update the statement you just wrote to include an `ORDER BY` clause:

```sql
SELECT topping_name, additional_price
    FROM topping
    ORDER BY additional_price;
```

Now the toppings are in numerical order by price. "Extra Cheese" is at the top because it has the lowest price, and "Chicken" is at the bottom because it has the highest price.

However, the toppings that are the same price don't appear in any particular order. You may want the toppings sorted alphabetically when they're the same price. You can do so by specifying multiple columns in the `ORDER BY` clause:

```sql
SELECT topping_name, additional_price
    FROM topping
    ORDER BY additional_price, topping_name;
```

Now the toppings that are $0.75 start with "Black Olives" and end with "Tomatoes" before the toppings of the next higher price.

## Part Two: Filtering data using WHERE

Retrieving all the data from a table isn't always what you want, especially if you're looking for rows that match a certain value or condition. That's where the `WHERE` clause comes in.

The `pizza` table represents pizzas that customers have ordered. Write and run this statement to retrieve the data from the `pizza` table:

```sql
SELECT pizza_id, sale_id, size_id, crust, sauce, price, additional_instructions
    FROM pizza
    ORDER BY pizza_id;
```

Notice that pizzas have a size, which you see represented by the same `size_id` you saw when you ran the `SELECT` statement on the `size` table. There's a relationship between the `pizza` table and the `size` table. You'll learn more about relationships between tables in a later lesson.

Pizzas also have a crust type, amount of sauce desired, and a price that includes toppings. There's also optional "additional instructions" that a customer can request, such as asking to cut the pizza into squares.

Say you want to return only the small pizzas that customers have ordered. You can use a `WHERE` clause to filter the data down to the ones you're interested in.

Modify the `pizza` statement you wrote to have a `WHERE` clause that gets only small pizzas:

```sql
SELECT pizza_id, sale_id, size_id, crust, sauce, price, additional_instructions
    FROM pizza
    WHERE size_id = 'S'
    ORDER BY pizza_id;
```

Note that you add the `WHERE` clause between the `FROM` and `ORDER BY` clause. The order of SQL clauses is important. If it isn't correct, you'll encounter an error.

When you run the modified statement, you'll see that the rows in the **Results** section are only for small pizzas.

### Multiple conditions

If you also wanted to know which small pizzas were also thin crust, you could combine these conditions with `AND`:

```sql
SELECT pizza_id, sale_id, size_id, crust, sauce, price, additional_instructions
    FROM pizza
    WHERE size_id = 'S'
    AND crust = 'Thin'
    ORDER BY pizza_id;
```

Now if you run the statement, you only get the small thin crust pizzas.

You can also use `OR` to say that you want one condition, or the other, to be true. For example, if you change the `AND` to `OR` in the previous statement, you'll get all small pizzas regardless of crust, and non-small pizzas that have thin crust:

```sql
SELECT pizza_id, sale_id, size_id, crust, sauce, price, additional_instructions
    FROM pizza
    WHERE size_id = 'S'
    OR crust = 'Thin'
    ORDER BY pizza_id;
```

### Not equal

`WHERE` clauses aren't only used for retrieving values that match a certain value. You can also use them to filter out a specific value.

If you wanted to write a statement that returned everything *except* small pizzas, you can write the query like this:

```sql
SELECT pizza_id, sale_id, size_id, crust, sauce, price, additional_instructions
    FROM pizza
    WHERE size_id != 'S'
    ORDER BY pizza_id;
```

When you run this statement, you'll get all medium and large pizzas.

### Greater than or less than

You don't have to test for a value being equal to another. You can also test for values that are greater than or less than a certain value.

To retrieve all pizzas that cost more than $10, you can change your `WHERE` clause to `price > 10`:

```sql
SELECT pizza_id, sale_id, size_id, crust, sauce, price, additional_instructions
    FROM pizza
    WHERE price > 10
    ORDER BY pizza_id;
```

Now try changing the `ORDER BY` clause to something more interesting, like `ORDER BY price` to see pizzas from lowest price to highest:

```sql
SELECT pizza_id, sale_id, size_id, crust, sauce, price, additional_instructions
    FROM pizza
    WHERE price > 10
    ORDER BY price;
```

To get pizzas that cost less than $10, you can change it to this:

```sql
SELECT pizza_id, sale_id, size_id, crust, sauce, price, additional_instructions
    FROM pizza
    WHERE price < 10
    ORDER BY price;
```

To get values that are greater/less than *or* equal, you can use `>=` or `<=`:

```sql
SELECT pizza_id, sale_id, size_id, crust, sauce, price, additional_instructions
    FROM pizza
    WHERE price <= 10.99
    ORDER BY price;
```

Try running these statements and see how the results change.

### Boolean values

MSSQL doesn't have a "boolean" data type. A true or false value is usually represented in MSSQL using `bit` column. A bit is a single binary digit, meaning it can equal either zero (0) or one (1). To test for true, you check that the column value equals 1. To test for false, check that the column value equals 0.

Run this query, which selects all columns from the `sale` table and sorts it by id:

```sql
SELECT sale_id, total, is_delivery, customer_id
    FROM sale
    ORDER BY sale_id;
```

> Note: The id column is a good choice to `ORDER BY` when there isn't a requirement to sort by anything specific.

The `is_delivery` column is a `bit` value, indicating if the sale was a delivery (`1`) or not (`0`).

To get all sales that were delivery, you can use the `WHERE` clause to test for that value:

```sql
SELECT sale_id, total, is_delivery, customer_id
    FROM sale
    WHERE is_delivery = 1
    ORDER BY sale_id;
```

### Null values

Testing for null values is a bit different, though you still use a `WHERE` clause.

Looking at the `sale` table again, the `customer_id` column represents the customer that ordered a given sale. Some of the values are a number, and some are `NULL`. The sales with a null `customer_id` are walk-in orders where the customer didn't provide their info. The pizza shop doesn't need your name and address if you walked in to place an order for takeout.

To get sales that don't have an associated customer record, you can get the rows that have a null value by writing your statement like this:

```sql
SELECT sale_id, total, is_delivery, customer_id
    FROM sale
    WHERE customer_id IS NULL
    ORDER BY sale_id;
```

When you run that, you'll see all the walk-in orders with no customer record.

Conversely, if you want all the sales that *do* have a customer record, you can change it to `IS NOT NULL`:

```sql
SELECT sale_id, total, is_delivery, customer_id
    FROM sale
    WHERE customer_id IS NOT NULL
    ORDER BY sale_id;
```

## Part 3: Restricting rows

The `TOP` clause lets you specify how many rows you want. When you pair it with an `ORDER BY` clause, you can find the highest or lowest _something_ from the database.

To find the biggest sale, select all the rows from the `sale` table, order by `total` in descending order, and retrieve the `TOP` 1 row:

```sql
SELECT TOP 1 sale_id, total, is_delivery, customer_id
    FROM sale
    ORDER BY total DESC;
```

## Next steps

You don't always have to retrieve all the columns for a particular table. You can `SELECT` just the ones you want. Try removing one or two columns from one of the SQL statements in this tutorial, and see how the results change.

If you don't know the names of the columns, or you want a quick way to get all of them, you can write `SELECT * FROM ...` instead. The `*` means "return all columns." This is helpful for a quick query, but it's a best practice to always name the columns you want. You'll learn why that's important in a later lesson.

Explore the `customer` table by writing your own `SELECT` statements. Can you get all customers in a particular town? What about customers who haven't provided an email address and phone number?
