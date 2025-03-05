# Subqueries and aggregation

## Problem statement

Filtered, but otherwise unorganized information is only so useful. Grouping and ordering data can make information *understandable*.

Database functions to count, sum, average, and otherwise massage the data can further enrich the information.

## Lesson objectives

* String operations functions
* Subqueries
* Aggregate functions
* Grouping results

## Notes and examples

### **String operations**

- You can concatenate the values across multiple columns (or literals) into a single field.
    - Use the `+` operator to concatenate strings. `SELECT (column1 + ' ' + column2) FROM table`

### **Subqueries**

A **subquery**, or _inner_ query, can provide the results of one query as input to another.

- Often used in the `WHERE` clause
- Can only return one item in the `SELECT` clause

	```sql
	SELECT column_name [, column_name]
        FROM table1 [, table2]
        WHERE column_name (IN | NOT IN)
            (SELECT column_name FROM table [WHERE])
	```

### **Aggregate functions**

* **`AVG`** returns the average value of a numeric column
* **`SUM`**  returns the total sum of a numeric column
* **`COUNT`** returns the number of rows matching criteria
* **`MIN`** returns the smallest value of the selected column
* **`MAX`** returns the largest value of the selected column

### **Grouping results**

`GROUP BY` statements group records into summary rows and return one record for each group.

- Grouping data is the process of combining columns with duplicate values.
    - For example, a database may contain information about employees. Many employees may live in different cities. Suppose you wanted to figure the average salary paid to employees within each city. You would use the aggregate function `AVG` for the salary, and `GROUP BY` the city.
    - You can use the `GROUP BY` clause in conjunction with aggregate functions to collect data across multiple records.
    - The signature of the statement follows:

    ```sql
    SELECT expression1, expression2, ... expression_n,
        aggregate_function (aggregate_expression)
    FROM table
    [WHERE condition_expression]
    GROUP BY expression1, expression2, ... expression_n
    ORDER BY order_column;
    ```
