# Introduction to databases

## Overview of session

In this lecture, you'll introduce the concept of relational databases, and show the students how to write simple SQL queries.

## Session objectives

- Understand what a database is and why it's useful
- Understand what SQL is and how it's used
- Have a basic proficiency with the tools specific to the DBMS used
- Be able to write simple queries that retrieve data

## Instructor notes

1. **Problem Statement**

    - Databases are everywhere, from the largest enterprises that manage employee resources and financial records, to smaller websites that manage customers and members.

    - What locations can you think of that might store or house massive amounts of data?

      - **Netflix, Hulu, YouTube** to track user's history and show preferences
      - **Online games** to track individual player history
      - **Sports teams** track player analytics
      - **Social media** to provide recommended businesses, products, friends, etc.

2. **Database Terminology**

    - A **database** is an organized collection of data that can be accessed, managed, and updated.

    - Data in a **relational database** can be accessed and reassembled in many different ways without having to reorganize the data.

    - A database table represents a data **entity**.

    - The table columns are **attributes**.

    - **Rows** represent individual records.

    - **RDBMS** (Relational Database Management System) software helps with database management. Its four basic functions are:

        1. Data definition
        2. Data storage
        3. Data retrieval
        4. Administration

3. **Popular RDBMSs**

    - Oracle, Microsoft SQL Server, PostgreSQL, IBM Db2, MySQL, SQLite

4. **Why use databases?**

    - easily support storing large number of records
    - central storage
    - supports structured query syntax to retrieve data
    - enforces consistency and integrity of data
    - guarantee data type

5. **Introduce tools and setup database**

    - [C#/Microsoft SQL Server](./mssql-notes.md)

6. **What is SQL?**

    - **S**tructured **Q**uery **L**anguage
    - A _declarative_ programming language (rather than _imperative_ like Java and C#)
    - **ANSI SQL** refers to the standardized form of the language, but each RDBMS typically includes its own proprietary version that offers additional features and alternative syntax.

7. **Writing queries to retrieve data**

    - Open the `lecture-queries.sql` file and point out that currently all the lines begin with `--` which makes them comments.
    - Work through the file, writing queries for each prompt to demonstrate the use of the following keywords and operators:
      - `SELECT`
      - `FROM`
      - `AS`
      - `ORDER BY` (see [additional notes on ordering](#ordering-results))
      - `TOP`
      - `WHERE`
      - `=`, `>`, `<`, `>=`, `<=`, `<>` (or `!=`)
      - `IN`
      - `AND`, `OR`
      - `IS NULL`, `IS NOT NULL`
      - `DISTINCT`
      - `LIKE` (with `%` wildcards)
      - `BETWEEN __ AND __`
    - If there's still time after going through the file, consider demonstrating more complex `WHERE` clauses by combining earlier queries or asking the class for suggestions.
    - Emphasize that the clauses of a SQL statement need to come in a certain order, and that it's a good idea to end each complete statement with a semicolon.
    - Be aware that the `state` table of the `UnitedStates` database includes states, territories, and the District of Columbia. Point this out so students aren't confused about why there are more than 50 records.

    ### **Ordering results**

    To sort a result set, you can use the `ORDER BY` clause:

        ```sql
        SELECT col1, col2
            FROM tablename
            WHERE col1 = 'value'
            ORDER BY col1 [ASC | DESC], col2 [ASC | DESC]
        ```
    You can:
    - Sort using columns that aren't included in the result set.
    - Use aliased columns for sorting.
    - Create a *priority* sort by listing multiple column names. In other words, the first column in the `ORDER BY` clause is the *major* or *primary* sort. All other columns are *minor* or *secondary* sorts in order from left to right.

## Instructor resources

You can find an ERD of the `UnitedStates` database and a map of United States Census regions in the student `lecture` folder for use as visual aids during lecture and for student reference.
