# Database Design


## Problem statement

Making use of an existing database is one thing, making one from scratch is another. There's more to creating a database than declaring some tables and filling them up with data.

## Lecture objectives

  - Database Design Exercise
  - Database Definition Language
  - Database Control Language


## Notes and examples

### Normalization

- This design exercise is intended to be completed as a class.
- Go through and design the tables that would support the attached report image.
- Allowing students 10-15 minutes to see what they come up with.


![Hypothetical Example](resources/normalization-exercise.jpg)

[Design of Database](https://drive.google.com/open?id=10hS57mOAzeoI8vZpcHrt47mtdrxaHG2698pa1qaUSWk)

<div class="caution note">

Databases don't get designed one normal form at a time. In the past we've tried this and it has felt awkward. Instead encourage them to full design and they'll probably end up with something resembling 3NF.</div>

> NOTE: For the following example work through the art gallery example and then show the art gallery SQL.

#### **1st Normal Form Example**

Consider the above unnormalized database

- This table is not in first normal form because the purchases column can contain multiple values.
  - How would we query for this type of data?

<div class="definition note"><strong>Normalization</strong> is the process of organizing a database to reduce data redundancy and improve data integrity.</div>

<div class="definition note"><strong>In 1NF</strong>, no table should contain duplicative columns that one could use to get other types of information. Every table should be organized in rows with primary keys that uniquely identify it.</div>


**How to Pick Keys**
- What happens if more than one customer buys the same piece?
- What happens if a customer buys the same piece of art more than once?


#### **2nd Normal Form Example**

Database in 1NF includes customer purchase data. Some data elements in this table are not entirely dependent on the primary key (e.g. Title is dependent on ArtCode but not CustomerId or PurchaseDate).

<div class="definition note"><strong>In 2NF</strong> the table must be in 1NF and any non-key attribute must be dependent on the primary key.</div>

#### **3rd Normal Form Example**

<div class="definition note"><strong>In 3NF</strong> the table must be in 2NF and there is no transitive functional dependency.</div>

- This means that if A is dependent on B and B is dependent on C therefore C is transitively dependent on A.

<div class="analogy note">The difference between 2NF and 3NF is that in 2NF a non-key column must depend on the whole primary key and in 3NF it depends on nothing outside the primary key.</div>

> NOTE: As a second example, have the students think about an address book. Talk about the different aspects, like a contact having multiple phone numbers or email addresses. Show the address book SQL after to see what was the same/different.

### Database Definition Language

- Just like DML is used to manipulate and query data, DDL is used to create and define database structures.

<div class="definition note">DDL consists of the set of commands used to define tables, indexes, keys, and other metadata that comprise a database schema.</div>

<div class="definition note">

A **schema** is a skeleton structure of the database that focuses on the tables and constraints, not the data.</div>

1. **CREATE/DROP DATABASE**

   ```
   CREATE DATABASE database_name;
   DROP DATABASE database_name;
   ```

2. **CREATE TABLE**

  - The **Create Table** statement requires the columns, their data types. Constraints, Primary Keys, and Foreign Keys are typically included but can be added later.

    ```
    CREATE TABLE table_name
    (
      column_name1 data_type(size),
      column_name2 data_type(size) NOT NULL,
      column_name3 data_type(size),
      CONSTRAINT pk_column_1 PRIMARY KEY (column_name1),
      CONSTRAINT fk_column_2 FOREIGN KEY (column_name2) REFERENCES table_name(column_1)
    );
    ```

    - The column name specifies the exact name of the column.
    - The data type indicates what it holds.
    - The side specifies the maximum length of the column
    - For *MS SQL* a column can be identified as an IDENTITY column.

3. **DROP TABLE**

    ```
    DROP TABLE table_name
    ```

4. **ALTER TABLE**

    - The alter table command modifies a table definition by altering it, adding, or dropping columns and constraints on an existing table.

    ```
    ALTER TABLE table_name
    ADD CONSTRAINT pk_constraint_name PRIMARY KEY (column_name(s));

    ALTER TABLE table_name
    ADD CONSTRAINT fk_constraint_name FOREIGN KEY (column_name) REFERENCES table(column_name);

    ALTER TABLE table_name
    ADD CONSTRAINT chk_constraint_name CHECK (column_name = 'value' OR column_name IN (values));
    ```

### Database Control Language

- Database commands are executed under the context of an *authenticated user*.
- Database control language commands are used to create users, drop users, grant privileges and revoke privileges.
- The scope does not cover DCL in depth, but **explain what grant and revoke means and why it's important.**




## Reference


* [Joe Celko on Sequences](https://www.simple-talk.com/sql/learn-sql-server/sql-server-sequence-basics)
