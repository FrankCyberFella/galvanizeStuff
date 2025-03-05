--------------------------------------------------------------------------------------------------------
-- Ordering, Grouping Basic Functions 
--------------------------------------------------------------------------------------------------------
--
-- The order of the rows in a result is unpredictable unless you code an ORDER BY
-- You can run the same SELECT without an ORDER BY 1000 times and get results in the same order each time
-- but there is guarantee the order will be the same the 1001st time
--
-- IF YOU CARE ABOUT THE ORDER OF THE ROWS IN A RESULT CODE AN ORDER BY
--
-- ORDER BY is always coded last in standard SQL
--
-- ORDER BY -  Sequence of Rows in Result
--
--    ORDER BY          -- Ascending Sequence (low-high)
--    ORDER BY ASC      -- Ascending Sequence (low-high)
--    ORDER BY DESC     -- Descending Sequence (high-low)

-- List gamblers in order of salary from high to low
select *
  from gambler
order by monthly_salary desc

-- List gamblers in order of salary from low to high
select *
  from gambler
order by monthly_salary  -- coding ASC is an option, but is the default

-- List gamblers in order of salary from low to high
-- Only show gamblers whose salary is more that 5000
select *
  from gambler
 where monthly_salary > 5000
order by monthly_salary  -- coding ASC is an option, but is the default

-- List any gambler whose address is unknown (null)
select *
  from gambler
where address is null -- cannot use = or != when filtering nulls
                      -- Use IS NULL to see if it's null
					  --     IS NOT NULL to see if it is not null

-- List any gambler whose address is known (not null)
select *
  from gambler
where address is not null -- cannot use = or != when filtering nulls
                          -- Use IS NULL to see if it's null
					      --     IS NOT NULL to see if it is not null

-- List the casino in location and size sequence with the biggest listed first for the location
select *
  from casino
order by location, size desc -- to sort by multiple columns, code them separated by commas

--------------------------------------------------------------------------------------------------------
-- Limiting the number of rows in the result
--
-- TOP n is proprietary to SQL Server - it may not work in other dialects of SQL for other data base managers
--
-- TOP n   - Limit the number of rows in the result - always goes after the SELECT - before the first column 
--

-- List the two biggest casinos
select top 2  *  -- when using Top an order by is usually needed to sort the rows by why top thing you want
  from casino
order by size desc

-- List the two smallest casinos
select top 2  *  -- when using Top an order by is usually needed to sort the rows by why top thing you want
  from casino
order by size asc

--------------------------------------------------------------------------------------------------------
-- Concatenating values (like + in C# and Java with Strings)
--
-- the concat operator (+) may be used to concatenate character (string) values in a result
-- (both sides of the + must be strings)
--

-- Show the casino and it's location in ONE column with a comma between them
select casino_name + 'is in ' + location
from casino

-- Show the casino and it's location in ONE column with a comma between them
--      and no spaces after the casino_name
--
-- trim(string-column) will remove leading an training space from the string
select trim(casino_name) + ' is in ' + location
from casino

--------------------------------------------------------------------------------------------------------
-- Aggregate functions - produce one row in result for each group specified no matter how many rows are in the result
--                       rather than on row in the result for each row that satisifies the WHERE clause
--
-- Aggregate functions are used to produce a single value from a group of rows in a result
--
-- The group used by the aggregate functions is determined by the GROUP BY clause
-- if no GROUP BY clause is specified, the group is the set of rows in the result
--
--     AVG(column-expression)   - arithmentic average for group of non-NULL values in expression 
--     SUM(column-expression)   - arithmentic sum for group of a non-NULL values in expression 
--     MIN(column-expression)   - lowest value for group of non-NULL values in expression 
--     MAX(column-expression)   - highest value for group of non-NULL values in expression 
--     COUNT(*)                 - number of rows in the group
--     COUNT(column-expression) - number of rows for the group of non-NULL values in expression 
--
--  if the requirement says "number of" - count(*)
--  if the requirement says "number of without nulls" in a column - count(column-name)
--  if the requirement says "largest / biggest" - Max() or order by desc and Top
--  if the requirement says "smallest / least" - Min() or order by asc and Top 
--  if the requirement says "total or sum" - sum()
--  if the requirement says "average" - avg() 
--  if the requirement says "for each" or "by each" - GROUP BY
--
-- AVG(), SUM() may only be used with numeric data types
-- MIN(), MAX() may be used with numeric, character, date, time datatypes
--
-- COUNT() is applied to rows in the result (not columns)
--
--

-- How many gamblers are in the gambler table
select count(*)  as 'Number Of Gamblers' --  "How many" or "number" of means count(*)
  from gambler

-- How many gamblers do we have address for
select count(*)  as 'Number Of Gamblers' --  "How many" or "number" of means count(*)
      ,count(address) as 'Have Addresses'
  from gambler

-- What are the highest, lowest and average salaries of all gamblers
select max(monthly_salary) as 'Highest'
      ,min(monthly_salary) as 'Lowest'
	  ,avg(monthly_salary) as 'Average'
 from gambler

-- What are the highest, lowest and average sizes of all casinos
select max(size) as 'Highest'
      ,min(size) as 'Lowest'
	  ,avg(size) as 'Average'
 from casino
 -- Since there no where clause all rows in the table are processed by the aggregate functions

 -- What are the highest, lowest and average sizes of all non-vegas casinos
select max(size) as 'Highest'
      ,min(size) as 'Lowest'
	  ,avg(size) as 'Average'
 from casino
 where location != 'Las Vegas' -- limit the group of rows the aggregate functions process

--------------------------------------------------------------------------------------------------------
-- GROUP BY  - Specify the group to which the aggregate functions apply
--
--      GROUP BY column-expression
--
-- When using a GROUP BY the SELECT is limited to aggreggate functions or columns in the GROUP BY
--
--    GROUP BY    - generate one row for each unique value in a column
--    column-name - generate one row for every row in the table
--
--    if you add a column-name in teh SELECT that is not in the GROUP BY
--       you are requesting conflicting results: one row per unique value vs one row for row in the table
--
--    When SQL is asked to do something it doesn't understand it generates error
--
-- if requirement says 'by each', 'in each' or 'for each' that's a GROUP OF 
--
--
-- What are the highest, lowest and average sizes of each location
--   include the number of casinos in each location
select location   -- add the group by column te the result to make it more easily read
      ,count(*)  as '# of Casinos'
	  ,max(size) as 'Highest'
      ,min(size) as 'Lowest'
	  ,avg(size) as 'Average'
 from casino
 group by location -- produce one row for unique location

 -- What are the highest, lowest and average sizes of each location
--   include the number of casinos in each location and teh owner
 select location   -- add the group by column te the result to make it more easily read
 --     ,owner  -- causes and error because owner is not included in the GROUP BY
      ,count(*)  as '# of Casinos'
	  ,max(size) as 'Highest'
      ,min(size) as 'Lowest'
	  ,avg(size) as 'Average'
 from casino
 group by location -- produce one row for unique location
--------------------------------------------------------------------------------------------------------
-- SUBQUERIES - Using the result from one query (inner query) in another query (outer query)
--
-- Frequently used in a WHERE clause with an IN predicate:
--
--       WHERE column-name IN (SELECT column-name FROM some-table WHERE some-predicate)
--
-- Any WHERE predicate may be used to connect the subquery in a WHERE clause, but you must
-- be sure a single value is returned from the subquery. 
--
-- Subqueries may also be used in a SELECT as a column-specification or a FROM as a table
-- (These are advanced concepts we will discuss later, if there is time)
--
