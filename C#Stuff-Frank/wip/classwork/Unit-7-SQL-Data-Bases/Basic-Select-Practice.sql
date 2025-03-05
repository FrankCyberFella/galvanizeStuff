-- Practice Basic SQL
--
-- List all entries in the casino table
select *      -- select * - get all in the order they are defined
  from casino -- table with rows we want   

-- Show the location and name each casino
select location, casino_name  -- get these columns in this order
  from casino


-- List all Vegas casinos
select casino_name, 'is in Las Vegas' -- columns to include in the result
  from casino                 -- table with the columns
 where location = 'Las Vegas' -- which rows to include in the result

-- List all casinos whose size is more than 30000
select casino_name, size -- columns include in the result
  from casino            -- table with the columns
where size > 30000       -- rows to include in the result


-- Show the size of Vegas casinos if we double the size
-- Arithmetic expressions may be included in the select
select casino_name
     , size
	 , size * 2 as 'Double Size'  -- columns to include in the result
  from casino                     -- table with the columns
 where location = 'Las Vegas'     -- which rows to include in the result

-- List Gamblers with oldest ones first (earliest birth_date)
-- 
-- If the order of rows in teh result matter - code order by to sort them
-- ASC  - ascending (low to high) order is default
-- DESC - descending order
select *
  from gambler
order by birth_date desc