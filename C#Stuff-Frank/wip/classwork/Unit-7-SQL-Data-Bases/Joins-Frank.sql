--
-- This SQL uses the vegas database 
--
-- show the gambler_name and casinos and type of room they stay in when are hosted
--
-- we need data from gambler table (gambler_name) 
--                   host table (casino_name, room_type)
--
-- Whenever data is required in the result from more than one table use a join (inner join)
--
-- A inner join will return matching rows between tables
--
-- There must be a common value that can be used to match data between the tables
--    the names of the columns don't have to match, but content must match
--
-- if columns names are not unique between the tables, put tablename. front of the column name
--
-- The join-condition is what matches between the tables
--
-- The order of the tables for the join doesn't matter - data base managers figures out what to do
--
-- There are two formats for coding a join:
--
--      1. Classic syntax - code all tables for the join on the FROM
--                               and the join condition on the WHERE
--                          only an INNER JOIN may be done
--                          Works if you forget the join condition(s) - not correctly (not a join) but it runs
--                          if you forget the join condition result is a Cartersian Product
--                             (each row from one table is produced for every row in the other)
--                             (the number of rows in the results = #-rows-in-one-table * #-rows-in-other)
--                             (if you try to join a 1000 row table to a 1000 row table without
--                                 a join condition you will get 1000000 rows in the result)
--                             (since join is used to show matching data between tables, a Cartersian product is NOT a join)
--
--      2. Modern syntax - code the type of join bewteen the tables instead of a comma
--                              and the join condition is coded on an ON statement not WHERE
--                         any type of join may be done
--                         error if you forget the join condition(s)
--
--
----------------------------------------------------------------------------------------
-- Classic Syntax join
----------------------------------------------------------------------------------------
select gambler_name, casino_name, room_type -- Columns to be included in the result
  from gambler, host                        -- table(s) with the columns you want in the result
 where gambler.id = host.id -- code the where to match rows between the tables
                            -- if columns names are same, code the name table that has the column

----------------------------------------------------------------------------------------
-- Modern Syntax join
----------------------------------------------------------------------------------------                                            
select gambler_name, casino_name, room_type -- Columns to be included in the result
  from gambler    -- code one table on the from
       inner join -- type of join
	   host       -- code another table
       on gambler.id = host.id -- join condition coded in the ON phrase
                   
-- show the gambler_name and casinos, location and type of room they stay in vegas when are hosted
--
-- we need data from gambler table (gambler_name) 
--                   host table (casino_name, room_type) 
--                   casino table (location)

-- CLassic Syntax
-- if columns names are same, code the name table that has the column
-- (this is called qualifyng the column - teling SQL what table the column comes from)

select gambler_name, casino.casino_name, location, room_type -- Columns to be included in the result
  from gambler, host, casino                -- table(s) with the columns you want in the result
 where gambler.id = host.id -- code the where to match rows between the tables
   and host.casino_name = casino.casino_name -- code the join condition to match rows between the tables
                            -- if columns names are same, code the name table that has the column   
   and location = 'Las Vegas' -- filtering condition							

-- Modern syntax
select gambler_name, host.casino_name, location, room_type -- Columns to be included in the result
  from gambler    -- code one table on the from
       inner join -- type of join
	   host       -- code another table
	   on gambler.id = host.id -- join condition coded in the ON phrase
	   inner join -- type of join
	   casino     -- code another table
       on casino.casino_name = host.casino_name -- join condition coded in the ON phrase
   where location = 'Las Vegas' -- filtering condition
                                             
-- a Correlation name (nick name) may be assigned to a table to make it easier reference in the SQL
-- 
-- To assign a correleation name, just code it after the table name in the from
-- Then use it place of table name when needed
--
-- Correlation names are usual very short

select gambler_name, h.casino_name, location, room_type -- Columns to be included in the result
  from gambler g   -- code one table on the from with a correation name
       inner join -- type of join
	   host h      -- code another table with a correlation name
	   on g.id = h.id -- join condition coded in the ON phrase
	   inner join -- type of join
	   casino c     -- code another table a correlation name
       on c.casino_name = h.casino_name -- join condition coded in the ON phrase 
	   
-- Correlation names may be a slong or short as you'd like
select gambler_name, hotel.casino_name, location, room_type -- Columns to be included in the result
  from gambler people   -- code one table on the from with a correation name
       inner join       -- type of join
	   host hotel       -- code another table with a correlation name
	   on people.id = hotel.id -- join condition coded in the ON phrase
	   inner join              -- type of join
	   casino gambling_parlor  -- code another table a correlation name
       on gambling_parlor.casino_name = hotel.casino_name -- join condition coded in the ON phrase  