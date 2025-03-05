-- ---------------------------------------------------------------------------
-- This SQL will create all tables and supporting objects required for 
--
-- Frank's SQL training session
--
-- ---------------------------------------------------------------------------
-- THIS SQL ASSUMES A DATABASE CALLED 'vegasdb' already exist
-- ---------------------------------------------------------------------------

--
-- Dependent tables must be dropped before parent tables
--
drop table dealer;
drop table host;
--
-- Parent tables must be dropped after dependent tables
--
drop table gambler;
drop table casino;

-- ---------------------------------------------------------------------------
-- Tell SQL Server we want all processing done in the class database
-- --------------------------------------------------------------------------
use vegasdb;

--
-- Parent tables must be created before dependent tables
--

-- ---------------------------------------------------------------------------
-- Create casino table representing gamblers 
-- ---------------------------------------------------------------------------
create table casino
    (casino_name          char(20)            not null,
     location             char(15)            not null,
     owner                char(20)            not null,
     size                 integer             not null,
     primary key(casino_name));

-- ---------------------------------------------------------------------------
-- Create gambler table representing gamblers 
-- ---------------------------------------------------------------------------
create table gambler
    (id                   smallint            identity,  -- Assigned by database manager
     gambler_name         char(20)            not null,
     address              char(20),
     birth_date           date                not null,
     monthly_salary       decimal(9,2)        not null,
	 primary key(id))
  ;

--
-- Dependent tables must be created after parent tables
--

-- ---------------------------------------------------------------------------
-- Create host table representing gamblers hosted at a specific casino
--
-- Note: parent dependent relationships defined in foreign clauses
--       and cascade option on delete which will delete dependent rows in
--       this table if the matching parent row is deleted from the parent
--
-- This table must be created after all parent tables
-- ---------------------------------------------------------------------------
create table host
    (casino_name          char(20)            not null,
     id                   smallint            not null,
     room_type            char(15)            not null,
     credit_line          decimal(9,2)        not null,
     avg_bet              smallint            not null,
     primary key(casino_name, id),
     foreign key(casino_name) references casino(casino_name)
                              on delete cascade,
     foreign key(id) references gambler(id)
                              on delete cascade);
-- ---------------------------------------------------------------------------
-- Create dealer table representing dealer working at a specific casino
--
-- Note: parent dependent relationship defined in foreign clause
--      and cascase option on delete which will delete dependent rows in
--      this table if the matching parent row is deleted from the parent
--
-- This table must be created after all parent tables
-- ---------------------------------------------------------------------------

create table dealer
    (casino_name          char(20)            not null,
     dealer_name          char(20)            not null,
     game                 char(10)            not null,
     years_exp            smallint            not null,
     primary key(casino_name, dealer_name),
     foreign key(casino_name) references casino(casino_name)
                              on delete cascade);
--
-- Data must be inserted into parent table BEFORE inserting related data into dependent table
--

-- ---------------------------------------------------------------------------
-- Add data to casino table
-- ---------------------------------------------------------------------------
insert into casino
  values('Caesers Palace', 'Las Vegas', 'Julius Caeser', 40000);
insert into casino
  values('Harveys Casino', 'Lake Tahoe', 'Harvey Wallbanger', 28000);
insert into casino
  values('Ballys', 'Reno', 'Bill Bally', 31500);
insert into casino
  values('Trump Plaza', 'atlantic city', 'Donut Trump', 22800);
insert into casino
  values('Binions Horseshoe', 'Las vegas', 'Fred Binyawn', 16900);

-- ---------------------------------------------------------------------------
-- Add data to gambler table
--
-- Since the gambler id is an identity column:
--
--   1. Column list must be coded WITHOUT the gambler id column
--   2. Value is NOT coded for gambler id (it is assigned by database manager)
-------------------------------------------------------------------------------
insert into gambler
  (gambler_name, address, birth_date, monthly_salary)
  values('Neil Bransfield', 'Seattle, WA',
              '1959-03-11', 5000.00);
insert into gambler
  (gambler_name, address, birth_date, monthly_salary)
  values('Phil Donahuge', null,
              '1946-12-29', 3250.25);
insert into gambler
  (gambler_name, address, birth_date, monthly_salary)
  values('Stickman Nelson', 'Cumberland, MD',
              '1955-10-21', 12983.75);
insert into gambler
  (gambler_name, address, birth_date, monthly_salary)
  values('Bettina Chavez',  'Fresno, CA',
              '1964-09-09', 2950.00);
insert into gambler
  (gambler_name, address, birth_date, monthly_salary)
  values('T Judson Smith', 'Los Angeles, CA',
             '1972-05-01', 1398.65);
insert into gambler
  (gambler_name, address, birth_date, monthly_salary)
  values('Dana Imori', null,
              '1938-08-08', 7580.50);
insert into gambler
  (gambler_name, address, birth_date, monthly_salary)
  values('Frank Mint', 'El Paso, TX',
              '1977-06-23', 8200.00);
insert into gambler
  (gambler_name, address, birth_date, monthly_salary)
  values('Al Mostbroke', 'Clayton, MO',
              '1975-01-18', 4505.65);
insert into gambler
  (gambler_name, address, birth_date, monthly_salary)
  values('S.Q. Elle', 'Relational, DA',
              '1995-05-23', 1000000.01);              
insert into gambler
  (gambler_name, address, birth_date, monthly_salary)
  values('Red Squiggledoc', 'Java, Indonesia',
              '1979-10-16', 2000.00);

--
-- Data must be inserted into dependent table AFTER inserting related data into parent table
--


-- --------------------------------------------------------------------------------------------------
-- Add data to dealer table - must be done after data is added to it's parent table, casino
-- --------------------------------------------------------------------------------------------------
insert into dealer
  values('Harveys Casino', 'Patriccio Riodini', 'Craps', 4);
insert into dealer
  values('Binions Horseshoe', 'Fred Stipinski', 'Craps', 8);
insert into dealer
  values('Binions Horseshoe', 'Stickman Nelson', 'Blackjack', 10);
insert into dealer
  values('Trump Plaza', 'Lance Lu', 'Roulette', 5);
insert into dealer
  values('Caesers Palace', 'Arthur Andersen', 'Blackjack', 2);
insert into dealer
  values('Ballys', 'Frances Zielinski', 'Craps', 1);

-- --------------------------------------------------------------------------------------------------------
-- Add data to host table - must be done after data is added to both it's parent tables, casino, gambler
-- 
-- Note use of sub-query to get the gambler id (identity column) assigned by the database manager
--------------------------------------------------------------------------------------------------------
insert into host
  values('Ballys', (select id from gambler where gambler_name = 'T Judson Smith'), 'Deluxe', 5000.00, 75);
insert into host
  values('Ballys', (select id from gambler where gambler_name = 'Dana Imori')    , 'Rack',   5000.00, 65);
insert into host
  values('Binions Horseshoe', (select id from gambler where gambler_name = 'Neil Bransfield'), 'Presidential', 1000.00, 10);
insert into host
  values('Ballys', (select id from gambler where gambler_name = 'Neil Bransfield'), 'dump',   2500.00, 15);
insert into host
  values('Binions Horseshoe', (select id from gambler where gambler_name = 'Al Mostbroke'), 'Penthouse', 2000.00, 25);
insert into host
  values('Harveys Casino', (select id from gambler where gambler_name = 'Frank Mint'), 'Penthouse', 25000.00, 345);
insert into host
  values('Ballys', (select id from gambler where gambler_name = 'Stickman Nelson'), 'Rack',   16.50, 2);
insert into host
  values('Binions Horseshoe', (select id from gambler where gambler_name = 'Stickman Nelson'), 'Rack', 5000.00, 75);
insert into host
  values('Trump Plaza', (select id from gambler where gambler_name = 'Bettina Chavez'), 'Suite', 1000.00, 25);
insert into host
  values('Binions Horseshoe', (select id from gambler where gambler_name = 'T Judson Smith'), 'Presidential', 7500.00, 45);
insert into host
  values('Ballys', (select id from gambler where gambler_name = 'Al Mostbroke'), 'Rack', 1000.00, 20);
insert into host
  values('Trump Plaza', (select id from gambler where gambler_name = 'Dana Imori'), 'Non-Smoking', 100000.00, 600);
insert into host
  values('Binions Horseshoe', (select id from gambler where gambler_name = 'Bettina Chavez'), 'Deluxe', 5000.00, 55);
insert into host
  values('Ballys', (select id from gambler where gambler_name = 'Bettina Chavez'), 'Suite', 2250.00, 20);
insert into host
  values('Trump Plaza', (select id from gambler where gambler_name = 'Al Mostbroke'), 'Non-Smoking', 1000.00, 18);
insert into host
  values('Binions Horseshoe', (select id from gambler where gambler_name = 'Frank Mint'), 'Penthouse',  2000.00, 22);
insert into host
  values('Binions Horseshoe', (select id from gambler where gambler_name = 'Dana Imori'), 'Rack', 10000.00, 80);
insert into host
  values('Ballys', (select id from gambler where gambler_name = 'Frank Mint'), 'Rack', 30000.00, 130);
-- ---------------------------------------------------------------------------

-- Display the contents of the tables created for class work
select * from casino;
select * from dealer;
select * from gambler;
select * from host;


