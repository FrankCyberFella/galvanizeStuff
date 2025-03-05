drop database hotelDB
drop table hotel
drop table reservation
Go

create database hotelDB
Go

Create table Hotel 
(id              integer       Primary Key,
 name            nvarchar(40),
 address         nvarchar(100),
 stars           integer,
 roomsAvailable  integer,
 costPerNight    decimal(7,2),
 coverImage      nvarchar(100))

 insert into Hotel
 (id, name, address,stars, roomsAvailable, costPerNight, coverImage)
 Values(1,'Aloft Cleveland','111 W 10th St, Cleveland, Ohio 44114',4, 48, 274, null),
       (2,'Hilton Cleveland Downtown','100 Lakeside Ave, Cleveland, Ohio 44114',4, 12, 287, null),
	   (3,'Metropolitan at the 9','2017 E. 9th St, Cleveland, Ohio 48226',5, 22, 319, null),
	   (4,'The Westin Pittsburgh','1000 Pen Ave, Pittsburgh, Pennsylvania 15222',4, 60, 131, null),
	   (5,'Hilton Columbus Downtown','401 High St, Columbus, Ohio  43215',4, 34, 190, null),
	   (6,'The Summit - A Dolce Hotel','5345 Medpace Way, Cincinnati, Ohio 43215',4, 43, 218, null),
	   (7,'Greektown Detroit','1200 St. Antoine St, Detroit, Michigan 48226',4, 75, 185, null),
	   (8,'Fairmont Princess','7575 E. Princess Drive, E. Scottsdale, Arizona, 85255',5, 750, 650, null)

Create table reservation 
(id           integer  primary key,
 hotelId      integer,
 fullName     nvarchar(30),
 checkinDate  char(10),
 checkoutDate char(10),
 guests       integer
)

insert into reservation
(id, hotelId, fullName, checkinDate, checkoutDate, guests)
values(1, 1, 'John Smith', '07/01/2025', '07/10/2025', 2),
      (2, 1, 'Sam Turner', '12/29/2024', '01/04/2025', 1),
	  (3, 2, 'Mark Johnson', '11/18/2025', '12/01/2025', 3)


 select * from Hotel
 select * from reservation