drop table if exists Movies;

Create Table Movies (
movieId     int           identity primary key,   
title       nvarchar(100) not null,
releaseYear int,
director    nvarchar(200)
)

insert into Movies
(title, releaseYear, director)
values ('Godfather'   , 1972, 'Francis Ford Coppola'),
       ('Godfather II', 1974, 'Francis Ford Coppola'),
	   ('Star Trek: The Wrath of Kahn'      , 1982, 'Nicholas Meyer'),
	   ('Star Wars: The Empire Strikes Back', 1980, 'Irwin Kershner')

select * from movies