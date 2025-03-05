USE [master];
GO

IF DB_ID('Meetups') IS NOT NULL
BEGIN
	ALTER DATABASE [Meetups] SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
	DROP DATABASE [Meetups];
END

CREATE DATABASE [Meetups];
GO

USE [Meetups];
GO

CREATE TABLE [member] (
	member_id INT IDENTITY(1,1) NOT NULL,
	first_name VARCHAR(20) NOT NULL,
	last_name VARCHAR(30) NOT NULL,
	email VARCHAR(50) NOT NULL,
	phone VARCHAR(12),
	birthdate DATE NOT NULL,
	wants_reminders BIT NOT NULL,
	CONSTRAINT [PK_member] PRIMARY KEY (member_id)
);

-- note that group is a reserved word
CREATE TABLE [interest_group] (
	group_id INT IDENTITY(1,1) NOT NULL,
	name VARCHAR(40) NOT NULL,
	CONSTRAINT [PK_interest_group] PRIMARY KEY (group_id),
	CONSTRAINT [UQ_interest_group_name] UNIQUE (name)
);

CREATE TABLE [event] (
	event_id INT IDENTITY(1,1) NOT NULL,
	name VARCHAR(40) NOT NULL,
	description NVARCHAR (MAX),
	start_date DATE NOT NULL,
	start_time TIME NOT NULL,
	duration INT NOT NULL,
	group_id INT NOT NULL,
	CONSTRAINT [PK_event] PRIMARY KEY (event_id),
	CONSTRAINT [FK_event_interest_group] FOREIGN KEY (group_id) REFERENCES interest_group(group_id),
	CONSTRAINT [CHK_duration] CHECK (duration >= 30)
);

CREATE TABLE [interest_group_member] (
	group_id INT NOT NULL,
	member_id INT NOT NULL,
	CONSTRAINT [PK_interest_group_member] PRIMARY KEY (group_id, member_id),
	CONSTRAINT [FK_interest_group_member_interest_group] FOREIGN KEY (group_id) REFERENCES interest_group(group_id),
	CONSTRAINT [FK_interest_group_member_member] FOREIGN KEY (member_id) REFERENCES member(member_id)
);

CREATE TABLE [event_member] (
	event_id INT NOT NULL,
	member_id INT NOT NULL,
	CONSTRAINT [PK_event_member] PRIMARY KEY (event_id, member_id),
	CONSTRAINT [FK_event_member_event] FOREIGN KEY (event_id) REFERENCES event(event_id),
	CONSTRAINT [FK_event_member_member] FOREIGN KEY (member_id) REFERENCES member(member_id)
);


INSERT INTO [interest_group] (name) VALUES ('Java Users Group');
INSERT INTO [interest_group] (name) VALUES ('.NET Users Group');
INSERT INTO [interest_group] (name) VALUES ('Homebrew Website Club');

INSERT INTO [event] (name, start_date, start_time, duration, group_id)
VALUES ('Java Monthly Meetup', '2021-07-01', '19:00', 90, 1);
INSERT INTO [event] (name, start_date, start_time, duration, group_id)
VALUES ('Java Monthly Meetup', '2021-08-01', '19:00', 90, 1);
INSERT INTO [event] (name, start_date, start_time, duration, group_id)
VALUES ('.NET Monthly Meetup', '2021-07-15', '18:30', 120, 2);
INSERT INTO [event] (name, start_date, start_time, duration, group_id)
VALUES ('IndieWebCamp', '2021-12-04', '9:00', 480, 3);

INSERT INTO [member] (first_name, last_name, email, birthdate, wants_reminders)
VALUES ('Andy', 'Smith', 'andy@gmail.com', '1955-05-19', 0);
INSERT INTO [member] (first_name, last_name, email, birthdate, wants_reminders)
VALUES ('Bertha', 'Jones', 'bertha@hotmail.com', '1960-12-02', 1);
INSERT INTO [member] (first_name, last_name, email, birthdate, wants_reminders)
VALUES ('Chuck', 'Johnson', 'chuck@protonmail.com', '1955-06-08', 0);
INSERT INTO [member] (first_name, last_name, email, birthdate, wants_reminders)
VALUES ('Deanna', 'Wilson', 'deanna@outlook.com', '1975-09-19', 1);
INSERT INTO [member] (first_name, last_name, email, birthdate, wants_reminders)
VALUES ('Ernie', 'Fitzsimmons', 'ernie@yahoo.com', '1988-05-20', 0);
INSERT INTO [member] (first_name, last_name, email, birthdate, wants_reminders)
VALUES ('Faye', 'Andrews', 'faye@hey.com', '1995-07-10', 1);
INSERT INTO [member] (first_name, last_name, email, birthdate, wants_reminders)
VALUES ('Greg', 'Jackson', 'greg@mailinator.com', '1987-01-12', 0);
INSERT INTO [member] (first_name, last_name, email, birthdate, wants_reminders)
VALUES ('Henrietta', 'Hemingway', 'henrietta@juno.com', '1945-12-24', 1);

INSERT INTO [event_member] (event_id, member_id) VALUES (1, 1);
INSERT INTO [event_member] (event_id, member_id) VALUES (1, 2);
INSERT INTO [event_member] (event_id, member_id) VALUES (1, 3);
INSERT INTO [event_member] (event_id, member_id) VALUES (2, 4);
INSERT INTO [event_member] (event_id, member_id) VALUES (2, 5);
INSERT INTO [event_member] (event_id, member_id) VALUES (3, 6);
INSERT INTO [event_member] (event_id, member_id) VALUES (3, 7);
INSERT INTO [event_member] (event_id, member_id) VALUES (3, 8);

INSERT INTO [interest_group_member] (group_id, member_id) VALUES (1, 1);
INSERT INTO [interest_group_member] (group_id, member_id) VALUES (1, 2);
INSERT INTO [interest_group_member] (group_id, member_id) VALUES (2, 4);
INSERT INTO [interest_group_member] (group_id, member_id) VALUES (2, 5);
INSERT INTO [interest_group_member] (group_id, member_id) VALUES (3, 6);
INSERT INTO [interest_group_member] (group_id, member_id) VALUES (3, 7);
INSERT INTO [interest_group_member] (group_id, member_id) VALUES (3, 1);
