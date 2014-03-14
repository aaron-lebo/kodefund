create table projects(
	id serial primary key,
	user_id integer,
	founder integer,
	name text,
	time timestamp,
	edit_time timestamp,
	description text,
	type text,
	deadline integer,
	languages text,
	licenses text,
	plan text,
	changes text);
	
create table subscriptions(
	id serial primary key,
	project_id integer,
	user_id integer,
	edit boolean,
	comment boolean,
	update boolean,
	update-comment boolean,
	application boolean
	email text);
	
create table updates(
	id serial primary key,
	project_id integer,
	user_id integer,
	time timestamp,
	title text,
	update text);
	
create table comments(
	id serial primary key,
	number integer,
	user_id integer,
	project_id integer,
	update_id integer,
	parent integer,
	time timestamp,
	comment text);
	
create table pledges(
	id serial primary key,
	user_id integer,
	card_id integer,
	project_id integer,
	amount integer);

create table goals(
	id serial primary key,
	project_id integer,
	amount integer,
	deadline timestamp,
	description text);
	
create table applications(
	id serial primary key,
	project_id integer,
	user_id integer,
	time timestamp,
	application text);
	
create table messages(
	id serial primary key,
	user_id integer,
	recipient integer,
	parent integer,
	time timestamp,
	message text);

create table notifications(
	id serial primary key,
	project_id integer,
	user_id integer,
	time timestamp,
	type text,
	title text,
	link text,
	notification text);
	
create table cards(
	id serial primary key,
	user_id integer,
	stripe text,
	details text);

create table users(
	id serial primary key,
	username text,
	password text,
	name text,
	email text,
	confirmation text,
	github text,
	github_display text,
	google text,
	google_display text,
	twitter text,
	twitter_display text,
	facebook text,
	facebook_display text,
	display text,
	about text,
	notify boolean,
	messages timestamp
	notifications timestamp);