CREATE TABLE free_bike_status (
	bike_id text,
	lat float,
	lon float,
	type text,
	is_disabled integer,
	is_reserved integer,
	city text,
	operator text,
	collected_time integer
);

CREATE TABLE station_status (
	station_id text,
	num_bikes_available integer,
	num_ebikes_available integer,
	num_docks_available integer,
	station_status text,
	is_returning integer,
	is_renting integer,
	city text,
	operator text,
	collected_time integer
);