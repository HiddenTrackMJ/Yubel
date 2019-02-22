CREATE TABLE public.game_record (
  record_id    SERIAL8 PRIMARY KEY NOT NULL,
  room_id      INT   NOT NULL ,
  start_time   BIGINT NOT NULL,
  end_time     BIGINT NOT NULL,
  file_path    varchar(255) NOT NULL
  );
CREATE INDEX game_record_record_id_idx ON game_record(record_id);

CREATE TABLE public.user_in_record(
  user_id  varchar(255) NOT NULL,
  record_id BIGINT NOT NULL,
  room_id  INT NOT NULL
);
CREATE INDEX user_in_record_record_id_idx ON user_in_record(record_id);

ALTER TABLE public.user_in_record ADD nickname varchar(255) DEFAULT '' NOT NULL;

CREATE TABLE public.player_record (
  id SERIAL8 PRIMARY KEY NOT NULL,
  player_id varchar(255) NOT NULL,
  nickname varchar(255) NOT NULL,
  killing INT   NOT NULL,
  killed INT   NOT NULL,
  score FLOAT NOT NULL,
  start_time   BIGINT NOT NULL,
  end_time     BIGINT NOT NULL
);


create table if not exists users
(
	id bigint default nextval('user_id_seq'::regclass) not null
		constraint users_pkey
			primary key,
	username varchar(255) not null,
	secure_pwd varchar(255) not null,
	create_time bigint not null,
	state integer default 0 not null
)
;

create table if not exists game_record
(
	record_id bigserial not null
		constraint game_record_pkey
			primary key,
	room_id integer not null,
	start_time bigint not null,
	end_time bigint not null,
	file_path varchar(255) not null
)
;

create index if not exists game_record_record_id_idx
	on game_record (record_id)
;

create table if not exists user_in_record
(
	user_id varchar(255) not null,
	record_id bigint not null,
	room_id integer not null,
	nickname varchar(255) default ''::character varying not null
)
;

create index if not exists user_in_record_record_id_idx
	on user_in_record (record_id)
;

create table if not exists player_record
(
	id bigserial not null
		constraint player_record_pkey
			primary key,
	player_id varchar(255) not null,
	nickname varchar(255) not null,
	killing integer not null,
	killed integer not null,
	score double precision not null,
	start_time bigint not null,
	end_time bigint not null
)
;

