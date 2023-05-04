create table person (
       id integer primary key,
       first_name text,
       last_name text,
       age integer
);

create table pet (
       id integer primary key,
       name text,
       breed text,
       age integer,
       dead integer
);

create table person_pet (
       person_id integer,
       pet_id integer
);