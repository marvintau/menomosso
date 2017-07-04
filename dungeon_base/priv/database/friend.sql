drop table if exists friend cascade;

create table friend (
    friend_a text not null,
    friend_b text not null,
    time     timestamp not null default now()
);

drop table if exists enemy cascade;

create table enemy (
    self text not null,
    enemy text not null,
    time  timestamp not null default now()
);

drop table if exists circle cascade;

create table circle (
    circle_id serial,
    sender_id text not null,
    time timestamp not null default now(),
    content text not null,
    is_deleted boolean not null default false
);

drop table if exists circle_reply cascade;

create table circle_reply (
    reply_id serial,
    circle_id int not null,
    sender_id text not null,
    time timestamp not null default now(),
    content text not null,
    is_deleted boolean not null default false
);

drop table if exists circle_up cascade;

create table circle_up (
    circle_up_id serial, 
    circle_id int not null,
    time timestamp not null default now(),
    cancelled boolean not null default false,
    up_by text not null
);

drop table if exists friend_request cascade;

create table friend_request (
    request_id serial,
    sender_id text not null,
    receiver_id text not null,
    time timestamp not null default now(),
    is_accepted boolean not null,
    is_rejected boolean not null,
    accept_time timestamp not null default now(),
    reject_time timestamp not null default now()
)
