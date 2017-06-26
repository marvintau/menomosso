drop table if exists mail_reply cascade;

create table mail_reply(
    reply_id uuid not null default uuid_generate_v4(), unique(reply_id),
    mail_id uuid not null,
    reply_seq int not null,
    reply_time timestamp not null default now(),
    sender_id uuid not null,
    content text not null
);
