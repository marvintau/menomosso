drop table if exists mail cascade;

create table mail(
    mail_id uuid not null, unique(mail_id),
    sender_id text not null,
    receiver_id text not null,
    has_item boolean not null default false,
    repliable boolean not null default false,
    
    is_read boolean not null default false,
    is_replied boolean not null default false,
    is_opened  boolean not null default false,
    is_deleted boolean not null default false,

    content text not null,
    
    send_time timestamp not null default now(),
    item_open_time timestamp not null default now(),
    last_reply_time timestamp not null default now(),
    delete_time timestamp not null default now()
);
