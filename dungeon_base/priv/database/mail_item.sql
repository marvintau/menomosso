drop table if exists mail_item cascade;

create table mail_item (
    mail_id uuid not null, unique(mail_id),
    item_id uuid not null,
    item_qty int not null
);
