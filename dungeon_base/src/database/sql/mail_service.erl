-module(mail_service).

-export([send_mail/5, recv_mail_list/2, read_mail/3, reply_mail/5, delete_mail/3]).

send_mail(Conn, PlayerUUID, ReceiverUUID, Content, Attachment) ->

    Sender = case player:get(Conn, PlayerUUID) of
        #{error:=_} -> not_valid_sender;
        _ -> ok 
    end,

    Receiver = case player:get(Conn, ReceiverUUID) of
        #{error:=_} -> not_valid_receiver;
        _ -> ok
    end,

    case {Sender, Receiver} of
        {ok, ok} ->
            case Attachment of 
                [] ->
                    mail:add(Conn, #{sender_id=>PlayerUUID, receiver_id=>ReceiverUUID, content=>Content}),
                    #{ok=>mail_sent};
                Attachs ->
                    MailID = mail:add(Conn, #{sender_id=>PlayerUUID, receiver_id=>ReceiverUUID, content=>Content, has_items=>true}),
                    [mail_item:add(Conn, MailID, ItemID, ItemQty) || {ItemID, ItemQty} <- Attachs],
                    #{ok=>mail_sent}
            end;
        {OtherS, OtherR} ->
            #{error => #{send=>OtherS, recv=>OtherR}}
    end.


trunc_title(<<Title:20, _>>) -> Title;
trunc_title(Title) -> Title.

trunc_mail(#{content:=Content}=Mail) ->
    Mail#{content:=trunc_title(Content)}.

recv_mail_list(Conn, ReceiverUUID) ->

    ReceivedMails = mail:get(Conn, ReceiverUUID),
    
    ReturnedMailList = [trunc_mail(Mail) || #{is_deleted:=IsDeleted}=Mail <- ReceivedMails, IsDeleted /= <<"t">>],

    ReturnedMailList.

read_mail(Conn, ReceiverID, MailID) ->

    mail:set(Conn, #{is_read=>true}, ReceiverID, MailID),

    Mail = mail:get(Conn, ReceiverID, MailID),
    
    Replies = mail_reply:get(Conn, MailID),
    
    Mail#{replies=>Replies}.

reply_mail(Conn, PlayerUUID, ReceiverUUID, MailID, Content) ->


    Sender = case player:get(Conn, PlayerUUID) of
        #{error:=_} -> not_valid_sender;
        _ -> ok 
    end,

    Receiver = case player:get(Conn, ReceiverUUID) of
        #{error:=_} -> not_valid_receiver;
        _ -> ok
    end,

    case {Sender, Receiver} of
        {ok, ok} ->

            List = mail_reply:get(Conn, MailID),

            mail_reply:add(Conn, #{sender_id=>PlayerUUID, mail_id=>MailID, reply_seq=>length(List), content=>Content});

        {OtherS, OtherR} ->

            {error, #{send=>OtherS, recv=>OtherR}}
    end.

delete_mail(Conn, PlayerID, MailID) ->

    error_logger:info_report({deleting, MailID, by, PlayerID}),

    error_logger:info_report(mail:get(Conn, PlayerID)),

    mail:set(Conn, #{is_deleted=><<"true">>}, PlayerID, MailID),
    #{ok=>deleted}.


fetch_attachment_items(Conn, PlayerID, ItemID, ItemQty) ->
    erlang:display("yay"),
    case ItemID of
        <<"1">> ->
                Res=dungeon_query:update_coin(Conn, {binary_to_integer(ItemQty), PlayerID}),
                erlang:display(Res);
        <<"2">> ->dungeon_query:update_frag(Conn, {ItemQty, <<"946ae77c-183b-4538-b439-ac9036024676">>, PlayerID});
        <<"3">> ->dungeon_query:update_frag(Conn, {ItemQty, <<"1b0cf5e0-2164-46fd-8424-2146fca99fb9">>, PlayerID});
        <<"4">> ->dungeon_query:update_frag(Conn, {ItemQty, <<"a009e5e9-2057-4353-9871-309d68752c1b">>, PlayerID});
        <<"5">> ->dungeon_query:update_frag(Conn, {ItemQty, <<"a0c1a883-2995-4526-856c-26870e5b3f74">>, PlayerID});
        _ ->
                not_implemented_yet
    end.

