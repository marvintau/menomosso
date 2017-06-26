-module(mail_service).

-export([send_mail/4, recv_mail_list/2]).

send_mail(Conn, PlayerUUID, ReceiverUUID, Content) ->

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
            mail:add(Conn, #{sender_id=>PlayerUUID, receiver_id=>ReceiverUUID, content=>Content});
        {OtherS, OtherR} ->
            {error, #{send=>OtherS, recv=>OtherR}}
    end.


trunc_title(<<Title:20, _>>) -> Title;
trunc_title(Title) -> Title.

trunc_mail(#{content:=Content}=Mail) ->
    Mail#{content:=trunc_title(Content)}.

recv_mail_list(Conn, ReceiverUUID) ->

    ReceivedMails = mail:get(Conn, ReceiverUUID),
    
    ReturnedMailList = [trunc_mail(Mail) || #{is_deleted:=IsDeleted}=Mail <- ReceivedMails, not IsDeleted],

    ReturnedMailList.

read_mail(Conn, ReceiverID, MailID) ->

    mail:set(Conn, #{is_read=>true}, ReceiverID, MailID),

    Mail = mail:get(Conn, ReceiverID, MailID),
    
    Replies = mail_reply:get(Conn, MailID),
    
    Mail#{reply=>Replies}.

reply_mail(Conn, PlayerUUID) ->

    mail
