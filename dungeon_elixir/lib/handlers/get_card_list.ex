defmodule GetCardList do

    def init(req, opts) do
        {:cowboy_rest, req, opts}
    end

    def allowed_methods(req, opts) do
        {[<<"POST">>, <<"OPTIONS">>], req, opts}
    end

    def content_types_accepted(req, state) do
        :erlang.display(:yay)
        { [{<<"application/json">>, :handle_post}], req, state }
    end

    def options(req, state) do
        req1 = :cowboy_req.set_resp_header(<<"access-control-allow-methods">>, <<"POST, OPTIONS">>, req)
        req2 = :cowboy_req.set_resp_header(<<"access-control-allow-headers">>, <<"content-type, origin, access-control-request-origin">>, req1)
        req3 = :cowboy_req.set_resp_header(<<"access-control-allow-origin">>, <<"*">>, req2)
        {:ok, req3, state}
    end

    def content_types_provided(req, state) do
        { [{<<"application/json">>, :handle_post}], req, state }
    end


    def allow_missing_posts(req, state) do
        {:false, req, state}
    end


    def handle_post(req, state) do

        {:ok, req_body, next_req} = :cowboy_req.body(req)

        _data = :jiffy.decode(req_body)

        # {:ok, list} = dungeon_base_sup.query({get_card_list, {}})

        res = :cowboy_req.set_resp_body(:jiffy.encode(["a", "b", "c"]), next_req)

        res1 = :cowboy_req.set_resp_header(<<"access-control-allow-methods">>, <<"POST, OPTIONS">>, res)
        res2 = :cowboy_req.set_resp_header(<<"access-control-allow-headers">>, <<"content-type, origin, access-control-resuest-origin">>, res1)
        res3 = :cowboy_req.set_resp_header(<<"access-control-allow-origin">>, <<"*">>, res2)

        {:true, res3, state}
    end
end