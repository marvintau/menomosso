for i in {1..1}
do
    # printf "adding new player\n"
    # curl -H "Content-Type: application/json" -X POST -d '' http://everstream.cn:1337/api/add_new_player
    # printf "\ngetting player\n"
    # curl -H "Content-Type: application/json" -X POST -d '{"id":"8673cc53-e2a8-4375-b6a3-007e2ebe6d5f"}' http://everstream.cn:1337/api/get_player
    # printf "\ngetting player list \n"
    # curl -H "Content-Type: application/json" -X POST -d '{}' http://everstream.cn:1337/api/get_player_list
    # printf "\ncheck chest \n"
    # curl -H "Content-Type: application/json" -X POST -d '{"id":"8673cc53-e2a8-4375-b6a3-007e2ebe6d5f"}' http://everstream.cn:1337/api/check_chest
    # printf "\nopen chest \n"
    # curl -H "Content-Type: application/json" -X POST -d '{"id":"8673cc53-e2a8-4375-b6a3-007e2ebe6d5f"}' http://everstream.cn:1337/api/open_chest
    #  printf "\nopen chest \n"
    curl -H "Content-Type: application/json" -X POST\
        -d '{"id1":"8673cc53-e2a8-4375-b6a3-007e2ebe6d5f", "id2":"68b19bbe-bc2a-400f-b4e7-6e632b3b908f", "self_card_id" : "1b0cf5e0-2164-46fd-8424-2146fca99fb9",
        "skills":["increase_dodge", "double_attack", "triple_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack"]}' http://everstream.cn:1337/api/battle_request
done
