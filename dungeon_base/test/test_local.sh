for i in {1..1}
do
    # curl -H "Content-Type: application/json" -X POST\
    #     -d '{"id1":"8673cc53-e2a8-4375-b6a3-007e2ebe6d5f", "id2":"68b19bbe-bc2a-400f-b4e7-6e632b3b908f", "self_card_id" : "1b0cf5e0-2164-46fd-8424-2146fca99fb9",
    #     "skills":["rune_of_the_void", "double_attack", "triple_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack"]}' http://localhost:1337/api/battle_request
    curl -H "Content-Type: application/json" -X POST\
        -d '{"id1":"8673cc53-e2a8-4375-b6a3-007e2ebe6d5f", "id2":"68b19bbe-bc2a-400f-b4e7-6e632b3b908f", "self_card_id" : "1b0cf5e0-2164-46fd-8424-2146fca99fb9",
        "skills":["healing_potion", "counterattack", "triple_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack"]}' http://localhost:1337/api/battle_request
done
