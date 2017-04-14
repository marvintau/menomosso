for i in {1..1}
do
    curl -H "Content-Type: application/json" -X POST\
        -d '{"id1":"8673cc53-e2a8-4375-b6a3-007e2ebe6d5f", "id2":"68b19bbe-bc2a-400f-b4e7-6e632b3b908f",
        "skills":["rune_of_the_void", "double_attack", "triple_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack"]}' http://localhost:1337/api/battle_request
    curl -H "Content-Type: application/json" -X POST\
        -d '{"id1":"8673cc53-e2a8-4375-b6a3-007e2ebe6d5f", "id2":"68b19bbe-bc2a-400f-b4e7-6e632b3b908f",
        "skills":["sure_hit", "double_attack", "triple_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack"]}' http://localhost:1337/api/battle_request
done
