for i in {1..1}
do
    printf "adding new player\n"
    curl -H "Content-Type: application/json" -X POST -d '' http://localhost:1334/api/add_new_player
    printf "\ngetting player\n"
    curl -H "Content-Type: application/json" -X POST -d '{"id":"8673cc53-e2a8-4375-b6a3-007e2ebe6d5f"}' http://localhost:1334/api/get_player
    printf "\n getting player list \n"
    curl -H "Content-Type: application/json" -X POST -d '{}' http://localhost:1334/api/get_player_list
    printf "\nopen chest \n"
    curl -H "Content-Type: application/json" -X POST -d '{"id":"8673cc53-e2a8-4375-b6a3-007e2ebe6d5f"}' http://localhost:1334/api/open_chest
    printf "\nopen chest \n"
    curl -H "Content-Type: application/json" -X POST -d '{"id":"8673cc53-e2a8-4375-b6a3-007e2ebe6d5f"}' http://localhost:1334/api/check_chest
done
