module Character exposing (..)

import Array exposing (Array)


type alias Character =
    { guideImage : String
    , actionImage : String
    , dieIcons : Array.Array String
    }


barbarian : Character
barbarian =
    { guideImage = "barbarian_guide_smaller.jpeg"
    , actionImage = "barbarian_actions_smaller.png"
    , dieIcons =
        Array.fromList
            [ ""
            , "barbarian_sword.png"
            , "barbarian_sword.png"
            , "barbarian_sword.png"
            , "barbarian_heart.png"
            , "barbarian_heart.png"
            , "barbarian_pow.png"
            ]
    }


moonElf : Character
moonElf =
    { guideImage = "moon_elf_guide_smaller.jpeg"
    , actionImage = "moon_elf_actions_smaller.png"
    , dieIcons =
        Array.fromList
            [ ""
            , "moon_elf_arrow.png"
            , "moon_elf_arrow.png"
            , "moon_elf_arrow.png"
            , "moon_elf_foot.png"
            , "moon_elf_foot.png"
            , "moon_elf_moon.png"
            ]
    }
