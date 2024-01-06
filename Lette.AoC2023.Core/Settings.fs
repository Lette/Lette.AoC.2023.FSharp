namespace Lette.AoC2023

type DaySetting =
    | All
    | Latest
    | Only of int list
    | Except of int list

type PartSetting =
    | Both
    | First
    | Second

type Settings =
    {
        Days: DaySetting
        Parts: PartSetting
        ShowParserTime: bool
    }

module Settings =

    let setOnly day =
        function
        | Only ds -> Only (day :: ds)
        | _       -> Only (day :: [])

    let setExcept day =
        function
        | Except ds -> Except (day :: ds)
        | _         -> Except (day :: [])

    let defaultSettings =
        {
            Days = All
            Parts = Both
            ShowParserTime = false
        }
