module GlobalMsg exposing (GlobalMsg(..))

import Pages


type GlobalMsg
    = RedirectToPage Pages.Page
    | LogIn { token : String }
    | LogOut
    | EnterPrintMode
    | ExitPrintMode
    | Batch (List GlobalMsg)
