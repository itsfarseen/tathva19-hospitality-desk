module Pages.Rules exposing (view)

import Element exposing (column)
import Theme


view =
    column [ Element.spacing 20 ]
        [ Theme.title1 "RULES AND REGULATIONS"
        , Theme.title2 "1. Participants are strictly prohibited from use of liquor, cigarettes and other drugs."
        , Theme.title2 "2. Participants should maintain a decent dress code in the place of accommodation."
        , Theme.title2 "3. Participants are strictly prohibited from loitering around the places of accommodation."
        , Theme.title2 "4. Participants are supposed to maintain good conduct throughout their stay in college."
        , Theme.title2 "5. Any damage, if brought to the notice of the hospitality team will be penalized for all the participants staying in the room."
        , Theme.title2 "6. Participants shouldn't cause damage to any items present in the allotted rooms."
        , Theme.title2 "7. Hospitality team will be no way held responsible for any loss of personal belongings."
        , Theme.title2 "8. There shall not be re-allotment of rooms for any reason."
        , Theme.title2 "9. Participants are refrained from causing any inconvenience to the fellow participants."
        , Theme.title2 "10. Participants are supposed to abide by instructions given by the organizers, security staff and other personnel."
        , Theme.title2 "11. All inmates are supposed to return to their hostels within 15 minutes after the pro-show and other event conclude at night."
        , Theme.title2 "12. Fee receipt and allotment list are a requisite for refund."
        , Theme.title2 "13. The hospitality desk will close at 12:00 pm on Sunday. Inmates will not be refunded after closing the desk."
        , Theme.title2 "14. Anyone violating the above terms will be de-registered immediately without any refund."
        , Theme.title2 "All decisions taken by the hospitality team will be final in regard to the accommodation."
        ]
