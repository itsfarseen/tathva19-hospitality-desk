module Components.Bill exposing (newBill, savedBill)

import Backend exposing (BedAssignment, Bill, NewBill)
import Element exposing (Element, column, row, wrappedRow)
import Theme



-- PUBLIC


newBill : NewBill -> ({ shortId : String } -> msg) -> Element msg
newBill bill deleteMsg =
    column [ Element.padding 20, Element.spacing 20, Element.width Element.fill ]
        [ Theme.title1 ("Hospitality Bill: #" ++ bill.billNo)
        , column [ Element.spacing 10, Element.width Element.fill ]
            (if List.length bill.bedAssignments > 0 then
                [ bedAssignmentsHeader ]
                    ++ List.map (bedAssignment <| Just deleteMsg) bill.bedAssignments

             else
                [ emptyBill ]
            )
        ]


savedBill : Bill -> Element msg
savedBill bill =
    column [ Element.padding 20, Element.spacing 20, Element.width Element.fill ]
        [ Theme.title1 ("Hospitality Bill: #" ++ bill.billNo)
        , Theme.title2
            ("DEREG: "
                ++ (if bill.dereg then
                        "DE-REGED"

                    else
                        "NOT DE-REGED"
                   )
            )
        , column [ Element.spacing 10, Element.width Element.fill ]
            (if List.length bill.bedAssignments > 0 then
                [ bedAssignmentsHeader ]
                    ++ List.map (bedAssignment Nothing) bill.bedAssignments

             else
                [ emptyBill ]
            )
        ]



-- INTERNAL


emptyBill : Element msg
emptyBill =
    row
        (Theme.pageSubCardAttrs
            ++ [ Element.width Element.fill, Element.spaceEvenly ]
        )
        [ Element.el [ Element.centerX ] (Theme.title2 "Bill is empty") ]


bedAssignmentsHeader : Element msg
bedAssignmentsHeader =
    row
        (Theme.pageSubCardAttrs
            ++ [ Element.width Element.fill, Element.spaceEvenly ]
        )
        [ column [ Element.spacing 5, Element.width <| Element.fillPortion 7 ]
            [ Theme.title2 "Participant details" ]
        , Element.el [ Element.width (Element.fillPortion 3) ]
            (Theme.title2 "Mobile")
        , Element.el [ Element.width <| Element.fillPortion 3 ]
            (Theme.title2 "Hostel & Bed")
        , Element.el [ Element.width <| Element.fillPortion 3 ]
            (Theme.title2 " ")
        ]


bedAssignment : Maybe ({ shortId : String } -> msg) -> BedAssignment -> Element msg
bedAssignment maybeDeleteMsg { participant, bedno } =
    row
        (Theme.pageSubCardAttrs
            ++ [ Element.width Element.fill, Element.spaceEvenly ]
        )
        [ column [ Element.spacing 5, Element.width <| Element.fillPortion 7 ]
            [ Theme.title2 participant.shortId
            , Theme.title2 participant.name
            , Theme.title2 participant.college
            ]
        , Element.el [ Element.width <| Element.fillPortion 3 ]
            (Theme.title2 participant.mobile)
        , Element.el [ Element.width <| Element.fillPortion 3 ]
            (Theme.title2 bedno)
        , Element.el [ Element.width <| Element.fillPortion 3 ]
            (case maybeDeleteMsg of
                Just deleteMsg ->
                    Theme.button
                        "Delete"
                        (deleteMsg { shortId = participant.shortId })
                        True

                Nothing ->
                    Element.none
            )
        ]
