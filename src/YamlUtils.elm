module YamlUtils exposing (getNameAndNonEmptyValueWithIntendation, getNameAndValueWithDashAndIntendation, getNameAndValueWithIntendation, getNameWithIntendation)


getNameWithIntendation : String -> Int -> String
getNameWithIntendation name intendation =
    String.repeat intendation "  " ++ name ++ ":\n"


getNameAndValueWithIntendation : String -> String -> Int -> String
getNameAndValueWithIntendation name value intendation =
    String.repeat intendation "  " ++ name ++ ": " ++ value ++ "\n"


getNameAndNonEmptyValueWithIntendation : String -> String -> Int -> String
getNameAndNonEmptyValueWithIntendation name value intendation =
    if String.length value > 0 then
        getNameAndValueWithIntendation name value intendation

    else
        ""


getNameAndValueWithDashAndIntendation : String -> String -> Int -> String
getNameAndValueWithDashAndIntendation name value intendation =
    String.repeat (intendation - 1) "  " ++ "- " ++ name ++ ": " ++ value ++ "\n"
