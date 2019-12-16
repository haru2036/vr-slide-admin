module Util.Event exposing(..)

firstPageUrl : String -> String
firstPageUrl slideId = case slideId of 
    "" -> ""
    _ -> String.concat ["https://speakerd.s3.amazonaws.com/presentations/",
            slideId,
            "/preview_slide_0",
            ".jpg?373063"] 
